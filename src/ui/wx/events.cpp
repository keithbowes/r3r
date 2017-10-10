#include "desc.h"
#include "events.h"
#include "feedlist.h"
#include "frame.h"
#include "libr3r.h"
#include "location.h"
#include "settings-main.h"
#include "settings-subscriptions.h"
#include "settingsentry.h"
#include "subscriptions.h"
#include "widgetids.h"

#include "i18n.h"

void DescriptionBoxEvents::OnContact(wxCommandEvent & event)
{
	char * name;
	int count;
	unsigned char type;
	void * value;
	wxString cl, mclient, recep;

	wxButton * contact = (wxButton *) event.GetEventObject();
	char * whom = (char *) contact->GetClientData();

	name = (char *) "for:mailto";
	libr3r_access_settings(&name, &value, &type, &count, SETTINGS_READ);

	mclient = wxString((char *) value, wxConvUTF8);
	recep = wxString(whom, wxConvUTF8);
	mclient.Replace(wxT("%1"), recep);

	if (wxNOT_FOUND == mclient.Find(recep))
	{
		cl = mclient + wxT(" ") + recep;
	}
	else
	{
		cl = mclient;
	}
	
	wxExecute(cl);
}

void DescriptionBoxEvents::OnEnclosure(wxCommandEvent & event)
{
	enclosure_data * enclosure;
	wxString cl, mp;

	wxButton * enclosure_button = (wxButton *) event.GetEventObject();
	enclosure = (enclosure_data *) (enclosure_button->GetClientData());

	wxMimeTypesManager * mgr = new wxMimeTypesManager();
	wxFileType * ft = mgr->GetFileTypeFromMimeType(wxString(enclosure->type, wxConvUTF8));
	if (!ft)
	{
		return;
	}

	mp = ft->GetOpenCommand(wxString(enclosure->url, wxConvUTF8));
	if (!mp)
	{
		return;
	}

	mp.Replace(wxT("%1"), wxString(enclosure->url, wxConvUTF8));
	if (wxNOT_FOUND == mp.Find(wxString(enclosure->url, wxConvUTF8)))
	{
		cl = mp + wxString(wxT(" ")) + wxString(enclosure->url, wxConvUTF8);
	}
	else
	{
		cl = mp;
	}
	
	wxExecute(cl);
}

void DescriptionBoxEvents::OnSubscribe(wxCommandEvent & event)
{
	char * link;

	wxButton * subscribe = (wxButton *) event.GetEventObject();
	subscribe->Disable();

	ItemInfo * info = (ItemInfo *) subscribe->GetClientData();
	if (info->self && strlen(info->self) > 0)
	{
		link = info->self;
	}
	else
	{
		link = info->link;
	}

	Subscriptions * subs = GetSubscriptionsObject();
	subs->Add(link);
}

void FeedListViewEvents::OnActivate(wxListEvent & event)
{
	ItemInfo * info = (ItemInfo *) event.GetData();
	GoBrowser(info->link);
}

void FeedListViewEvents::OnSelect(wxListEvent & event)
{
	char * link;
	ItemInfo * info = (ItemInfo *) event.GetData();

	wxStaticBox * box = GetDescriptionBox();
	box->SetLabel(wxString(info->title, wxConvUTF8));

	HtmlDescriptionBox * html = (HtmlDescriptionBox *) box->GetClientData();
	html->SetPage(wxString(info->desc, wxConvUTF8));

	wxFrame * win = (wxFrame *) GetFeedList()->GetParent()->GetParent();
	win->SetStatusText(wxString(info->link, wxConvUTF8), 0);

	wxButton * contact = GetContactButton();

	if (strlen(info->contact) > 0)
	{
		contact->SetClientData(info->contact);
		contact->Enable();
	}
	else
	{
		contact->Disable();
	}

	wxButton * subscribe = GetSubscribeButton();
	Subscriptions * subs = GetSubscriptionsObject();

	if (info->self && strlen(info->self) > 0)
	{
		link = info->self;
	}
	else
	{
		link = info->link;
	}

	if (info->isTopLevel && subs->IndexOf(link) == -1)
	{

		subscribe->SetClientData(info);
		subscribe->Enable();
	}
	else
	{
		subscribe->Disable();
	}

	wxButton * enclosure_button = GetEnclosureButton();
	if (strlen(info->enclosure.url) > 0)
	{
		enclosure_button->SetClientData(&(info->enclosure));
		enclosure_button->Enable();
	}
	else
	{
		enclosure_button->Disable();
	}
}

void FrameEvents::OnClose(wxCloseEvent & WXUNUSED(event))
{
	delete GetDescriptionBox();
	delete GetFeedList();
	delete GetSubscriptionsObject();

	HideSettingsDialog();
	Destroy();
}

void FrameEvents::OnSize(wxSizeEvent & event)
{
	GetFeedList()->ResizeColumns(event.GetSize().GetWidth());
	event.Skip();
}

void GoButtonEvents::OnClick(wxCommandEvent & event)
{
	wxButton * button = (wxButton * ) event.GetEventObject();
	wxComboBox * entry = (wxComboBox *) button->GetClientData();
	wxString feed = entry->GetValue();
	AddLocationHistory(feed.mb_str());
	ParseFeed((char *) (const char *) feed.mb_str());
}

void GoFieldEvents::OnKeyDown(wxKeyEvent & event)
{
	if (event.GetKeyCode() != WXK_RETURN)
	{
		event.Skip();
	}
	else
	{
		wxComboBox * entry = (wxComboBox *) event.GetEventObject();
		wxString feed = entry->GetValue();
		AddLocationHistory(feed.mb_str());
		ParseFeed((char *) (const char *) feed.mb_str());
	}
}

void MenuEvents::OnAbout(wxCommandEvent & WXUNUSED(event))
{
	InitGettext();

	char * user_agent = libr3r_get_user_agent();
	wxMessageBox(wxString(user_agent, wxConvUTF8), _("About R3R"), wxOK | wxICON_INFORMATION);
}

void MenuEvents::OnDonate(wxCommandEvent & WXUNUSED(event))
{
	GoBrowser((char *) "http://sourceforge.net/donate/index.php?group_id=90897");
}

void MenuEvents::OnLoadSubscriptions(wxCommandEvent & event)
{
	char *s;
	Subscriptions * subs = GetSubscriptionsObject();
	wxWindow * obj = (wxWindow *) event.GetEventObject();
	wxMenuItem * item;
	
	if (obj->IsKindOf(CLASSINFO(wxMenu)))
	{
		// Linux
		item = ((wxMenu *) obj)->FindItem(wxID_LOAD_SUBSCRIPTIONS);
	}
	else if (obj->IsKindOf(CLASSINFO(wxFrame)))
	{
		// Windows
		wxMenuBar * mb = ((wxFrame *) obj)->GetMenuBar();
		item = mb->FindItem(wxID_LOAD_SUBSCRIPTIONS);
	}

	if (item)
	{
		item->Enable(false);
	}

	while((s = subs->GetNext()) != NULL)
	{
		ParseFeed(s);
	}
}

void MenuEvents::OnOpen(wxCommandEvent & WXUNUSED(event))
{
	wxString filter = _("Atom") + wxString("|*.atom;*.xml|", wxConvUTF8) +
		_("ESF") + wxString("|*.esf|", wxConvUTF8) +
		_("Generic XML") + wxString("|*.xml|", wxConvUTF8) +
		_("RSS 1.0") + wxString("|*.rdf;*.rss;*.xml|", wxConvUTF8) +
		_("RSS 2.0") + wxString("|*.rss;*.xml|", wxConvUTF8) +
		_("RSS 3.0") + wxString("|*.r3;*.rss3", wxConvUTF8);

	wxFileDialog * openFileDialog = new wxFileDialog(this, _("Open a file"), wxEmptyString, wxEmptyString, filter);

	if (openFileDialog->ShowModal() == wxID_OK)
	{
		wxString fileName = openFileDialog->GetPath();
		ParseFeed((char *) (const char *) fileName.mb_str());
	}
}

void MenuEvents::OnQuit(wxCommandEvent & WXUNUSED(event))
{
	Close(true);
}

void MenuEvents::OnRefresh(wxCommandEvent & WXUNUSED(event))
{
	FeedListView * view = GetFeedList();
	view->DeleteAllItems();
	LoadFeeds(get_argc(), get_argv());
}

void MenuEvents::OnSettings(wxCommandEvent & WXUNUSED(event))
{
	HideSettingsDialog();
	ShowSettingsDialog();
}

void SettingsCheckBoxEvents::OnClick(wxCommandEvent & event)
{
	wxCheckBox * box = (wxCheckBox *) event.GetEventObject();;
	bool is_checked = box->GetValue();

	SettingsListElement * elem = (SettingsListElement *) box->GetClientData();
	elem->SetValue((void *) is_checked);
}

void SettingsDialogEvents::OnCancel(wxCommandEvent & WXUNUSED(event))
{
	HideSettingsDialog();
}

void SettingsDialogEvents::OnClose(wxCloseEvent & WXUNUSED(event))
{
	wxDialog * dlg = GetSettingsDialog();
	dlg->Show(false);
}

void SettingsDialogEvents::OnOK(wxCommandEvent & WXUNUSED(event))
{
	char * name;
	int count;
	unsigned char type;
	void * value;

	SettingsList * list = GetSettingsList();
	SettingsList::iterator iter;

	for (iter = list->begin(); iter != list->end(); iter++)
	{
		SettingsListElement * current = *iter;
		
		name = current->GetName();
		value = current->GetValue();
		libr3r_access_settings(&name, &value, &type, &count, SETTINGS_WRITE);
	}

	HideSettingsDialog();
}

void SettingsEntryEvents::OnChange(wxCommandEvent & event)
{
	SettingsEntry * entry = (SettingsEntry *) event.GetEventObject();

	SettingsListElement * elem = (SettingsListElement *) entry->GetClientData();

	if (elem)
	{
		const wxString text = entry->GetValue();
		entry->SetToolTip(text);

		switch (elem->GetType())
		{
			case TYPE_INTEGER:
				elem->SetValue((void *) (long) atoi(text.mb_str()));
				break;
			case TYPE_STRING:
				char * val = strdup(text.mb_str());
				elem->SetValue((void *) val);
				break;
		}
	}
}

void SubscriptionsEvents::OnAdd(wxCommandEvent & event)
{
	wxButton * button = (wxButton *) event.GetEventObject();
	SubscriptionData * data = (SubscriptionData *) button->GetClientData();
	wxListBox * box = data->box;
	wxTextCtrl * entry = data->entry;

	if (strlen((const char *) entry->GetValue().mb_str()) > 0)
	{
		wxString * value = new wxString();
		*value = entry->GetValue();
		box->InsertItems(1, value, box->GetCount());
		box->SetSelection(box->GetCount() - 1);

		Subscriptions * subs = GetSubscriptionsObject();
		subs->Add((char *) (const char *) value->mb_str());

		entry->SetFocus();
	}
}

void SubscriptionsEvents::OnBrowse(wxCommandEvent & event)
{
	wxButton * button = (wxButton *) event.GetEventObject();
	wxTextCtrl * entry = (wxTextCtrl *) button->GetClientData();

	wxFileDialog * openFileDialog = new wxFileDialog(this);
	if (openFileDialog->ShowModal() == wxID_OK)
	{
		wxString fileName = openFileDialog->GetPath();
		entry->SetValue(fileName);
	}
}

void SubscriptionsEvents::OnDelete(wxCommandEvent & event)
{
	wxButton * button = (wxButton *) event.GetEventObject();
	SubscriptionData * data = (SubscriptionData *) button->GetClientData();
	wxListBox * box = data->box;
	
	int sel = box->GetSelection();

	if (sel != wxNOT_FOUND)
	{
		box->Delete(sel);

		char * sub;
		unsigned int count = 0;
		sub = NULL;
		libr3r_access_subscriptions(sel, SUBSCRIPTIONS_DELETE, &sub, &count);

		if (box->GetCount() > 0)
		{
			sel = (sel > 0) ? sel - 1 : 0;
			box->Select(sel);
		}
	}
}

void HtmlBoxEvents::OnHover(wxHtmlCellEvent & event)
{
	wxHtmlCell * theCell = event.GetCell();

	wxHtmlLinkInfo * theLink = theCell->GetLink();
	wxFrame * win = (wxFrame *) GetFeedList()->GetParent()->GetParent();

	if (theLink)
	{
		win->SetStatusText(theLink->GetHref(), 0);
	}
	else
	{
		long selectedIndex = GetFeedList()->GetNextItem(-1, wxLIST_NEXT_ALL, wxLIST_STATE_SELECTED);
		ItemInfo * info = (ItemInfo *) GetFeedList()->GetItemData(selectedIndex);
		win->SetStatusText(wxString(info->link, wxConvUTF8), 0);
	}
}

void HtmlBoxEvents::OnLink(wxHtmlLinkEvent & event)
{
	int count;
	char * name = (char *) "use-external-browser";
	unsigned char type;
	void * value;

	libr3r_access_settings(&name, &value, &type, &count, SETTINGS_READ);
	if (!(bool) value)
	{
		wxHtmlWindow * htmlBox = (wxHtmlWindow *) GetDescriptionBox()->GetClientData();
		htmlBox->LoadPage(event.GetLinkInfo().GetHref());
	}
	else
	{
		GoBrowser(event.GetLinkInfo().GetHref().char_str());
	}
}

#include "eventtables.h"
