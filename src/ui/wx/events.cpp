#include "desc.h"
#include "events.h"
#include "feedlist.h"
#include "frame.h"
#include "libr3r.h"
#include "settings-main.h"
#include "settings-subscriptions.h"
#include "settingsentry.h"
#include "subscriptions.h"
#include "widgetids.h"

#include "i18n.h"

void DescriptionBoxEvents::OnContact(wxCommandEvent & event)
{
  char * name;
  int count, index;
  unsigned char type;
  void * value;
  wxString cl, mclient, recep;

  wxButton * contact = (wxButton *) event.GetEventObject();
  char * whom = (char *) contact->GetClientData();

  index = 0;
  name = (char *) "for:mailto";
  libr3r_access_settings(&index, &name, &value, &type, &count, SETTINGS_READ);

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

void DescriptionBoxEvents::OnPodcast(wxCommandEvent & event)
{
  char * name;
  int count, index;
  unsigned char type;
  void * value;
  wxString cl, enclosure, mp;

  wxButton * podcast = (wxButton *) event.GetEventObject();
  enclosure = wxString((wxChar *) podcast->GetClientData());

  index = 0;
  name = (char *) "for:.ogg";
  libr3r_access_settings(&index, &name, &value, &type, &count, SETTINGS_READ);

  mp = wxString((wxChar *) value);
  mp.Replace(wxT("%1"), enclosure);

  if (wxNOT_FOUND == mp.Find(enclosure))
  {
    cl = mp + wxString(wxT(" ")) + enclosure;
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

  wxHtmlWindow * html = (wxHtmlWindow *) box->GetClientData();
  html->SetPage(wxString(info->desc));

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

  wxButton * podcast = GetPodcastButton();
  if (strlen(info->podcast) > 0)
  {
    podcast->SetClientData(info->podcast);
    podcast->Enable();
  }
  else
  {
    podcast->Disable();
  }
}

void FrameEvents::OnClose(wxCloseEvent & event)
{
  delete GetDescriptionBox();
  delete GetFeedList();
  delete GetSubscriptionsObject();

  HideSettingsDialog();
  Destroy();
}

void FrameEvents::OnSize(wxSizeEvent & event)
{
  GetFeedList()->ResizeColumns();
  event.Skip();
}

void GoButtonEvents::OnClick(wxCommandEvent & event)
{
  wxButton * button = (wxButton * ) event.GetEventObject();
  wxTextCtrl * entry = (wxTextCtrl *) button->GetClientData();
  wxString feed = entry->GetValue();
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
    wxTextCtrl * entry = (wxTextCtrl *) event.GetEventObject();
    wxString feed = entry->GetValue();
    ParseFeed((char *) (const char *) feed.mb_str());
  }
}

void MenuEvents::OnAbout(wxCommandEvent & WXUNUSED(event))
{
  InitGettext();

  char * user_agent = libr3r_get_user_agent();
  wxMessageBox(wxString(user_agent, wxConvUTF8), _("About R3R"), wxOK | wxICON_INFORMATION);
}

void MenuEvents::OnCheckUpdates(wxCommandEvent & WXUNUSED(event))
{
  wxString command, url, version;
  url = wxString("http://r3r.sourceforge.net/check.php?v=", wxConvUTF8);
  version = wxString(wxString(VERSION, wxConvUTF8));
  command = url + version + wxString("&display=1", wxConvUTF8);

  GoBrowser((char *) (const char *) command.mb_str());
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
    item->Enable(FALSE);
  }

  while((s = subs->GetNext()) != NULL)
  {
    ParseFeed(s);
  }
}

void MenuEvents::OnOpen(wxCommandEvent & event)
{
  wxFileDialog * openFileDialog = new wxFileDialog(this);

  if (openFileDialog->ShowModal() == wxID_OK)
  {
    wxString fileName = openFileDialog->GetPath();
    ParseFeed((char *) (const char *) fileName.mb_str());
  }
}

void MenuEvents::OnQuit(wxCommandEvent & WXUNUSED(event))
{
  Close(TRUE);
}

void MenuEvents::OnRefresh(wxCommandEvent & WXUNUSED(event))
{
  FeedListView * view = GetFeedList();
  view->DeleteAllItems();
  GetAllFeeds(0, NULL);
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

void SettingsDialogEvents::OnCancel(wxCommandEvent & event)
{
  HideSettingsDialog();
}

void SettingsDialogEvents::OnOK(wxCommandEvent & event)
{
  char * name;
  int count, index;
  unsigned char type;
  void * value;

  SettingsList * list = GetSettingsList();
  SettingsList::iterator iter;

  for (iter = list->begin(); iter != list->end(); iter++)
  {
    SettingsListElement * current = *iter;
    
    index = 0;
    name = current->GetName();
    value = current->GetValue();
    libr3r_access_settings(&index, &name, &value, &type, &count, SETTINGS_WRITE);
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
        elem->SetValue((void *) atoi(text.mb_str()));
        break;
      case TYPE_STRING:
        char * val = (char *) malloc(257);
        strcpy(val, text.mb_str());
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
      box->Select(sel);
    }
  }
}

#include "eventtables.h"
