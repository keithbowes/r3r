#include "desc.h"
#include "events.h"
#include "feedlist.h"
#include "frame.h"
#include "libr3r.h"
#include "settings-main.h"
#include "settings-subscriptions.h"
#include "settingsentry.h"
#include "subscriptions.h"

#include "i18n.h"

void DescriptionBoxEvents::OnContact(wxCommandEvent & event)
{
  char * name;
  unsigned char count, index, type;
  void * value;
  wxString cl, mclient, recep;

  wxButton * contact = (wxButton *) event.GetEventObject();
  char * whom = (char *) contact->GetClientData();

  index = 0;
  name = (char *) "mail-client";
  libr3r_access_settings(&index, &name, &value, &type, &count, SETTINGS_READ);

  mclient = wxString((char *) value);
  recep = wxString((char *) whom);
  cl = mclient + " " + recep;
  
  wxExecute(cl);
}

void DescriptionBoxEvents::OnSubscribe(wxCommandEvent & event)
{
  char * link;

  wxButton * subscribe = (wxButton *) event.GetEventObject();
  subscribe->Disable();

  ItemInfo * info = (ItemInfo *) subscribe->GetClientData();
  if (info->self != "")
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
  box->SetLabel(info->title);

  wxTextCtrl * memo = (wxTextCtrl *) box->GetClientData();
  memo->SetValue(info->desc);

  wxFrame * win = (wxFrame *) GetFeedList()->GetParent()->GetParent();
  win->SetStatusText(info->link, 0);

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

  if (info->self != "")
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
}

void FrameEvents::OnClose(wxCloseEvent & event)
{
  delete GetDescriptionBox();
  delete GetFeedList();

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
  const char * feed = entry->GetValue();
  ParseFeed((char *) feed);
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
    const char * feed = entry->GetValue();
    ParseFeed((char *) feed);
  }
}

void MenuEvents::OnAbout(wxCommandEvent & WXUNUSED(event))
{
  InitGettext();

  char * user_agent = libr3r_get_user_agent();
  wxMessageBox(user_agent, _("About R3R"), wxOK | wxICON_INFORMATION);
}

void MenuEvents::OnDonate(wxCommandEvent & WXUNUSED(event))
{
  GoBrowser((char *) "http://sourceforge.net/donate/index.php?group_id=90897");
}

void MenuEvents::OnLoadSubscriptions(wxCommandEvent & WXUNUSED(event))
{
  char *s;
  Subscriptions * subs = GetSubscriptionsObject();

  while((s = subs->GetNext()))
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
    ParseFeed((char *) fileName.c_str());
  }
}

void MenuEvents::OnQuit(wxCommandEvent & WXUNUSED(event))
{
  Close(TRUE);
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
  unsigned char count, index, type;
  char * name;
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
  SettingsListElement * tog = entry->GetToggle();

  if (elem)
  {
    const char * text = entry->GetValue();
    entry->SetToolTip(text);

    switch (elem->GetType())
    {
      case TYPE_INTEGER:
        elem->SetValue((void *) atoi(text));
        break;
      case TYPE_STRING:
        char * val = (char *) malloc(257);
        strcpy(val, text);
        elem->SetValue((void *) val);
        break;
    }

    if (tog)
    {
      bool has_changed = true;
      tog->SetValue((void *) has_changed);
    }
  }
}

void SubscriptionsEvents::OnAdd(wxCommandEvent & event)
{
  wxButton * button = (wxButton *) event.GetEventObject();
  SubscriptionData * data = (SubscriptionData *) button->GetClientData();
  wxListBox * box = data->box;
  wxTextCtrl * entry = data->entry;

  if (strlen(entry->GetValue()) > 0)
  {
    wxString * value = new wxString();
    *value = entry->GetValue();
    box->InsertItems(1, value, box->GetCount());

    Subscriptions * subs = GetSubscriptionsObject();
    subs->Add((char *) value->c_str());

    entry->SetFocus();
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
