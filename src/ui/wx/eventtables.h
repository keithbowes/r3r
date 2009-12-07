#include "gofield.h"
#include "settingscheckbox.h"
#include "settingsdialog.h"
#include "widgetids.h"
  
BEGIN_EVENT_TABLE(FeedListView, wxListView)
  EVT_LIST_ITEM_ACTIVATED(wxID_ANY, FeedListViewEvents::OnActivate)
  EVT_LIST_ITEM_SELECTED(wxID_ANY, FeedListViewEvents::OnSelect)
END_EVENT_TABLE()
  
BEGIN_EVENT_TABLE(RFrame, wxFrame)
  EVT_BUTTON(wxID_CONTACT_BUTTON, DescriptionBoxEvents::OnContact)
  EVT_BUTTON(wxID_SUBSCRIBE_BUTTON, DescriptionBoxEvents::OnSubscribe)
  EVT_BUTTON(wxID_GO_BUTTON, GoButtonEvents::OnClick)
  EVT_CLOSE(FrameEvents::OnClose)
  EVT_MENU(wxID_ABOUT, MenuEvents::OnAbout)
  EVT_MENU(wxID_DONATE, MenuEvents::OnDonate)
  EVT_MENU(wxID_EXIT, MenuEvents::OnQuit)
  EVT_MENU(wxID_LOAD_SUBSCRIPTIONS, MenuEvents::OnLoadSubscriptions)
  EVT_MENU(wxID_OPEN, MenuEvents::OnOpen)
  EVT_MENU(wxID_SETTINGS, MenuEvents::OnSettings)
  EVT_SIZE(FrameEvents::OnSize)
END_EVENT_TABLE()

BEGIN_EVENT_TABLE(GoField, wxTextCtrl)
  EVT_KEY_DOWN(GoFieldEvents::OnKeyDown)
END_EVENT_TABLE()

BEGIN_EVENT_TABLE(SettingsCheckBox, wxCheckBox)
  EVT_CHECKBOX(wxID_ANY, SettingsCheckBoxEvents::OnClick)
END_EVENT_TABLE()

BEGIN_EVENT_TABLE(SettingsDialog, wxDialog)
  EVT_BUTTON(wxID_ADD_SUBSCRIPTION, SubscriptionsEvents::OnAdd)
  EVT_BUTTON(wxID_CANCEL, SettingsDialogEvents::OnCancel)
  EVT_BUTTON(wxID_DELETE_SUBSCRIPTION, SubscriptionsEvents::OnDelete)
  EVT_BUTTON(wxID_OK, SettingsDialogEvents::OnOK)
END_EVENT_TABLE()

BEGIN_EVENT_TABLE(SettingsEntry, wxTextCtrl)
  EVT_TEXT(wxID_ANY, SettingsEntryEvents::OnChange)
END_EVENT_TABLE()
