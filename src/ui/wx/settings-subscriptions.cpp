#include "settings-subscriptions.h"
#include "subscriptions.h"
#include "widgetids.h"

#include "i18n.h"

void CreateSubscriptionsPage(wxTreebook * parent)
{
  InitGettext();

  char * s;
  int i = 0;
  SubscriptionData * data;
  wxString * str;

  wxPanel * panel = new wxPanel(parent);

  wxBoxSizer * hbox = new wxBoxSizer(wxHORIZONTAL);
  panel->SetSizer(hbox);

  wxListBox * box = new wxListBox(panel, -1);
  data->box = box;
  hbox->Add(box, 2, wxEXPAND | wxALL);

  Subscriptions * subs = new Subscriptions();
  
  while ((s = subs->GetNext()))
  {
    str = new wxString(s);
    box->InsertItems(1, str, i);
    i++;
  }

  wxBoxSizer * vbox = new wxBoxSizer(wxVERTICAL);
  hbox->Add(vbox, 1);

  wxButton * del = new wxButton(panel, wxID_DELETE_SUBSCRIPTION, _("&Delete"));
  vbox->Add(del, 1, wxEXPAND | wxALL, 5);
  del->SetClientData(data);

  wxButton * add = new wxButton(panel, wxID_ADD_SUBSCRIPTION, _("&Add"));
  vbox->Add(add, 1, wxEXPAND | wxALL, 5);

  wxTextCtrl * addEntry = new wxTextCtrl(panel, -1);
  vbox->Add(addEntry, 1, wxALL, 5);
  data->entry = addEntry;
  add->SetClientData(data);

  parent->AddPage(panel, _("Subscriptions"));
}
