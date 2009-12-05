#include "settings-subscriptions.h"
#include "subscriptions.h"

#include "i18n.h"

void CreateSubscriptionsPage(wxTreebook * parent)
{
  InitGettext();

  char * s;
  int i = 0;
  wxString * str;
  wxPanel * panel = new wxPanel(parent);

  wxBoxSizer * hbox = new wxBoxSizer(wxHORIZONTAL);
  panel->SetSizer(hbox);

  wxListBox * box = new wxListBox(panel, -1);
  hbox->Add(box);

  Subscriptions * subs = new Subscriptions();
  subs->Load();
  
  while ((s = subs->GetNext()))
  {
    str = new wxString(s);
    box->InsertItems(1, str, i);
    i++;
  }

  parent->AddPage(panel, _("Subscriptions"));
  subs->Queue();
}
