#include "settings-http.h"
#include "settingsentry.h"

#include "i18n.h"

void CreateHTTPPage(wxTreebook * parent)
{
  InitGettext();
  wxPanel * panel = new wxPanel(parent);

  wxFlexGridSizer * table = new wxFlexGridSizer(2, 2, 0, 5);
  panel->SetSizer(table);

  wxStaticText * addrText = new wxStaticText(panel, -1, _("Proxy &Address: "));
  table->Add(addrText, 1, wxEXPAND | wxALL, 5);

  SettingsEntry * addrEntry = new SettingsEntry(panel, (char *) "proxy-address", (char *) "use-proxy");
  table->Add(addrEntry, 2, wxEXPAND | wxALL, 5);

  wxStaticText * portText = new wxStaticText(panel, -1, _("&Proxy Port: "));
  table->Add(portText, 1, wxEXPAND | wxALL, 5);

  SettingsEntry * portEntry = new SettingsEntry(panel, (char *) "proxy-port", (char *) "use-proxy");
  table->Add(portEntry, 2, wxEXPAND | wxALL, 5);

  parent->AddPage(panel, _("HTTP"));
}
