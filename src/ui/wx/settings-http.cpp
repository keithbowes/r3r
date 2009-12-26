#include "settings-http.h"
#include "settingscheckbox.h"
#include "settingsentry.h"

#include "i18n.h"

void CreateHTTPPage(wxTreebook * parent)
{
  InitGettext();
  wxPanel * panel = new wxPanel(parent);

  wxBoxSizer * vbox = new wxBoxSizer(wxVERTICAL);
  panel->SetSizer(vbox);

  wxFlexGridSizer * table = new wxFlexGridSizer(2, 2, 0, 5);
  vbox->Add(table);

  wxStaticText * addrText = new wxStaticText(panel, -1, _("Proxy &Address: "));
  table->Add(addrText, 1, wxEXPAND | wxALL, 5);

  SettingsEntry * addrEntry = new SettingsEntry(panel, (char *) "proxy-address");
  table->Add(addrEntry, 2, wxEXPAND | wxALL, 5);

  wxStaticText * portText = new wxStaticText(panel, -1, _("&Proxy Port: "));
  table->Add(portText, 1, wxEXPAND | wxALL, 5);

  SettingsEntry * portEntry = new SettingsEntry(panel, (char *) "proxy-port");
  table->Add(portEntry, 2, wxEXPAND | wxALL, 5);

  SettingsCheckBox * proxyCheck = new SettingsCheckBox(panel, _("&Enable Proxy"), (char *) "use-proxy");
  vbox->Add(proxyCheck);

  parent->AddPage(panel, _("HTTP"));
}
