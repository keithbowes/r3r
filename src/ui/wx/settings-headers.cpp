#include "settings-headers.h"
#include "settingsentry.h"

#include "i18n.h"

void CreateHeadersPage(wxTreebook * parent)
{
  InitGettext();
  wxPanel * panel = new wxPanel(parent);

  wxFlexGridSizer * table = new wxFlexGridSizer(2, 2, 0, 0);
  panel->SetSizer(table);

  wxStaticText * typeText = new wxStaticText(panel, -1, _("Custom Accept &Types: "));
  table->Add(typeText, 1, wxEXPAND | wxALL, 5);

  SettingsEntry * typeEntry = new SettingsEntry(panel, (char *) "accept-types", (char *) "use-custom-accept-types");
  table->Add(typeEntry, 1, wxEXPAND | wxALL, 5);

  wxStaticText * langText = new wxStaticText(panel, -1, _("Custom Accept &Languages: "));
  table->Add(langText, 1, wxEXPAND | wxALL, 5);

  SettingsEntry * langEntry = new SettingsEntry(panel, (char *) "accept-langs", (char *) "use-custom-accept-langs");
  table->Add(langEntry, 1, wxEXPAND | wxALL, 5);

  parent->AddPage(panel, _("HTTP Headers"));
}
