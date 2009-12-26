#include "settings-headers.h"
#include "settingscheckbox.h"
#include "settingsentry.h"

#include "i18n.h"

void CreateHeadersPage(wxTreebook * parent)
{
  InitGettext();
  wxPanel * panel = new wxPanel(parent);

  wxFlexGridSizer * table = new wxFlexGridSizer(2, 3, 0, 0);
  panel->SetSizer(table);

  wxStaticText * typeText = new wxStaticText(panel, -1, _("Custom Accept &Types: "));
  table->Add(typeText, 1, wxEXPAND | wxALL, 5);

  SettingsEntry * typeEntry = new SettingsEntry(panel, (char *) "accept-types");
  table->Add(typeEntry, 1, wxEXPAND | wxALL, 5);

  SettingsCheckBox * typeCheck = new SettingsCheckBox(panel, (char *) NULL, (char *) "use-custom-accept-types");
  typeCheck->SetToolTip(_("Enable"));
  table->Add(typeCheck, 1, wxEXPAND | wxALL, 5);

  wxStaticText * langText = new wxStaticText(panel, -1, _("Custom Accept &Languages: "));
  table->Add(langText, 1, wxEXPAND | wxALL, 5);

  SettingsEntry * langEntry = new SettingsEntry(panel, (char *) "accept-langs");
  table->Add(langEntry, 1, wxEXPAND | wxALL, 5);

  SettingsCheckBox * langCheck = new SettingsCheckBox(panel, (char *) NULL, (char *) "use-custom-accept-langs");
  langCheck->SetToolTip(_("Enable"));
  table->Add(langCheck, 1, wxEXPAND | wxALL, 5);

  parent->AddPage(panel, _("HTTP Headers"));
}
