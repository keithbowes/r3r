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

#if wxCHECK_VERSION(2,9,0)
  wxStaticText * typeText = new wxStaticText(panel, -1, _("&Types: "));
	typeText->SetToolTip(_("Custom Accept Types"));
#else
  wxStaticText * typeText = new wxStaticText(panel, -1, _("Custom Accept Types: "));
#endif
  table->Add(typeText, 1, wxEXPAND | wxALL, 5);

  SettingsEntry * typeEntry = new SettingsEntry(panel, (char *) "accept-types");
  table->Add(typeEntry, 1, wxEXPAND | wxALL, 5);

  SettingsCheckBox * typeCheck = new SettingsCheckBox(panel, (wxChar *) NULL, (char *) "use-custom-accept-types");
  typeCheck->SetToolTip(_("Enable"));
  table->Add(typeCheck, 1, wxEXPAND | wxALL, 5);

#if wxCHECK_VERSION(2,9,0)
  wxStaticText * langText = new wxStaticText(panel, -1, _("&Languages: "));
	langText->SetToolTip(_("Custom Accept Languages"));
#else
  wxStaticText * langText = new wxStaticText(panel, -1, _("Custom Accept Languages: "));
#endif
  table->Add(langText, 1, wxEXPAND | wxALL, 5);

  SettingsEntry * langEntry = new SettingsEntry(panel, (char *) "accept-langs");
  table->Add(langEntry, 1, wxEXPAND | wxALL, 5);

  SettingsCheckBox * langCheck = new SettingsCheckBox(panel, (wxChar *) NULL, (char *) "use-custom-accept-langs");
  langCheck->SetToolTip(_("Enable"));
  table->Add(langCheck, 1, wxEXPAND | wxALL, 5);

  parent->AddPage(panel, _("HTTP Headers"));
}
