#include "settings-programs.h"
#include "settingsentry.h"

#include "i18n.h"

void CreateProgramsPage(wxTreebook * parent)
{
  InitGettext();
  wxPanel * panel = new wxPanel(parent);

  wxFlexGridSizer * table = new wxFlexGridSizer(3, 2, 0, 0);
  panel->SetSizer(table);

  wxStaticText * browText = new wxStaticText(panel, -1, _("&Browser"));
  table->Add(browText, 1, wxEXPAND | wxALL, 5);

  SettingsEntry * browEntry = new SettingsEntry(panel, (char *) "browser");
  table->Add(browEntry, 1, wxEXPAND | wxALL, 5);

  wxStaticText * mailText = new wxStaticText(panel, -1, _("&Mail Client"));
  table->Add(mailText, 1, wxEXPAND | wxALL, 5);

  SettingsEntry * mailEntry = new SettingsEntry(panel, (char *) "mail-client");
  table->Add(mailEntry, 1, wxEXPAND | wxALL, 5);

  wxStaticText * editText = new wxStaticText(panel, -1, _("&Editor"));
  table->Add(editText, 1, wxEXPAND | wxALL, 5);

  SettingsEntry * editEntry = new SettingsEntry(panel, (char *) "editor");
  table->Add(editEntry, 1, wxEXPAND | wxALL, 5);

  parent->AddPage(panel, _("Programs"));
}
