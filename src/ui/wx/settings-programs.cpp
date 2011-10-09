#include "settings-programs.h"
#include "settingsentry.h"
#include "widgetids.h"

#include "i18n.h"

void CreateProgramsPage(wxTreebook * parent)
{
  InitGettext();
  wxPanel * panel = new wxPanel(parent);

  wxFlexGridSizer * table = new wxFlexGridSizer(3, 3, 0, 0);
  panel->SetSizer(table);

  wxStaticText * browText = new wxStaticText(panel, -1, _("&Browser"));
  table->Add(browText, 1, wxEXPAND | wxALL, 5);

  SettingsEntry * browEntry = new SettingsEntry(panel, (char *) "for:http");
  table->Add(browEntry, 1, wxEXPAND | wxALL, 5);

  wxButton * browButton = new wxButton(panel, wxID_BROWSE, _("&Browse..."));
  browButton->SetClientData(browEntry);
  table->Add(browButton, 1, wxEXPAND | wxALL, 5);

  wxStaticText * mailText = new wxStaticText(panel, -1, _("&Mail Client"));
  table->Add(mailText, 1, wxEXPAND | wxALL, 5);

  SettingsEntry * mailEntry = new SettingsEntry(panel, (char *) "for:mailto");
  table->Add(mailEntry, 1, wxEXPAND | wxALL, 5);

  wxButton * mailButton = new wxButton(panel, wxID_BROWSE, _("&Browse..."));
  mailButton->SetClientData(mailEntry);
  table->Add(mailButton, 1, wxEXPAND | wxALL, 5);

  parent->AddPage(panel, _("Programs"));
}
