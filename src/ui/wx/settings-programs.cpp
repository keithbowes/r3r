#include "settings-programs.h"
#include "settingscheckbox.h"
#include "settingsentry.h"
#include "widgetids.h"

#include "i18n.h"

void CreateProgramsPage(wxTreebook * parent)
{
	InitGettext();
	wxPanel * panel = new wxPanel(parent);

	wxBoxSizer * vbox = new wxBoxSizer(wxVERTICAL);
	panel->SetSizer(vbox);

	wxFlexGridSizer * browTable = new wxFlexGridSizer(1, 3, 0, 0);
	vbox->Add(browTable);

	wxStaticText * browText = new wxStaticText(panel, -1, _("&Browser"));
	browTable->Add(browText, 1, wxEXPAND | wxALL, 5);

	SettingsEntry * browEntry = new SettingsEntry(panel, (char *) "for:http");
	browTable->Add(browEntry, 1, wxEXPAND | wxALL, 5);

	wxButton * browButton = new wxButton(panel, wxID_BROWSE, _("&Browse..."));
	browButton->SetClientData(browEntry);
	browTable->Add(browButton, 1, wxEXPAND | wxALL, 5);

	wxBoxSizer * extbrowBox = new wxBoxSizer(wxHORIZONTAL);
	vbox->Add(extbrowBox);

	libr3r_register_setting((char *) "use-external-browser", (char *) "Programs", (void *) false, TYPE_BOOLEAN, (char *) (const char *) wxString(_("Use an external browser for opening links in the description box")).mb_str());
	SettingsCheckBox *extbrowCheck = new SettingsCheckBox(panel, _("&Use an external browser"), (char *) "use-external-browser");
	extbrowCheck->SetToolTip(_("Use an external browser for opening links in the description box"));
	extbrowBox->Add(extbrowCheck, 1, wxEXPAND | wxALL, 5);

	wxFlexGridSizer * mailTable = new wxFlexGridSizer(1, 3, 0, 0);
	vbox->Add(mailTable);

	wxStaticText * mailText = new wxStaticText(panel, -1, _("&Mail Client"));
	mailTable->Add(mailText, 1, wxEXPAND | wxALL, 5);

	SettingsEntry * mailEntry = new SettingsEntry(panel, (char *) "for:mailto");
	mailTable->Add(mailEntry, 1, wxEXPAND | wxALL, 5);

	wxButton * mailButton = new wxButton(panel, wxID_BROWSE, _("&Browse..."));
	mailButton->SetClientData(mailEntry);
	mailTable->Add(mailButton, 1, wxEXPAND | wxALL, 5);

	parent->AddPage(panel, _("Programs"));
}
