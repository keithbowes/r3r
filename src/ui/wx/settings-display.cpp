#include "settings-display.h"
#include "settings-main.h"
#include "settingscheckbox.h"
#include "settingsentry.h"

#include "i18n.h"

void CreateDisplayPage(wxTreebook * parent)
{
	InitGettext();

	wxPanel * panel = new wxPanel(parent);

	wxBoxSizer * vbox = new wxBoxSizer(wxVERTICAL);
	panel->SetSizer(vbox);

	SettingsCheckBox * warnCheck = new SettingsCheckBox(panel, _("Show &messages from the back-end"), (char *) "show-messages");
	vbox->Add(warnCheck, 1);

	SettingsCheckBox * titleCheck = new SettingsCheckBox(panel, _("Show &feed titles only"), (char *) "display-feed-title-only");
	vbox->Add(titleCheck, 1);

	SettingsCheckBox * hideCheck = new SettingsCheckBox(panel, _("Hide &cached feeds"), (char *) "hide-cached-feeds");
	vbox->Add(hideCheck, 1);

	SettingsCheckBox * hideItemCheck = new SettingsCheckBox(panel, _("&Hide cached feed items"), (char *) "hide-cached-feed-items");
	vbox->Add(hideItemCheck, 1);

	SettingsCheckBox * guessCheck = new SettingsCheckBox(panel, _("&Guess the file types of feeds"), (char *) "enable-mime-guess");
	vbox->Add(guessCheck, 1);

	SettingsCheckBox * updateCheck = new SettingsCheckBox(panel, _("&Notify of updates"), (char *) "check-for-updates");
	vbox->Add(updateCheck, 1);

	SettingsCheckBox * subCheck = new SettingsCheckBox(panel, _("&Load subscriptions on startup"), (char *) "load-subscriptions-on-startup");
	vbox->Add(subCheck, 1);

	wxBoxSizer * encBox = new wxBoxSizer(wxHORIZONTAL);
	vbox->Add(encBox, 1, wxEXPAND | wxALL, 5);

	parent->AddPage(panel, _("Display"));
}
