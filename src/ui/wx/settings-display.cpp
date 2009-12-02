#include "settings-display.h"
#include "settings-main.h"
#include "settingscheckbox.h"

void CreateDisplayPage(wxTreebook * parent)
{
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

  parent->AddPage(panel, _("Display"));
}
