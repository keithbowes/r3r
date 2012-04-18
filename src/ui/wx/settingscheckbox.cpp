#include "libr3r.h"
#include "settingscheckbox.h"
#include "settingslist.h"

SettingsCheckBox::SettingsCheckBox(wxWindow * parent, const wxString & label, char * setting_name) :
  wxCheckBox(parent, -1, label)
{
  bool is_enabled;
  char * name;
  int count;
  unsigned char type;
  void * setting_value;

  libr3r_access_settings(&setting_name, &setting_value, &type, &count, SETTINGS_READ);
  is_enabled = (bool) setting_value;
  SetValue(is_enabled);

  name = (char *) malloc(257);
  strcpy(name, setting_name);

  SettingsList * list = GetSettingsList();

  SettingsListElement * elem = new SettingsListElement();
  elem->SetName(name);
  elem->SetType(type);
  elem->SetValue((void *) setting_value);

  list->Append(elem);
  SetClientData(elem);
}
