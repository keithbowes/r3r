#include "settingsentry.h"
#include "widgetids.h"

SettingsEntry::SettingsEntry(wxWindow * parent, char * setting_name) :
  wxTextCtrl(parent, -1)
{
  char * name, * text;
  int count, index;
  unsigned char type;
  void * value;

  index = 0;
  libr3r_access_settings(&index, &setting_name, &value, &type, &count, SETTINGS_READ);

  switch (type)
  {
    case TYPE_INTEGER:
      text = (char *) malloc(257);
      sprintf(text, "%d", (int) value);
      break;
    case TYPE_STRING:
      text = (char *) value;
      break;
  }

  name = (char *) malloc(257);
  strcpy(name, setting_name);

  SetValue(text);
  SetToolTip(text);

  if (TYPE_INTEGER == type)
  {
    free(text);
  }

  SettingsList * list = GetSettingsList();

  SettingsListElement * elem = new SettingsListElement();
  elem->SetName(name);
  elem->SetType(type);
  elem->SetValue(value);

  list->Append(elem);
  SetClientData(elem);
}
