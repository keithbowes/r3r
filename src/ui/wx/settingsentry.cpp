#include "settingsentry.h"
#include "widgetids.h"

SettingsEntry::SettingsEntry(wxWindow * parent, char * setting_name, char * toggle) :
  wxTextCtrl(parent, -1)
{
  char * name, * text;
  int count, index;
  unsigned char type;
  void * value;

  m_toggle = NULL;

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

  if (toggle)
  {
    index = 0;
    libr3r_access_settings(&index, &toggle, &value, &type, &count, SETTINGS_READ);

    char * togName = (char *) malloc(257);
    strcpy(togName, toggle);

    SettingsListElement * togElem = new SettingsListElement();
    togElem->SetName(togName);
    togElem->SetValue(value);
    list->Append(togElem);

    SetToggle(togElem);
  }
}

SettingsListElement * SettingsEntry::GetToggle()
{
  return m_toggle;
}

void SettingsEntry::SetToggle(SettingsListElement * toggle)
{
  m_toggle = toggle;
}
