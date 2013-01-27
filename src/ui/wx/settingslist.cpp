#include "settingslist.h"

SettingsList * setList;
SettingsListElement setElem;

char * SettingsListElement::GetName()
{
  return m_name;
}

void SettingsListElement::SetName(char * name)
{
  m_name = strdup(name);
}

unsigned char SettingsListElement::GetType()
{
  return m_type;
}

void SettingsListElement::SetType(unsigned char type)
{
  m_type = type;
}

void * SettingsListElement::GetValue()
{
  return m_value;
}

void SettingsListElement::SetValue(void * value)
{
  m_value = value;
}

SettingsList * GetSettingsList()
{
  if (!setList)
  {
    setList = new SettingsList();
  }

  return setList;
}

WX_DEFINE_LIST(SettingsList);
