#include "libr3r.h"
#include "settingslist.h"
#include "subscriptions.h"
#include <string.h>

#define ALLOC_CHARS 100000

char *tmp;
Subscriptions * subs;

#ifdef WIN32
// Windows doesn't have the reentrant strtok_r, so:
#define strtok_r(str, delim, saved) strtok(str, delim)
#endif

Subscriptions::Subscriptions()
{
  m_first_get = true;
  m_internal = (char *) malloc(ALLOC_CHARS);
  subs = this;
}

Subscriptions::~Subscriptions()
{
  free(m_internal);
}

void Subscriptions::Load()
{
  char * name;
  unsigned char count, index, type;
  void * value;

  index = 0;
  name = (char *) "subscriptions";
  libr3r_access_settings(&index, &name, &value, &type, &count, SETTINGS_READ);

  strcpy(m_internal, (char *) value);
}

void Subscriptions::Queue()
{
  SettingsList * list = GetSettingsList();

  SettingsListElement * elem = new SettingsListElement();
  elem->SetName((char *) "subscriptions");
  elem->SetValue((void *) m_internal);

  list->Append(elem);
}

void Subscriptions::Add(char * sub)
{
  strcat(m_internal, sub);
  strcat(m_internal, SEPARATOR_CHAR);
}

void Subscriptions::Delete(char * sub)
{
  char * s, * str;

  while ((s = GetNext()))
  {
    if (strcmp(sub, s) != 0)
    {
      strcpy(str, s);
    }
  }

  m_internal = str;
}

int Subscriptions::IndexOf(char * sub)
{
  bool is_found = false;
  char * s;
  int i = 0;

  m_first_get = true;
  while ((s = GetNext()))
  {
    i++;

    if (strcmp(sub, s) == 0)
    {
      is_found = true;
      break;
    }
  }
  m_first_get = true;

  if (is_found)
  {
    return i;
  }
  else
  {
    return -1;
  }
}

char * Subscriptions::GetNext()
{
  char * str;

  if (m_first_get)
  {
    tmp = (char *) malloc(ALLOC_CHARS);
    strcpy(tmp, m_internal);

    str = strtok_r(tmp, SEPARATOR_CHAR, &m_saved);
    m_first_get = false;

  }
  else
  {
    str = strtok_r(NULL, SEPARATOR_CHAR, &m_saved);
  }

  if (!str)
  {
    free(tmp);
    m_first_get = true;
  }

  return str;
}

char * Subscriptions::GetAll()
{
  return m_internal;
}

Subscriptions * GetSubscriptionsObject()
{
  return subs;
}
