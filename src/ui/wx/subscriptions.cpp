#include <string.h>

#include "libr3r.h"
#include "subscriptions.h"

Subscriptions * subs;

Subscriptions::Subscriptions()
{
  m_current = 0;
  subs = this;
}

void Subscriptions::Add(char * sub)
{
  unsigned int count;
  libr3r_access_subscriptions(0, SUBSCRIPTIONS_ADD, &sub, &count);
}

void Subscriptions::Delete(char * sub)
{
  unsigned int count;
  libr3r_access_subscriptions(0, SUBSCRIPTIONS_DELETE, &sub, &count);
}

int Subscriptions::IndexOf(char * sub)
{
  bool is_found = false;
  char * s;
  int i = 0;

  m_current = 0;
  while ((s = GetNext()))
  {
    i++;
    if (sub && strcmp(sub, s) == 0)
    {
      is_found = true;
      break;
    }
  }

  if (is_found)
  {
    return i;
  }
  else
  {
    return -1;
  }
}

#include <stdio.h>

char * Subscriptions::GetNext()
{
  char * ret;
  unsigned int count = 0;
  libr3r_access_subscriptions(m_current, SUBSCRIPTIONS_GET, &ret, &count);

  if (m_current >= count)
  {
    m_current = 0;
    ret = NULL;
  }
  else
  {
    m_current++;
  }

  return ret;
}

Subscriptions * GetSubscriptionsObject()
{
  return subs;
}
