#include "feedlist.h"
#include "libr3r.h"
#include "wx.h"

#include "i18n.h"

bool i18n_inited = false;

void InitGettext()
{
  int count, index;
  char * name;
  unsigned char type;
  void * value;

  index = 0;
  name = (char *) "installed-prefix";
  libr3r_access_settings(&index, &name, &value, &type, &count, SETTINGS_READ);

  wxString localeDir, path, prefix;
  path = wxString(wxT("/share/locale"));
  prefix = wxString((wxChar *) value);
  localeDir = prefix + path;

  wxLocale * locale = new wxLocale();
  locale->Init();
  locale->AddCatalogLookupPathPrefix(localeDir);
  locale->AddCatalog(wxT("r3r_wx"));

  if (!i18n_inited && !locale->IsOk())
  {
    SendOwnMessage(0, (char *) "Locale support is not available in your computer's current configuration.", NULL);
  }

  i18n_inited = true;
}
