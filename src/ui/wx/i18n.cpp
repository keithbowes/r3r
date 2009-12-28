#include "libr3r.h"
#include "wx.h"

#include "i18n.h"

#ifdef USE_SYSTEM_GETTEXT
  #include <locale.h>
#endif

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
  path = wxString("/share/locale");
  prefix = wxString((char *) value);
  localeDir = prefix + path;

  #ifdef USE_SYSTEM_GETTEXT
    setlocale(LC_ALL, "");
    textdomain("r3r_wx");
    bindtextdomain("r3r_wx", localeDir.c_str());
  #else
    wxLocale * locale = new wxLocale();
    locale->Init();
    locale->AddCatalogLookupPathPrefix(localeDir);
    locale->AddCatalog("r3r_wx");
  #endif
}
