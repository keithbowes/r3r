#include "i18n.h"

#ifdef USE_SYSTEM_GETTEXT
  #include <locale.h>
#else
  #include "wx.h"
#endif

void InitGettext()
{
  #ifdef USE_SYSTEM_GETTEXT
    setlocale(LC_ALL, "");
    textdomain("r3r_wx");
    bindtextdomain("r3r_wx", LOCALEDIR);
  #else
    wxLocale * locale = new wxLocale();
    locale->Init();
    locale->AddCatalogLookupPathPrefix(LOCALEDIR);
    locale->AddCatalog("r3r_wx");
  #endif
}
