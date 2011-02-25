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

	wxChar sep = wxFileName::GetPathSeparator();
	wxChar * spath = (wxChar *) malloc(sizeof(wxChar) * 15);
	wxStrcpy(spath, wxString(sep));
	wxStrcat(spath, wxT("share"));
	wxStrcat(spath, wxString(sep));
	wxStrcat(spath, wxT("locale"));
	path = wxString(spath, wxConvUTF8);
	prefix = wxString((const char *) value, wxConvUTF8);
	free(spath);

  localeDir = prefix + path;

  wxLocale * locale = new wxLocale();
#if wxCHECK_VERSION(2,9,0)
	int language;
	if (locale->GetSystemLanguage() != wxLANGUAGE_UNKNOWN)
		language = wxLANGUAGE_DEFAULT;
	else
		language = wxLANGUAGE_ENGLISH;
  locale->Init(language, wxLOCALE_LOAD_DEFAULT);
#else
	locale->Init();
#endif
  locale->AddCatalogLookupPathPrefix(localeDir);
  locale->AddCatalog(wxT("r3r_wx"));

  if (!i18n_inited && !locale->IsOk())
  {
    SendOwnMessage(0, (char *) "Locale support is not available in your computer's current configuration.", NULL);
  }

  i18n_inited = true;
}
