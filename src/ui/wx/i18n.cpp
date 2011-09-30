#include "libr3r.h"
#include "wx.h"

#include "i18n.h"

void InitGettext()
{
#ifdef USE_NLS
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
	int language = wxLANGUAGE_DEFAULT;
	
	/* Kludge for Windows */
#ifdef __WIN32__
	language = locale->GetSystemLanguage();
#endif

	locale->Init(language);
	locale->AddCatalogLookupPathPrefix(localeDir);
	locale->AddCatalog(wxT("r3r_wx"));
#endif
}
