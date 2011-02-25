#include "desc.h"
#include "feedlist.h"
#include "frame.h"
#include "libr3r.h"
#include "location.h"
#include "menu.h"
#include "settings-main.h"

RFrame::RFrame(const wxString & title, const wxPoint & pos, const wxSize & size) :
  wxFrame((wxFrame *) NULL, -1, title, pos, size)
{
  char * name;
  int count, index;
  unsigned char type;
  void * value;
  index = 0;
  name = (char *) "installed-prefix";
  libr3r_access_settings(&index, &name, &value, &type, &count, SETTINGS_READ);

  wxString path, prefix;
	wxChar sep = wxFileName::GetPathSeparator();
	wxChar * spath = (wxChar *) malloc(sizeof(wxChar) * 256);
	wxStrcpy(spath, wxString(sep));
	wxStrcat(spath, wxT("share"));
	wxStrcat(spath, wxString(sep));
	wxStrcat(spath, wxT("icons"));
	wxStrcat(spath, wxString(sep));
	wxStrcat(spath, wxT("r3r.png"));
  prefix = wxString((char *) value, wxConvUTF8);
  path = wxString(spath, wxConvUTF8);
	free(spath);

  wxPanel * panel = new wxPanel(this, wxID_ANY);
  wxBoxSizer * vbox = new wxBoxSizer(wxVERTICAL);
  panel->SetSizer(vbox);

	wxInitAllImageHandlers();
  SetIcon(wxIcon(prefix + path, wxBITMAP_TYPE_PNG));
  
  CreateMenus(this);

  CreateFeedList(panel);
  CreateDescriptionBox(panel);
  CreateLocationBar(panel);

  CreateStatusBar(1);

  CreateSettingsDialog(this);
}
