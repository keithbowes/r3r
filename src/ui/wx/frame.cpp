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
	wxPanel * panel = new wxPanel(this, wxID_ANY);
	wxBoxSizer * vbox = new wxBoxSizer(wxVERTICAL);
	panel->SetSizer(vbox);

#if wxUSE_LIBPNG
	wxString path, prefix;
	wxChar sep = wxFileName::GetPathSeparator();
	wxChar * spath = (wxChar *) malloc(sizeof(wxChar) * FILENAME_MAX);
	wxStrcpy(spath, wxString(sep));
	wxStrcat(spath, wxT("share"));
	wxStrcat(spath, wxString(sep));
	wxStrcat(spath, wxT("icons"));
	wxStrcat(spath, wxString(sep));
	wxStrcat(spath, wxT("r3r.png"));
	prefix = wxString(getenv("R3R_INSTALLED_PREFIX"), wxConvUTF8);
	path = wxString(spath, wxConvUTF8);
	free(spath);

	wxInitAllImageHandlers();
	SetIcon(wxIcon(prefix + path, wxBITMAP_TYPE_PNG));
#endif
	
	CreateMenus(this);

	GetFeedList()->CreateView(panel);

	CreateDescriptionBox(panel);
	CreateLocationBar(panel);

	CreateStatusBar(1);

	CreateSettingsDialog(this);
}
