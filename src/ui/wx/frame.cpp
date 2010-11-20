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
  prefix = wxString((char *) value, wxConvUTF8);
  path = wxString(wxT("/share/icons/r3r.png"));

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
