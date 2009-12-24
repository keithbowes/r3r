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
  name = (char *) "installed-path";
  libr3r_access_settings(&index, &name, &value, &type, &count, SETTINGS_READ);

  wxString prefix = wxString((char *) value);
  wxString path = wxString("/share/icon/r3r.png");

  wxPanel * panel = new wxPanel(this, wxID_ANY);
  wxBoxSizer * vbox = new wxBoxSizer(wxVERTICAL);
  panel->SetSizer(vbox);

  SetIcon(wxIcon(prefix + path));
  
  CreateMenus(this);

  CreateFeedList(panel);
  CreateDescriptionBox(panel);
  CreateLocationBar(panel);

  CreateStatusBar(1);

  CreateSettingsDialog(this);
}
