#include "desc.h"
#include "feedlist.h"
#include "frame.h"
#include "location.h"
#include "menu.h"
#include "settings-main.h"

RFrame::RFrame(const wxString & title, const wxPoint & pos, const wxSize & size) :
  wxFrame((wxFrame *) NULL, -1, title, pos, size)
{
  wxPanel * panel = new wxPanel(this, wxID_ANY);
  wxBoxSizer * vbox = new wxBoxSizer(wxVERTICAL);
  panel->SetSizer(vbox);

  SetIcon(wxIcon(ICONFILE));
  
  CreateMenus(this);

  CreateFeedList(panel);
  CreateDescriptionBox(panel);
  CreateLocationBar(panel);

  CreateStatusBar(1);

  CreateSettingsDialog(this);
}
