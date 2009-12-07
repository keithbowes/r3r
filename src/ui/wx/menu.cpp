#include "events.h"
#include "frame.h"
#include "menu.h"
#include "widgetids.h"

#include "i18n.h"

void CreateMenus(wxFrame * parent)
{
  InitGettext();

  /* The File Menu */
  wxMenu * menuFile = new wxMenu;
  menuFile->Append(wxID_OPEN, _("&Open..."));
  menuFile->AppendSeparator();
  menuFile->Append(wxID_EXIT, _("E&xit"));

  /* The Tools Menu */
  wxMenu * menuTools = new wxMenu;
  menuTools->Append(wxID_LOAD_SUBSCRIPTIONS, _("&Load Subscriptions"));
  menuTools->AppendSeparator();
  menuTools->Append(wxID_SETTINGS, _("&Settings..."));

  /* The Help Menu */
  wxMenu * menuHelp = new wxMenu;
  menuHelp->Append(wxID_DONATE, _("&Donate"));
  menuHelp->AppendSeparator();
  menuHelp->Append(wxID_ABOUT, _("&About"));

  /* Attach menus to the menu bar */
  wxMenuBar * menuBar = new wxMenuBar;
  menuBar->Append(menuFile, _("&File"));
  menuBar->Append(menuTools, _("&Tools"));
  menuBar->Append(menuHelp, _("&Help"));

  /* Set the main menu bar */
  parent->SetMenuBar(menuBar);
}
