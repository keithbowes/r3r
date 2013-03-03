#include "events.h"
#include "frame.h"
#include "libr3r.h"
#include "menu.h"
#include "widgetids.h"

#include "i18n.h"


void CreateMenus(wxFrame * parent)
{
	int count;
	char * name;
	unsigned char type;
	void * value;
	wxMenuItem * load;

	name = (char *) "load-subscriptions-on-startup";
	libr3r_access_settings(&name, &value, &type, &count, SETTINGS_READ);

	InitGettext();

	/* The File Menu */
	wxMenu * menuFile = new wxMenu;
	menuFile->Append(wxID_OPEN, _("&Open..."));
	menuFile->AppendSeparator();
	menuFile->Append(wxID_EXIT, _("E&xit"));

	/* The Tools Menu */
	wxMenu * menuTools = new wxMenu;
	load = menuTools->Append(wxID_LOAD_SUBSCRIPTIONS, _("&Load Subscriptions"));
	menuTools->Append(wxID_REFRESH, _("&Refresh"));
	menuTools->AppendSeparator();
	menuTools->Append(wxID_SETTINGS, _("&Settings..."));

	load->Enable(!(bool) value);

	/* The Help Menu */
	wxMenu * menuHelp = new wxMenu;
	menuHelp->Append(wxID_DONATE, _("&Donate"));
	menuHelp->AppendSeparator();
	menuHelp->Append(wxID_CHECK_UPDATES, _("&Check for updates"));
	menuHelp->Append(wxID_ABOUT, _("&About"));

	/* Attach menus to the menu bar */
	wxMenuBar * menuBar = new wxMenuBar;
	menuBar->Append(menuFile, _("&File"));
	menuBar->Append(menuTools, _("&Tools"));
	menuBar->Append(menuHelp, _("&Help"));

	/* Set the main menu bar */
	parent->SetMenuBar(menuBar);
}
