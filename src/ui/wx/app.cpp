#include "app.h"
#include "feedlist.h"
#include "frame.h"
#include "i18n.h"

IMPLEMENT_APP(R3R)

bool R3R::OnInit()
{
  InitGettext();

	if (argc > 1)
	{
		printf(_("The use of the command line is deprecated.  Please add feeds to which you want to subscribe to.\n"));
		exit(argc - 1);
	}

  RFrame * frame = new RFrame(_("R3R"), wxDefaultPosition, wxSize(480, 380));
  frame->Show(TRUE);
  SetTopWindow(frame);

  LoadFeeds();

  return TRUE;
}
