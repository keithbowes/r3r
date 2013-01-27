#include "app.h"
#include "feedlist.h"
#include "frame.h"
#include "i18n.h"
#include "libr3r.h"
#include "wx.h"

IMPLEMENT_APP(R3R)

char * R3R::GetUserAgent()
{
	char * r = (char *) malloc(50);
	sprintf(r, "%s/%d.%d.%d", "wxWidgets", wxMAJOR_VERSION, wxMINOR_VERSION, wxRELEASE_NUMBER);
	return r;
}

bool R3R::OnInit()
{
  InitGettext();

  RFrame * frame = new RFrame(_("R3R"), wxDefaultPosition, wxSize(480, 380));
  frame->Show(TRUE);
  SetTopWindow(frame);

	char * ua = GetUserAgent();
	libr3r_set_user_agent_info(ua);
	free(ua);

  LoadFeeds(argc, argv);

  return TRUE;
}
