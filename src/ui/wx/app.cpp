#include "app.h"
#include "feedlist.h"
#include "frame.h"
#include "i18n.h"
#include "libr3r.h"

IMPLEMENT_APP(R3R)

bool R3R::OnInit()
{
  InitGettext();
  RFrame * frame = new RFrame(_("R3R"), wxDefaultPosition, wxSize(480, 380));

  frame->Show(TRUE);
  SetTopWindow(frame);

  GetAllFeeds(argc, argv);

  return TRUE;
}
