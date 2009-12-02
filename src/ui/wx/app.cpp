#include "app.h"
#include "feedlist.h"
#include "frame.h"

IMPLEMENT_APP(R3R);

bool R3R::OnInit()
{
  wxLocale locale;
  locale.AddCatalogLookupPathPrefix(LOCALEDIR);
  locale.AddCatalog("r3r_wx");

  RFrame * frame = new RFrame(_("R3R"), wxDefaultPosition, wxSize(480, 380));

  frame->Show(TRUE);
  SetTopWindow(frame);

  GetAllFeeds(argc, argv);

  return TRUE;
}
