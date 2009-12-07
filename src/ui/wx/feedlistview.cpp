#include "feedlistview.h"
#include "libr3r.h"

FeedListView::FeedListView(wxWindow * parent, wxWindowID id, const wxPoint & pos, const wxSize & size, long style) :
  wxListView(parent, id, pos, size, style)
{
}

FeedListView::~FeedListView()
{
  libr3r_free(GetClientData());
}

void FeedListView::ResizeColumns()
{
  int h, w;
  if(this)
  {
    GetClientSize(&w, &h);

    SetColumnWidth(0, w / 5);
    SetColumnWidth(1, w / 3);
    SetColumnWidth(2, w / 5);
    SetColumnWidth(3, w / 4);
  }
}
