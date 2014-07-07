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
	if (this)
	{
		GetClientSize(&w, &h);

		if (w > 4)
		{
			int wdate, wfeed, witem, wsubj;
			wdate = w / 4;
			wfeed = w / 5;
			wsubj = w / 5;
			witem = w - (wdate + wfeed + wsubj);

			SetColumnWidth(0, wfeed);
			SetColumnWidth(1, witem);
			SetColumnWidth(2, wsubj);
			SetColumnWidth(3, wdate);

			FitInside();
		}
	}
}
