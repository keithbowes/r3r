#include "wx.h"

class FeedListView : public wxListView
{
  public:
    FeedListView(wxWindow * parent, wxWindowID id, const wxPoint & pos, const wxSize & size, long style);
    virtual ~FeedListView();
    void ResizeColumns();

  DECLARE_EVENT_TABLE();
};
