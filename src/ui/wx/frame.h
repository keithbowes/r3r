#include "wx.h"

class RFrame : public wxFrame
{
  public:
    RFrame(const wxString & title, const wxPoint & pos, const wxSize & size);
    DECLARE_EVENT_TABLE()
};
