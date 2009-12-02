#include "wx.h"

class GoField : public wxTextCtrl
{
  public:
    GoField(wxWindow * parent);

  DECLARE_EVENT_TABLE()
};
