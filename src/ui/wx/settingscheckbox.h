#include "wx.h"

class SettingsCheckBox : public wxCheckBox
{
  public:
    SettingsCheckBox(wxWindow * parent, const wxString & label, char * setting_name);

  DECLARE_EVENT_TABLE()
};
