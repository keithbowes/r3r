#include "libr3r.h"
#include "settingslist.h"
#include "wx.h"

class SettingsEntry : public wxTextCtrl
{
  private:
    SettingsListElement * m_toggle;
  public:
    SettingsEntry(wxWindow * parent, char * setting_name, char * toggle = NULL);

    SettingsListElement * GetToggle();
    void SetToggle(SettingsListElement * toggle);

  DECLARE_EVENT_TABLE()
};
