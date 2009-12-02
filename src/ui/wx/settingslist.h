#include "wx.h"

class SettingsListElement
{
  private:
    char * m_name;
    unsigned char m_type;
    void * m_value;
  public:
    char * GetName();
    void SetName(char * name);

    unsigned char GetType();
    void SetType(unsigned char type);

    void * GetValue();
    void SetValue(void * value);
};

WX_DECLARE_LIST(SettingsListElement, SettingsList);

SettingsList * GetSettingsList();
