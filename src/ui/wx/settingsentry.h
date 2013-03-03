#include "libr3r.h"
#include "settingslist.h"
#include "wx.h"

class SettingsEntry : public wxTextCtrl
{
	public:
		SettingsEntry(wxWindow * parent, char * setting_name);

	DECLARE_EVENT_TABLE()
};
