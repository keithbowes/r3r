#include "wx.h"

#if defined(__SMARTPHONE__) || defined(__PDA__) || \
	defined(__HANDHELD__) || defined(__POCKETPC) || \
	defined(__WINCE_STANDARDSDK__) || defined(__WINCE__NET__) || \
	defined(WIN32_PLATFORM_WFSP)
#define MOBILE
#endif

#ifndef MOBILE
#define HEIGHT 320
#define WIDTH HEIGHT / 2 * 3 + 8
#define DLGSIZE wxSize(WIDTH, HEIGHT)
#else
#define DLGSIZE wxDefaultSize
#endif

class SettingsDialog: public wxDialog
{
	public:
		SettingsDialog(wxWindow * parent);

	DECLARE_EVENT_TABLE()
};
