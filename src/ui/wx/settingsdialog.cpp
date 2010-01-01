#include "settingsdialog.h"

#include "i18n.h"

#ifdef WIN32
#define DLGSIZE wxSize(400, 300)
#else
#define DLGSIZE wxDefaultSize
#endif

SettingsDialog::SettingsDialog(wxWindow * parent) :
  wxDialog(parent, -1, _("Settings"), wxDefaultPosition, DLGSIZE)
{
}
