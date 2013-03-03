#include "events.h"
#include "settings-display.h"
#include "settings-headers.h"
#include "settings-http.h"
#include "settings-main.h"
#include "settings-programs.h"
#include "settings-subscriptions.h"
#include "settingsdialog.h"

SettingsDialog * dlg;
wxFrame *pFrame;

void CreateSettingsDialog(wxFrame * parent)
{
	dlg = new SettingsDialog(parent);
	pFrame = parent;

	wxBoxSizer * vbox = new wxBoxSizer(wxVERTICAL);
	dlg->SetSizer(vbox);

	wxBoxSizer * hbox = new wxBoxSizer(wxHORIZONTAL);
	vbox->Add(hbox, 2, wxEXPAND);

	wxTreebook * view = new wxTreebook(dlg, -1);
	hbox->Add(view, 1, wxEXPAND | wxALL);

	wxSizer * sizer = dlg->CreateButtonSizer(wxOK | wxCANCEL);
	vbox->Add(sizer, 1, wxEXPAND| wxALL, 10);

	CreateDisplayPage(view);
	CreateHTTPPage(view);
	CreateHeadersPage(view);
	CreateProgramsPage(view);
	CreateSubscriptionsPage(view);
}

wxDialog * GetSettingsDialog()
{
	if (!dlg)
	{
		CreateSettingsDialog(pFrame);
	}

	return dlg;
}

void ShowSettingsDialog()
{
	GetSettingsDialog();
	dlg->Show(TRUE);
	dlg->Centre();
}

void HideSettingsDialog()
{
	if (dlg)
	{
		delete dlg;
		dlg = NULL;
	}
}
