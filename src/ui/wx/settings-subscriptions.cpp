#include "settings-subscriptions.h"
#include "settingsdialog.h"
#include "subscriptions.h"
#include "widgetids.h"

#include "i18n.h"

void FillSubscriptionsListBox(wxListBox * box)
{
	char * s;
	int i = 0;
	wxString * str;
	Subscriptions * subs = new Subscriptions();
	
	while ((s = subs->GetNext()) != NULL)
	{
		if (strlen(s) > 0)
		{
			str = new wxString(s, wxConvUTF8);
			box->InsertItems(1, str, i);
			i++;
		}
	}
}

void CreateSubscriptionsPage(wxTreebook * parent)
{
	InitGettext();

	wxSize mysize = DLGSIZE;
	mysize.x /= 3;
	mysize.y /= 2;

	SubscriptionData * data = (SubscriptionData *) malloc(sizeof(SubscriptionData));
	wxPanel * panel = new wxPanel(parent);

	wxBoxSizer * hbox = new wxBoxSizer(wxHORIZONTAL);
	panel->SetSizer(hbox);

	wxListBox * box = new wxListBox(panel, -1, wxDefaultPosition, mysize, 0, NULL, wxLB_NEEDED_SB | wxLB_HSCROLL);
	data->box = box;
	hbox->Add(box, 2, wxBOTTOM | wxEXPAND);

	FillSubscriptionsListBox(box);

	wxBoxSizer * vbox = new wxBoxSizer(wxVERTICAL);
	hbox->Add(vbox, 1);

	wxButton * del = new wxButton(panel, wxID_DELETE_SUBSCRIPTION, _("&Delete"));
	vbox->Add(del, 1, wxEXPAND | wxALL, 5);
	del->SetClientData(data);

	wxButton * add = new wxButton(panel, wxID_ADD_SUBSCRIPTION, _("&Add"));
	vbox->Add(add, 1, wxEXPAND | wxALL, 5);

	wxTextCtrl * addEntry = new wxTextCtrl(panel, -1);
	vbox->Add(addEntry, 1, wxEXPAND | wxALL, 5);
	data->entry = addEntry;
	add->SetClientData(data);

	parent->AddPage(panel, _("Subscriptions"));
}
