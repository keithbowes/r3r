#include "events.h"
#include "gofield.h"
#include "libr3r.h"
#include "location.h"
#include "widgetids.h"

#include "i18n.h"

GoField * entry;

void AddLocationHistory(const char * histent)
{
	entry->Insert(wxString(histent, wxConvUTF8), 0);
}

void CreateLocationBar(wxPanel * parent)
{
  InitGettext();
  wxSizer * sizer = parent->GetSizer();
  wxBoxSizer * hbox = new wxBoxSizer(wxHORIZONTAL);

  entry = new GoField(parent);
  entry->SetFocus();
  hbox->Add(entry, 1, wxALL | wxEXPAND, 5);

	while (libr3r_history_is_next())
	{
		AddLocationHistory(libr3r_history_next());
	}

  wxButton * button = new wxButton(parent, wxID_GO_BUTTON, _("Go"), wxDefaultPosition, wxDefaultSize);
  hbox->Add(button, 0, wxALL, 5);
  button->SetClientData(entry);

  sizer->Add(hbox, 0, wxEXPAND);
}
