#include "events.h"
#include "gofield.h"
#include "libr3r.h"
#include "location.h"
#include "widgetids.h"

#include "i18n.h"

void CreateLocationBar(wxPanel * parent)
{
  InitGettext();
  wxSizer * sizer = parent->GetSizer();
  wxBoxSizer * hbox = new wxBoxSizer(wxHORIZONTAL);

  GoField * entry = new GoField(parent);
  entry->SetFocus();
  hbox->Add(entry, 1, wxALL | wxEXPAND, 5);

	while (libr3r_history_is_next())
	{
		entry->Insert(wxString(libr3r_history_next(), wxConvUTF8), 0);
	}

  wxButton * button = new wxButton(parent, wxID_GO_BUTTON, _("Go"), wxDefaultPosition, wxDefaultSize);
  hbox->Add(button, 0, wxALL, 5);
  button->SetClientData(entry);

  sizer->Add(hbox, 0, wxEXPAND);
}
