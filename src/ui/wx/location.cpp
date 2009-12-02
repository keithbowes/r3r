#include "events.h"
#include "gofield.h"
#include "location.h"
#include "widgetids.h"

void CreateLocationBar(wxPanel * parent)
{
  wxSizer * sizer = parent->GetSizer();
  wxBoxSizer * hbox = new wxBoxSizer(wxHORIZONTAL);

  GoField * entry = new GoField(parent);
  entry->SetFocus();
  hbox->Add(entry, 1, wxALL | wxEXPAND, 5);

  wxButton * button = new wxButton(parent, wxID_GO_BUTTON, _("Go"), wxDefaultPosition, wxDefaultSize);
  hbox->Add(button, 0, wxALL, 5);
  button->SetClientData(entry);

  sizer->Add(hbox, 0, wxEXPAND);
}
