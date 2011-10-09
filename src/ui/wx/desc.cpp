#include "desc.h"
#include "htmldescriptionbox.h"
#include "widgetids.h"

#include "i18n.h"

wxButton * contact, * enclosure, * subscribe;
wxStaticBox * descBox;

void CreateDescriptionBox(wxPanel * parent)
{
  InitGettext();
  wxSizer * sizer = parent->GetSizer();

  descBox = new wxStaticBox(parent, -1, (wxChar *) NULL);
	
	HtmlDescriptionBox * html = new HtmlDescriptionBox(parent);
  descBox->SetClientData(html);

  wxStaticBoxSizer * bsizer = new wxStaticBoxSizer(descBox, wxVERTICAL);

  bsizer->Add(html, 1, wxEXPAND | wxALL, 5);

  wxBoxSizer * hbox = new wxBoxSizer(wxHORIZONTAL);
  bsizer->Add(hbox, 1, wxALL, 5);

  contact = new wxButton(parent, wxID_CONTACT_BUTTON, _("&Contact"));
  contact->Disable();
  hbox->Add(contact, 1, wxALL | wxALIGN_RIGHT, 5);

  subscribe = new wxButton(parent, wxID_SUBSCRIBE_BUTTON, _("&Subscribe"));
  subscribe->Disable();
  hbox->Add(subscribe, 1, wxALL | wxALIGN_RIGHT, 5);

  enclosure = new wxButton(parent, wxID_ENCLOSURE_BUTTON, _("&View Enclosure..."));
  enclosure->Disable();
  hbox->Add(enclosure, 1, wxALL | wxALIGN_RIGHT, 5);

  sizer->Add(bsizer, 2, wxEXPAND | wxALL, 5);
}

wxStaticBox * GetDescriptionBox()
{
  return descBox;
}

wxButton * GetContactButton()
{
  return contact;
}

wxButton * GetEnclosureButton()
{
  return enclosure;
}

wxButton * GetSubscribeButton()
{
  return subscribe;
}
