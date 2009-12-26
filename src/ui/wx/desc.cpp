#include "desc.h"
#include "widgetids.h"

#include "i18n.h"

wxButton * contact, * podcast, * subscribe;
wxStaticBox * descBox;

void CreateDescriptionBox(wxPanel * parent)
{
  InitGettext();
  wxSizer * sizer = parent->GetSizer();

  descBox = new wxStaticBox(parent, -1, "");

  wxTextCtrl * memo = new wxTextCtrl(parent, -1, "", wxDefaultPosition, wxDefaultSize, wxTE_MULTILINE | wxTE_READONLY);
  descBox->SetClientData(memo);

  wxStaticBoxSizer * bsizer = new wxStaticBoxSizer(descBox, wxVERTICAL);
  bsizer->Add(memo, 1, wxEXPAND | wxALL, 5);

  wxBoxSizer * hbox = new wxBoxSizer(wxHORIZONTAL);
  bsizer->Add(hbox, 1, wxALL, 5);

  contact = new wxButton(parent, wxID_CONTACT_BUTTON, _("&Contact"));
  contact->Disable();
  hbox->Add(contact, 1, wxALL | wxALIGN_RIGHT, 5);

  subscribe = new wxButton(parent, wxID_SUBSCRIBE_BUTTON, _("&Subscribe"));
  subscribe->Disable();
  hbox->Add(subscribe, 1, wxALL | wxALIGN_RIGHT, 5);

  podcast = new wxButton(parent, wxID_PODCAST_BUTTON, _("&Play Podcast..."));
  podcast->Disable();
  hbox->Add(podcast, 1, wxALL | wxALIGN_RIGHT, 5);

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

wxButton * GetPodcastButton()
{
  return podcast;
}

wxButton * GetSubscribeButton()
{
  return subscribe;
}
