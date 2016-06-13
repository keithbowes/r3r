#include "feedlist.h"
#include "libr3r.h"
#include "subscriptions.h"

#include "i18n.h"

FeedList * feedList;
FeedListView * feedListView;
bool topItem = TRUE;

void normalize_field_value(char ** field_value)
{
	char * s = *field_value;

	if (0 == strlen(s))
	{
		wxString t = _("[None]");
		s = (char *) malloc(t.Length() * sizeof(wxChar));
		strcpy(s, (char *) t.char_str());

		strcpy(*field_value, s);
		free(s);
	}
}

void item_parsed(void * item, void * data)
{
	static long itemIndex = 0;
	char * created = (char *) libr3r_get_item_field(item, (char *) "created");
	char * desc = (char *) libr3r_get_item_field(item, (char *) "description");
	char * subject = (char *) libr3r_get_item_field(item, (char *) "subject");
	char * title = (char *) libr3r_get_item_field(item, (char *) "title-text");

	normalize_field_value(&desc);

	ItemInfo * info = (ItemInfo *) malloc(sizeof(ItemInfo));
	info->isTopLevel = topItem;
	info->desc = desc;
	info->link = (char *) libr3r_get_item_field(item, (char *) "link");
	info->title = title;
	info->contact = (char *) libr3r_get_item_field(item, (char *) "contact-email");
	info->self = (char *) libr3r_get_item_field(item, (char *) "self");
	info->enclosure.type = (char *) libr3r_get_item_field(item, (char *) "enclosure-type");
	info->enclosure.url = (char *) libr3r_get_item_field(item, (char *) "enclosure-url");

	wxListItem listItem;
	listItem.SetColumn(0);
	listItem.SetData(info);
	listItem.SetId(itemIndex);
	listItem.SetText(wxString(title, wxConvUTF8));

	if (topItem)
	{
		feedListView->InsertItem(listItem);

		listItem.SetColumn(1);
		listItem.SetText(wxT(""));
		feedListView->SetItem(listItem);
	}
	else
	{
		listItem.SetText(wxT(""));
		feedListView->InsertItem(listItem);

		listItem.SetColumn(1);
		listItem.SetText(wxString(title, wxConvUTF8));
		feedListView->SetItem(listItem);

		normalize_field_value(&created);
		normalize_field_value(&subject);
	}

	listItem.SetColumn(2);
	listItem.SetText(wxString(subject, wxConvUTF8));
	feedListView->SetItem(listItem);

	listItem.SetColumn(3);
	listItem.SetText(wxString(created, wxConvUTF8));
	feedListView->SetItem(listItem);

	topItem = (bool) libr3r_get_item_field(item, (char *) "finished");
	itemIndex++;
}

void message_received(unsigned short int is_error, char * message_name, char * extra)
{
	int error_type = is_error ? wxICON_ERROR : wxICON_WARNING;

	wxMessageBox(wxString(message_name, wxConvUTF8), wxString(extra,wxConvUTF8), wxOK | error_type);
}

FeedList::FeedList()
{
	libr3r_create();
	libr3r_on_item_parsed(&item_parsed, NULL);
	libr3r_on_message_received(&message_received);
}

FeedList::~FeedList()
{
	libr3r_free();
}

FeedList * GetFeedList()
{
	if (feedList)
	{
		return feedList;
	}

	return new FeedList();
}

FeedListView * GetFeedListView()
{
	return feedListView;
}

wxListView * FeedList::CreateView(wxPanel * parent)
{
	InitGettext();

	wxListItem listItem;
	wxSizer * sizer = parent->GetSizer();

	feedListView = new FeedListView(parent, -1, wxDefaultPosition, wxDefaultSize, wxLC_REPORT | wxLC_SINGLE_SEL);

	listItem.SetText(_("Feed Title"));
	feedListView->InsertColumn(0, listItem);

	listItem.SetText(_("Item Title"));
	feedListView->InsertColumn(1, listItem);

	listItem.SetText(_("Subject"));
	feedListView->InsertColumn(2, listItem);

	listItem.SetText(_("Date Created"));
	feedListView->InsertColumn(3, listItem);

	sizer->Add(feedListView, 2, wxEXPAND, 10);
	return feedListView;
}

void FeedList::Add(char * feed, bool now)
{
	libr3r_queue_uri(feed);

	if (now)
	{
		Parse();
	}
}

void FeedList::Parse()
{
	while (libr3r_retrieve_chunk())
	{
		wxApp::GetInstance()->Yield();
	}
}

void FeedList::Load()
{
	int count;
	char * name;
	unsigned char type;
	void * value;
	name = (char *) "load-subscriptions-on-startup";
	libr3r_access_settings(&name, &value, &type, &count, SETTINGS_READ);

	if (wxApp::GetInstance()->argc > 1)
	{
		Add((char *) (const char *) wxApp::GetInstance()->argv[1]);
	}
	else if ((bool) value)
	{
		LoadSubscriptions();
	}
}

void FeedList::LoadSubscriptions()
{
	char * s;

	Subscriptions * subs = GetSubscriptionsObject();
	while (NULL != (s = subs->GetNext()))
	{
		Add(s, FALSE);
	} 

	Parse();
}

void GoBrowser(char * url)
{
	char * name;
	int count;
	unsigned char type;
	void * value;
	wxString browser, command, URL;

	if (!url) return;

	name = (char *) "for:http";
	libr3r_access_settings(&name, &value, &type, &count, SETTINGS_READ);

	browser = wxString((char *) value, wxConvUTF8);
	URL = wxString(url, wxConvUTF8);

	if (wxNOT_FOUND == browser.Find(wxT("%1")))
	{
		command = browser + wxT(" ") + wxT("\"") + URL + wxT("\"");
	}
	else
	{
		browser.Replace(wxT("%1"), URL);
		URL.Clear();
		command = browser;
	}

	wxExecute(command);
}
