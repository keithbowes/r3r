#include "feedlist.h"
#include "libr3r.h"
#include "subscriptions.h"

#include "i18n.h"

void * rlib;
FeedListView * feedList;
bool topItem = false;

int rargc;
wxChar ** rargv;

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
	info->self = (char *) libr3r_get_item_field(item, (char *) "myself");
	info->enclosure.type = (char *) libr3r_get_item_field(item, (char *) "enclosure-type");
	info->enclosure.url = (char *) libr3r_get_item_field(item, (char *) "enclosure-url");

	wxListItem listItem;
	listItem.SetColumn(0);
	listItem.SetData(info);
	listItem.SetId(itemIndex);
	listItem.SetText(wxString(title, wxConvUTF8));

	if (topItem)
	{
		feedList->InsertItem(listItem);

		listItem.SetColumn(1);
		listItem.SetText(wxT(""));
		feedList->SetItem(listItem);
	}
	else
	{
		listItem.SetText(wxT(""));
		feedList->InsertItem(listItem);

		listItem.SetColumn(1);
		listItem.SetText(wxString(title, wxConvUTF8));
		feedList->SetItem(listItem);

		normalize_field_value(&created);
		normalize_field_value(&subject);
	}

	listItem.SetColumn(2);
	listItem.SetText(wxString(subject, wxConvUTF8));
	feedList->SetItem(listItem);

	listItem.SetColumn(3);
	listItem.SetText(wxString(created, wxConvUTF8));
	feedList->SetItem(listItem);

	topItem = false;
	itemIndex++;
}

void message_received(unsigned short int is_error, char * message_name, char * extra)
{
	int error_type = is_error ? wxICON_ERROR : wxICON_WARNING;

	wxMessageBox(wxString(message_name, wxConvUTF8), wxString(extra,wxConvUTF8), wxOK | error_type);
}

void CreateFeedList(wxPanel * parent)
{
	InitGettext();

	wxListItem listItem;
	wxSizer * sizer = parent->GetSizer();

	feedList = new FeedListView(parent, -1, wxDefaultPosition, wxDefaultSize, wxLC_REPORT | wxLC_SINGLE_SEL);

	listItem.SetText(_("Feed Title"));
	feedList->InsertColumn(0, listItem);

	listItem.SetText(_("Item Title"));
	feedList->InsertColumn(1, listItem);

	listItem.SetText(_("Subject"));
	feedList->InsertColumn(2, listItem);

	listItem.SetText(_("Date Created"));
	feedList->InsertColumn(3, listItem);

	sizer->Add(feedList, 2, wxEXPAND, 10);

	rlib = libr3r_create();
	libr3r_on_item_parsed(rlib, &item_parsed, NULL);
	libr3r_on_message_received(rlib, &message_received);

	feedList->SetClientData(rlib);
}

FeedListView * GetFeedList()
{
	return feedList;
}

void * ParseFeedThread(void * resource)
{
	FeedResource * res = (FeedResource *) resource;

	topItem = true;

	libr3r_retrieve_feed(res->lib, res->res);
	free(res);

	return NULL;
}

void ParseFeed(char * res)
{
	FeedResource * resource = (FeedResource *) malloc(sizeof(FeedResource));
	resource->lib = rlib;
	resource->res = res;
	ParseFeedThread(resource);
}

void LoadFeeds(int argc, wxChar ** argv)
{
	char * name, * s;
	int count;
	unsigned char type;
	void * value;
	name = (char *) "load-subscriptions-on-startup";
	libr3r_access_settings(&name, &value, &type, &count, SETTINGS_READ);

	rargc = argc;
	rargv = argv;

	if (argc > 1)
	{
		ParseFeed((char *) (const char *) wxString(argv[1], wxConvUTF8).mb_str());
	}
	else if ((bool) value)
	{
		Subscriptions * subs = GetSubscriptionsObject();
		while ((s = subs->GetNext()) != NULL)
		{
			ParseFeed(s);
		}
	}
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

int get_argc()
{
	return rargc;
}

wxChar ** get_argv()
{
	return rargv;
}
