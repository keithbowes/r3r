#include "feedlist.h"
#include "libr3r.h"

#include "i18n.h"

void * rlib;
FeedListView * feedList;
bool topItem = FALSE;

int wxCALLBACK sort_items(long item1, long item2, long WXUNUSED(sortData))
{
  item1 = ((ItemInfo *) item1)->index;
  item2 = ((ItemInfo *) item2)->index;
  if (item1 < item2)
    return -1;
  if (item1 > item2)
    return 1;

  return 0;
}

void normalize_field_value(char ** field_value)
{
  char * s = *field_value;

  if (0 == strlen(s))
  {
    s = (char *) _("[None]");
  }

  *field_value = s;
}

void item_parsed(void * item)
{
  static long itemIndex;
  itemIndex++;

  char * created = (char *) libr3r_get_item_field(item, (char *) "created");
  char * desc = (char *) libr3r_get_item_field(item, (char *) "description");
  char * subject = (char *) libr3r_get_item_field(item, (char *) "subject");
  char * title = (char *) libr3r_get_item_field(item, (char *) "title");

  normalize_field_value(&desc);

  ItemInfo * info = (ItemInfo *) malloc(sizeof(ItemInfo));
  info->isTopLevel = topItem;
  info->index = itemIndex;
  info->desc = desc;
  info->link = (char *) libr3r_get_item_field(item, (char *) "main-link");
  info->title = title;
  info->contact = (char *) libr3r_get_item_field(item, (char *) "contact-email");

  wxListItem listItem;
  listItem.SetColumn(0);
  listItem.SetData(info);
  listItem.SetText(title);

  if (topItem)
  {
    feedList->InsertItem(listItem);

    listItem.SetColumn(1);
    listItem.SetText("");
    feedList->SetItem(listItem);
  }
  else
  {
    listItem.SetText("");
    feedList->InsertItem(listItem);

    listItem.SetColumn(1);
    listItem.SetText(title);
    feedList->SetItem(listItem);

    normalize_field_value(&created);
    normalize_field_value(&subject);
  }

  listItem.SetData(item);

  listItem.SetColumn(2);
  listItem.SetText(subject);
  feedList->SetItem(listItem);

  listItem.SetColumn(3);
  listItem.SetText(created);
  feedList->SetItem(listItem);

  feedList->SortItems(&sort_items, 0);

  topItem = FALSE;
}

void message_received(unsigned short int is_error, char * message_name, char * extra)
{
  int error_type = is_error ? wxICON_ERROR : wxICON_WARNING;

  wxMessageBox(message_name, extra, wxOK | error_type);
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
  feedList->SetClientData(rlib);
}

FeedListView * GetFeedList()
{
  return feedList;
}

void ParseFeed(char * res)
{
  topItem = TRUE;

  libr3r_on_item_parsed(rlib, &item_parsed);
  libr3r_on_message_received(rlib, &message_received);
  libr3r_retrieve_feed(rlib, res);
}

void GetAllFeeds(int argc, char ** argv)
{
  int i;

  for (i = 1; i < argc; i++)
  {
    ParseFeed(argv[i]);
  }
}

void GoBrowser(char * url)
{
  char * name;
  unsigned char count, index, type;
  void * value;
  wxString browser, command, URL;

  index = 0;
  name = (char *) "browser";
  libr3r_access_settings(&index, &name, &value, &type, &count, SETTINGS_READ);

  browser = wxString((char *) value);
  URL = wxString(url);
  command = browser + " " + URL;

  wxExecute(command);
}
