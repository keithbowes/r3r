#include "feedlistview.h"

typedef struct
{
  char * type, * url;
} enclosure_data;

typedef struct
{
  bool isTopLevel;

  char * contact;
  char * desc;
  char * link;
  char * title;
  char * self;
  enclosure_data enclosure;
} ItemInfo;

typedef struct
{
  void * lib;
  char * res;
} FeedResource;

void CreateFeedList(wxPanel * parent);
FeedListView * GetFeedList();
void ResizeColumns(FeedListView * list);
void CleanupFeedList();
void ParseFeed(char * res);
void * ParseFeedThread(void * resource);
void LoadFeeds(int argc, wxChar ** argv);

void GoBrowser(char * link);

int get_argc();
wxChar ** get_argv();
