#include "feedlistview.h"

typedef struct
{
  bool isTopLevel;

  char * contact;
  char * desc;
  char * link;
  char * title;
  char * self;
  char * podcast;
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
void GetAllFeeds(int argc, wxChar ** argv);

void GoBrowser(char * link);

void SendOwnMessage(unsigned char is_error, char * message, char * extra);
