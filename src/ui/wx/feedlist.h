#include "feedlistview.h"

typedef struct
{
  long index;
  bool isTopLevel;

  char * contact;
  char * desc;
  char * link;
  char * title;
} ItemInfo;

void CreateFeedList(wxPanel * parent);
FeedListView * GetFeedList();
void ResizeColumns(FeedListView * list);
void CleanupFeedList();
void ParseFeed(char * res);
void GetAllFeeds(int argc, char ** argv);

void GoBrowser(char * link);
