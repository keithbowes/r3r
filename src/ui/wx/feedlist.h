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

class FeedList
{
	public:
		FeedList();
		wxListView * CreateView(wxPanel * parent);
		void Add(char * feed, bool now = true);
		void Parse();
		void Load();
		void LoadSubscriptions();
};

FeedList * GetFeedList();
FeedListView * GetFeedListView();

void GoBrowser(char * link);
