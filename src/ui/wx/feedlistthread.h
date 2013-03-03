#include "wx.h"

class FeedListThread : public wxThread
{
	public:
		FeedListThread(wxThreadKind kind);
		void * Entry();

		void * GetEntryData();
		void SetEntryData(void * data);
};
