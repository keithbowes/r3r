#include "feedlist.h"
#include "feedlistthread.h"

void * thread_data = NULL;

FeedListThread::FeedListThread(wxThreadKind kind = wxTHREAD_DETACHED) :
	wxThread(kind)
{
}

void * FeedListThread::Entry()
{
	return ParseFeedThread(thread_data);
}

void * FeedListThread::GetEntryData()
{
	return thread_data;
}

void FeedListThread::SetEntryData(void * data)
{
	thread_data = data;
}
