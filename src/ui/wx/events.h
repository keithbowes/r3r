#include "htmldescriptionbox.h"
#include "wx.h"

class DescriptionBoxEvents: public wxWindow
{
	public:
		void OnContact(wxCommandEvent & event);
		void OnEnclosure(wxCommandEvent & event);
		void OnSubscribe(wxCommandEvent & event);
};

class FeedListViewEvents: public wxWindow
{
	public:
		void OnActivate(wxListEvent & event);
		void OnSelect(wxListEvent & event);
};

class FrameEvents: public wxWindow
{
	public:
		void OnClose(wxCloseEvent & event);
		void OnSize(wxSizeEvent & event);
};

class GoButtonEvents : public wxWindow
{
	public:
		void OnClick(wxCommandEvent & event);
};

class GoFieldEvents: public wxWindow
{
	public:
		void OnKeyDown(wxKeyEvent & event);
};

class MenuEvents : public wxWindow
{
	public:
		void OnAbout(wxCommandEvent & event);
		void OnDonate(wxCommandEvent & event);
		void OnLoadSubscriptions(wxCommandEvent & event);
		void OnOpen(wxCommandEvent & event);
		void OnQuit(wxCommandEvent & event);
		void OnRefresh(wxCommandEvent & event);
		void OnSettings(wxCommandEvent & event);
};

class SettingsCheckBoxEvents : public wxWindow
{
	public:
		void OnClick(wxCommandEvent & event);
};

class SettingsDialogEvents: public wxWindow
{
	public:
		void OnCancel(wxCommandEvent & event);
		void OnClose(wxCloseEvent & event);
		void OnOK(wxCommandEvent & event);
};

class SettingsEntryEvents: public wxWindow
{
	public:
		void OnChange(wxCommandEvent & event);
};

class SubscriptionsEvents: public wxWindow
{
	public:
		void OnAdd(wxCommandEvent & event);
		void OnBrowse(wxCommandEvent & event);
		void OnDelete(wxCommandEvent & event);
};

class HtmlBoxEvents : public wxEvtHandler
{
	public:
		void OnLink(wxHtmlLinkEvent & event);
};
