#include <wx/html/htmlwin.h>

class HtmlDescriptionBox : public wxHtmlWindow
{
  public:
    HtmlDescriptionBox(wxWindow * parent);

  DECLARE_EVENT_TABLE()
};
