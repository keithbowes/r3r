#include "wx.h"

#ifdef USE_HTML_DESCRIPTION_BOX
#include <wx/wxhtml.h>
#endif /* USE_HTML_DESCRIPTION_BOX */

void CreateDescriptionBox(wxPanel * parent);
wxStaticBox * GetDescriptionBox();
wxButton * GetContactButton();
wxButton * GetPodcastButton();
wxButton * GetSubscribeButton();
