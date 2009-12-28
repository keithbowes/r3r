#include <libintl.h>

void InitGettext();

#ifdef USE_SYSTEM_GETTEXT
#ifdef _
#undef _
#endif
#define _(str) gettext(str)
#endif
