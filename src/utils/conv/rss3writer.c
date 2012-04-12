#include <libr3r.h>
#include <stdio.h>

#include "r3r-conv.h"
#include "rss3writer.h"

void write_rss3(void * item)
{
	FILE * fh = get_handle();
	if (!fh) return;

	fprintf(fh, "title: %s\nlink: %s\ndescription: %s\nSubject: %s\nGenerator: %s\nLanguage: %s\nGUID: %s\nRights: %s\n\n",
			ri(item, "title"), ri(item, "link"), ri(item, "description"), ri(item, "subject"),
			ri(item, "generator"), ri(item, "language"), ri(item, "id"), ri(item, "copyright"));

	if (out_file)
		fclose(fh);
}
