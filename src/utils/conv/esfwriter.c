#include <stdio.h>

#include "esfwriter.h"
#include "r3r-conv.h"

void write_esf(void * item, char is_top)
{
	FILE * fh = get_handle();
	if (!fh) return;

	if (is_top)
	{
		fprintf(fh, "title\t%s\n", ri(item, "title"));
		fprintf(fh, "description\t\n", ri(item, "description"));
		fprintf(fh, "link\t\n", ri(item, "link"));
		fprintf(fh, "contact\t\n", ri(item, "contact-email"));
		fprintf(fh, "\n");
	}
	else
	{
		char * created = format_time(ri(item, "created"), "%s");
		fprintf(fh, "%d\t%s\t%s\n", created, ri(item, "title"), ri(item, "link"));
	}

	if (out_file)
		fclose(fh);
}
