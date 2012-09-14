#include <libr3r.h>
#include <stdio.h>

#include "r3r-conv.h"
#include "rsswriter.h"

void write_rss(void * item, char is_top)
{
	static erste = 1;

	FILE * fh = get_handle();
	if (!fh) return;

	if (erste)
	{
		fprintf(fh, "<?xml version=\"1.0\"?>\n<rss version=\"2.0\" xmlns:atom=\"http://www.w3.org/2005/Atom\">\n<channel>\n");
		erste = 0;
	}

	if (!is_top)
		fprintf(fh, "<item>\n");

	char * created = format_time(ri(item, "created"), "%a, %d %b %y %T %z");
	char * modified = format_time(ri(item, "last-modified"), "%a, %d %b %y %T %z");

	fprintf(fh, "<title>%s</title>\n<link>%s</link>\n<description><![CDATA[\n%s]]>\n</description>\n<category>%s</category>\n<pubDate>%s</pubDate>\n<lastPubDate>%s</lastPubDate>\n<generator>%s</generator>\n<guid>%s</guid>\n<language>%s</language>\n<copyright>%s</copyright>\n<atom:link rel=\"self\" href=\"%s\"/>\n<enclosure>%s</enclosure>\n",
			ri(item, "title"), ri(item, "link"), ri(item, "description"), ri(item, "subject"), created, modified, ri(item, "generator"), ri(item, "id"), ri(item, "language"), ri(item, "copyright"), ri(item, "myself"), ri(item, "enclosure-url"));

	if (!is_top)
		fprintf(fh, "</item>\n");

	if (out_file)
		fclose(fh);
}

void close_rss(void)
{
	FILE * fh = get_handle();
	if (!fh) return;
	fprintf(fh, "</channel>\n</rss>\n");
	
	if (out_file)
		fclose(fh);
}
