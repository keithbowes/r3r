#include <libr3r.h>
#include <stdio.h>

#include "r3r-conv.h"
#include "atomwriter.h"

void write_atom(void * item, char is_top)
{
	static erste = 1;

	FILE * fh = get_handle();
	if (!fh) return;

	if (erste)
	{
		fprintf(fh, "<?xml version=\"1.0\"?>\n<feed version=\"1.0\" xmlns=\"http://www.w3.org/2005/Atom\" xml:lang=\"%s\">\n", ri(item, "language"));
		erste = 0;
	}

	if (!is_top)
		fprintf(fh, "<entry xml:lang=\"%s\">\n", ri(item, "language"));

	fprintf(fh, "<title>%s</title>\n<link rel=\"alternate\" href=\"%s\"/>\n<link rel=\"self\" href=\"%s\"/>\n<link rel=\"enclosure\" href=\"%s\"/>\n<content type=\"html\"><![CDATA[\n%s\n]]></content>\n<category label=\"%s\"/>\n<created>%s</created>\n<modified>%s</modified>\n<author>\n<name>%s</name>\n<email>%s</email>\n</author>\n<generator>%s</generator>\n<id>%s</id>\n<copyright>%s</copyright>\n",
			ri(item, "title"), ri(item, "link"), ri(item, "myself"), ri(item, "enclosure-url"), ri(item, "description"), ri(item, "subject"), ri(item, "created"), ri(item, "last-modified"), ri(item, "contact-name"), ri(item, "contact-email"), ri(item, "generator"), ri(item, "id"), ri(item, "copyright"));

	if (!is_top)
		fprintf(fh, "</entry>\n");

	if (out_file)
		fclose(fh);
}

void close_atom(void)
{
	FILE * fh = get_handle();
	if (!fh) return;
	fprintf(fh, "</feed>\n");
	
	if (out_file)
		fclose(fh);
}
