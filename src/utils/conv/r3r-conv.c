#include <libintl.h>
#include <libr3r.h>
#include <locale.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "r3r-conv.h"

#ifdef _
#undef _
#endif

#define _ gettext

char is_top_elem = 1;

void item_received(void * item)
{
	char * s = (char *) libr3r_get_item_field(item, "title");
	if (0 == strcmp(out_type, "esf"))
		write_esf(item, is_top_elem);
	else if (0 == strcmp(out_type, "rss3"))
		write_rss3(item);
	else if (0 == strcmp(out_type, "rss"))
		write_rss(item, is_top_elem);
	else if (0 == strcmp(out_type, "atom"))
		write_atom(item, is_top_elem);
	else
		printf(_("Out-file type must be one of atom, esf, rss, rss3\n"));
	is_top_elem = 0;
}

void show_help(char * invok)
{
	printf(_("Usage: %s -i in_file -t out_type [-o out_file])\n"), invok);
}

FILE * get_handle()
{
	static char erste = 1;

	FILE * fh;
	if (!out_file)
		fh = stdout;
	else
	{
		fh = fopen(out_file, "a+");
		if (!fh)
		{
			fclose(fh);
			return NULL;
		}
		
		if (erste)
		{
			fclose(fh);
			fh = fopen(out_file, "w+");
			erste = 0;
		}

		fclose(fh);

		fh = fopen(out_file, "a");
		if (!fh)
		{
			fclose(fh);
			return NULL;
		}
	}

	return fh;
}

char * get_locale_dir()
{
	int count;
	char * name, * s;
	char type;
	void * value;

	name = "installed-prefix";
	libr3r_access_settings(&name, &value, &type, &count, SETTINGS_READ);

	s = malloc(sizeof(char) * 256);
	sprintf(s, "%s/share/locale", (char *) value);
	return s;
}

int main(int argc, char ** argv)
{
	char * locdir = get_locale_dir();
	setlocale(LC_ALL, "");
	textdomain("r3r_conv");
	bindtextdomain("r3r_conv", locdir);
	free(locdir);

	int i;
	char * invok = argv[0];
	const char * optstring = "hi:o:t:";
	do
	{
		i = getopt(argc, argv, optstring);
		if (i >= 0)
		{
			char * opts = argv[optind - 2];
			char opt = opts[1];
			switch (opt)
			{
				case 'h':
					show_help(invok);
					return;
				case 'i':
					in_file = optarg;
					break;
				case 'o':
					out_file = optarg;
				  break;
				case 't':
					out_type = optarg;
					break;
			}
		}
	}
	while (i >= 0);

	if (!in_file || !out_type)
	{
		show_help(invok);
		return 0;
	}

	void * feed = libr3r_create();
	libr3r_on_item_parsed(feed, &item_received);
	libr3r_retrieve_feed(feed, in_file /*"/home/keith/Documents/programs/r3r/feed.atom"*/);
	libr3r_free(feed);

	if (0 == strcmp("rss", out_type))
		close_rss();
	else if (0 == strcmp("atom", out_type))
		close_atom();

	return 0;
}

