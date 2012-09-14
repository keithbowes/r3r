#define ri(item, field) libr3r_get_item_field(item, field) ? (char *) libr3r_get_item_field(item, field): ""

char * in_file, * out_file, * out_type;

void item_received(void * item);
void show_help(char * invok);

char * format_time(char * in_time, char * out_fmt);

FILE * get_handle();
