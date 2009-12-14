#define SETTINGS_READ 1
#define SETTINGS_WRITE 2

#define SUBSCRIPTIONS_ADD 1
#define SUBSCRIPTIONS_DELETE 2
#define SUBSCRIPTIONS_GET 3

#define TYPE_NONE 0
#define TYPE_STRING 1
#define TYPE_INTEGER 2
#define TYPE_BOOLEAN 3

#define SEPARATOR_CHAR "\x93"

typedef void (TMessageProc) (unsigned short int is_error, char * message_name, char * extra);
typedef void (TParsedProc) (void * item);
typedef void (TUpdateProc) ();

#ifdef __cplusplus
 extern "C" {
#endif

extern void * libr3r_create(void);
extern void libr3r_free (void * lib);

extern void libr3r_retrieve_feed(void * lib, char * resource);
extern void libr3r_on_item_parsed(void * lib, TParsedProc proc);
extern void libr3r_on_message_received(void * lib, TMessageProc proc);
extern void libr3r_on_update(void * lib, TUpdateProc proc);

extern void * libr3r_get_item_field(void * item, char * field_name);
extern char * libr3r_get_user_agent(void);

extern void libr3r_access_settings(int * index, char ** setting_name, void ** setting_value, unsigned char * setting_type, int * settings_count, unsigned char settings_mode);

extern void libr3r_access_subscriptions(unsigned char index, unsigned char mode, char ** sub, unsigned int * count);

#ifdef __cplusplus
 }
#endif
