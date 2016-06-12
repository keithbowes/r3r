#define SETTINGS_READ 1
#define SETTINGS_WRITE 2

#define SUBSCRIPTIONS_ADD 1
#define SUBSCRIPTIONS_DELETE 2
#define SUBSCRIPTIONS_GET 3
#define SUBSCRIPTIONS_GETCOUNT 4

#define TYPE_NONE 0
#define TYPE_STRING 1
#define TYPE_INTEGER 2
#define TYPE_BOOLEAN 3

typedef void (TMessageProc) (unsigned short int is_error, char * message_name, char * extra);
typedef void (TParsedProc) (void * item, void * data);

#ifdef __cplusplus
 extern "C" {
#endif

extern void * libr3r_create(void);
extern void libr3r_free (void * lib);

extern void libr3r_queue_uri(void * lib, char * resource);
extern void libr3r_unqueue_uri(void *lib);

extern int libr3r_retrieve_chunk(void * lib);
extern void libr3r_retrieve_feed(void * lib, char * resource);

extern void libr3r_on_item_parsed(void * lib, TParsedProc proc, void * data);
extern void libr3r_on_message_received(void * lib, TMessageProc proc);
extern void * libr3r_get_item_field(void * item, char * field_name);

extern char * libr3r_get_user_agent(void);
extern void * libr3r_set_user_agent_info(char * uainfo);

extern void libr3r_register_setting(char * setting_name, char * setting_section, void * setting_value, unsigned int settings_type, char * desc);
extern void libr3r_access_settings(char ** setting_name, void ** setting_value, unsigned char * setting_type, int * settings_count, unsigned char settings_mode);

extern void libr3r_access_subscriptions(unsigned char index, unsigned char mode, char ** sub, unsigned int * count);

extern void libr3r_history_add(char * entry);
extern int libr3r_history_is_next(void);
extern char * libr3r_history_next(void);

extern char * libr3r_get_settings_dir(void);
extern char * libr3r_get_data_dir(void);
extern char * libr3r_get_cache_dir(void);

extern char * libr3r_get_version(void);

#ifdef __cplusplus
 }
#endif
