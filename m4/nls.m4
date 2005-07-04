dnl Macros to enable Native Language Support (NLS)
dnl If you add a new language, be sure to add the code to all the macros
dnl Try to keep the languages alphabetical (de, en, fr, zh)
dnl
define(Get_Language_Deps, po/r3r$(POTEXT) ifdef(USE_NLS, po/en$(MOEXT) po/eo$(MOEXT)))dnl
define(Install_Langs,
	-$(MKDIR) $(localedir)/en/LC_MESSAGES
	-$(MKDIR) $(localedir)/eo/LC_MESSAGES
	$(INSTALL) po/en$(MOEXT) $(localedir)/en/LC_MESSAGES/r3r$(MOEXT)
	$(INSTALL) po/eo$(MOEXT) $(localedir)/eo/LC_MESSAGES/r3r$(MOEXT))dnl
define(Uninstall_Langs,
	$(RM) $(localedir)/en/LC_MESSAGES/r3r$(MOEXT)
	$(RM) $(localedir)/eo/LC_MESSAGES/r3r$(MOEXT))dnl
define(Ap_Copy_Langs, ifdef(USE_NLS,
copyFiles r3r.en.mo "$PREFIX/share/locale/en/LC_MESSAGES/r3r.mo"
copyFiles r3r.eo.mo "$PREFIX/share/locale/eo/LC_MESSAGES/r3r.mo"))dnl
define(Is_Install_Langs, ifdef(USE_NLS,
Source: "..\..\po\r3r.en.mo"; DestDir: "{app}\share\locale\en\LC_MESSAGES\r3r.mo";
Source: "..\..\po\r3r.eo.mo"; DestDir: "{app}\share\locale\eo\LC_MESSAGES\r3r.mo";))dnl
