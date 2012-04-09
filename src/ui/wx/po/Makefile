INTLTOOL_UPDATE ?= $(call programpath,intltool-update)

include ../../../../scripts/common.make

all:
ifneq ($(mo_files),)
	-$(MAKE) $(mo_files)
endif

install:
	-$(foreach l, $(LINGUAS), $(MKDIR) $(localedir)/$l; $(MKDIR) $(localedir)/$l/LC_MESSAGES; $(INSTALL) $l.mo $(localedir)/$l/LC_MESSAGES/r3r_wx.mo;)

uninstall:
	$(DEL) $(wildcard $(localedir)/*/LC_MESSAGES/r3r_wx.mo)

clean:
	$(DEL) $(mo_files)

%.mo: %.po
	-$(MSGFMT) $(MSGFMTFLAGS) -o $@ $<

%.po: r3r.pot
	-$(MSGMERGE) --backup=none --silent --update $@ $<
	-$(TOUCH) $@

r3r.pot: POTFILES.in
	-$(INTLTOOL_UPDATE) --gettext-package=r3r --pot
