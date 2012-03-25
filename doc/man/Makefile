include ../../scripts/common.make

mandir = $(datadir)/man

install:
ifneq ($(USE_NLS),0)
	$(MKDIR) $(mandir)
	$(MKDIR) $(mandir)/man1
	$(INSTALL) r3r.1 $(mandir)/man1
	$(foreach d,$(wildcard r3r.1.*),$(MKDIR) $(mandir)/$(subst r3r.1.,,$d); $(MKDIR) $(mandir)/$(subst r3r.1.,,$d)/man1; $(INSTALL) $d $(mandir)/$(subst r3r.1.,,$d)/man1/$(basename $d); )
endif

uninstall:
	-$(RM) $(mandir)/man1/r3r.1 $(wildcard $(mandir)/*/man1/r3r.1)
	-$(RMDIR) $(mandir)/man1 $(wildcard $(mandir)/*/man1)
