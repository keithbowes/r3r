include ../../../../scripts/common.make

install:
	$(MKDIR) $(docdir)
	$(INSTALL) $(wildcard *.html) $(docdir)

uninstall:
	$(DEL) $(addprefix $(docdir),$(wildcard *.html))
