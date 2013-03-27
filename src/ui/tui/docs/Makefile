include ../../../../scripts/common.make

install:
	$(MKDIR) $(docdir)
	$(INSTALL) $(wildcard *.html) $(docdir)

uninstall:
	$(DEL) $(wildcard $(addprefix $(docdir),$(wildcard *.html)))
