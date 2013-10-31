include ../../../../scripts/common.make

install:
	$(MKDIR) $(docdir)
	$(INSTALL) $(wildcard *.html) $(docdir)

uninstall:
	$(RM) $(wildcard $(addprefix $(docdir),$(wildcard *.html)))
