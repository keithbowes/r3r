include ../../../../scripts/common.make

install:
	$(MKDIR) $(rdatadir)
	$(MKDIR) $(skindir)
	$(INSTALL) $(wildcard *.skin) $(skindir)

uninstall:
	$(RMRF) $(wildcard $(skindir))
