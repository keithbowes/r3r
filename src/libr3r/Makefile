EXEOUT = .
TARGET_DIRS = po settings protocols formats
UNIT_DIRS = $(TARGET_DIRS) ../bindings/* ../utils/*
UNITS = info rfilter rgetfeed rhistory rmessage rparseurl rsubscriptions

include ../../scripts/common.make

all:
	-$(TOUCH) _info.pas
	$(MAKE) info.pas
	$(MAKE) info$(PPUEXT)
	$(foreach dir,$(TARGET_DIRS),cd $(dir) && $(MAKE) all && cd ..;)
	$(MAKE) $(addsuffix $(PPUEXT),$(UNITS))
	cd ../utils && $(MAKE) r3r-conv
	$(MAKE) libr3r$(PPUEXT)
ifdef BUILD_SHARED
ifneq ($(BUILD_SHARED),0)
	$(MAKE) libr3r_shared$(PPUEXT) && $(TOUCH) libr3r_shared.pas
endif
endif

clean: _clean
	$(foreach dir,$(TARGET_DIRS),cd $(dir) && $(MAKE) clean && cd ..;)
	$(RM) $(wildcard info.pas)
	$(RM) $(wildcard $(TARGETSHAREDLIBPREFIX)libr3r_shared$(TARGETSHAREDLIBEXT))
	
install: install-header install-lib
	cd po && $(MAKE) install

install-header:
	$(MKDIR) $(incdir)
	$(INSTALL) libr3r.h $(incdir)

install-lib:
	$(MKDIR) $(libdir)
	-$(INSTALLEXE) $(TARGETSHAREDLIBPREFIX)libr3r_shared$(TARGETSHAREDLIBEXT) $(libdir)
	-ldconfig

uninstall:
	cd po && $(MAKE) uninstall
	$(RM) $(wildcard $(libdir)/$(TARGETSHAREDLIBPREFIX)libr3r_shared$(TARGETSHAREDLIBEXT))
	-$(RMDIR) $(wildcard $(libdir))

info.pas: _info.pas ../../scripts/common.make
	$(SED) -e 's/@VERSION@/$(VERSION)/g' \
		-e 's/@CPU@/$(CPU_TARGET)/g' \
		-e 's&@PREFIX@&$(prefix)&g' _info.pas > info.pas
