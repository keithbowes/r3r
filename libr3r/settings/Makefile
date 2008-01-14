SHELL = sh

%$(OBJEXT): %.pas
	$(PC) $(PCFLAGS) $<

all: rsettings_routines$(OBJEXT) rsettings$(OBJEXT)

clean:
	rm -f *$(OBJEXT) *$(UNITEXT)

#rsettings$(OBJEXT): rsettings.pas
#	$(PC) $(PCFLAGS) $<
