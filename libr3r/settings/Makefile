SHELL = sh

all: rsettings$(OBJEXT)

clean:
	rm -f *$(OBJEXT) *$(UNITEXT)

rsettings$(OBJEXT): rsettings.pas
	$(PC) $(PCFLAGS) $<
