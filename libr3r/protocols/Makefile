.POSIX: 

.SUFFIXES: 
.SUFFIXES: $(OBJEXT) .pas

formatdeps: sockconsts$(OBJEXT)

all: rsock$(OBJEXT) localfile$(OBJEXT) headers$(OBJEXT) http$(OBJEXT)

clean:
	rm -f *$(OBJEXT) *$(UNITEXT)

.pas$(OBJEXT):
	$(PC) $(PCFLAGS) $(UNITDIR).. $(UNITDIR)../formats $(UNITDIR)../regexpr $<
