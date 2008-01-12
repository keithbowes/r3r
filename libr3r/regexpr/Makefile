SHELL = sh

RM = rm -fr

all: RegExpr$(OBJEXT)

clean:
	rm -f *$(OBJEXT) *$(UNITEXT)

RegExpr$(OBJEXT): RegExpr.pas
	$(PC) $(PCFLAGS) $<
