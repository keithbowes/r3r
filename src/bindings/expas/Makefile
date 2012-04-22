.PHONY: unit program

.SUFFIXES:
.SUFFIXES: $(PPUEXT) .pas

sinclude common.make

all: unit program

unit: expas$(PPUEXT)

program: example$(PPUEXT)

clean:

ifndef VERSION
.pas$(PPUEXT):
	$(PC) -o$@ $<
endif
