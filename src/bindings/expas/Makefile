.PHONY: unit program

.SUFFIXES:
.SUFFIXES: $(PPUEXT) .pas

sinclude common.make

all: unit program

unit: expas$(PPUEXT)

program: example$(PPUEXT)

clean:
	$(DEL) common.make

ifndef VERSION
clean:
	$(DEL) $(wildcard *$(OEXT) *$(PPUEXT))
	$(DEL) example$(EXEEXT)

.pas$(PPUEXT):
	$(FPC) $<
endif
