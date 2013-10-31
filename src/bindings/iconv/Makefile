include ../../../scripts/common.make

UNIT_DIRS = ../../utils/*

all: unit prog

prog: test$(PPUEXT)
	
unit: iconv$(PPUEXT)

clean:
	$(RM) $(wildcard test$(TARGETEXEEXT))
