include ../../../scripts/common.make

ifneq ($(findstring gcc,$(CC)),)
override DEFS_EXTRA+=USE_GCC
override PCFLAGS_EXTRA+=-k-L$(dir $(shell $(CC) -print-libgcc-file-name))
endif

ifeq ($(findstring LINK_DYNAMIC,$(DEFS_EXTRA)),)
ifdef USE_FPC
comma = ,

CURL_CONFIG = $(call programpath,curl-config)
CURL_CONFIGFLAGS = --static-libs

CURL_LIBS = $(addprefix -k,$(subst -Wl$(comma),,$(shell $(CURL_CONFIG) $(CURL_CONFIGFLAGS))))
override PCFLAGS_EXTRA+=$(CURL_LIBS)
endif
endif

all: curlslist$(PPUEXT) curlcore$(PPUEXT) curleasy$(PPUEXT) curlver$(PPUEXT)
