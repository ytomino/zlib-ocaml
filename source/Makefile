include Makefile.variables

MLI=zlib.mli
MLSRC=zlib.ml
MLINIT=
CSRC=zlib_stub.c

LIBNAME=zlib
STUBLIBNAME=ml$(LIBNAME)

WIFLAGS=
WLFLAGS=
ifneq ($(WITH_ZLIB),)
 WIFLAGS+=-I$(WITH_ZLIB)/include
 WLFLAGS+=-L$(WITH_ZLIB)/lib
endif

OCAMLCFLAGS=-unsafe-string -w +a
CFLAGS=$(WIFLAGS) -Wall -Wextra -Wno-unused-result

CCLIB=$(addprefix -cclib ,$(WLFLAGS) -lz)
LDFLAGS=$(WLFLAGS)
LDLIB=-lz

include Makefile.rules
