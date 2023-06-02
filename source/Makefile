include Makefile.variables

MLI=zlib.mli
MLSRC=zlib.ml
MLINIT=
CSRC=zlib_stub.c

LIBNAME=zlib
STUBLIBNAME=ml$(LIBNAME)

WITH_ZLIB?=
WIFLAGS=$(and $(WITH_ZLIB),-I$(WITH_ZLIB)/include)
WLFLAGS=$(and $(WITH_ZLIB),-L$(WITH_ZLIB)/lib)

OCAMLCFLAGS=-w +a
OCAMLOPTFLAGS=$(OCAMLCFLAGS)
CFLAGS=$(WIFLAGS) -Wall -Wextra -Wno-unused-result

CCLIB=$(addprefix -cclib ,$(WLFLAGS) -lz)
LDFLAGS=$(WLFLAGS)
LDLIB=-lz

include Makefile.rules
