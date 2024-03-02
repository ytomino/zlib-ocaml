include Makefile.variables

MLI=zlib.mli
MLSRC=zlib.ml
MLINIT=
CSRC=zlib_stub.c

LIBNAME=zlib
STUBLIBNAME=ml$(LIBNAME)

DEBUG=
ASSERT=

WITH_ZLIB?=
WIFLAGS=$(and $(WITH_ZLIB),-I$(WITH_ZLIB)/include)
WLFLAGS=$(and $(WITH_ZLIB),-L$(WITH_ZLIB)/lib)

OCAMLCFLAGS=$(and $(filter-out 0,$(DEBUG)),-g) $(if $(ASSERT),,-noassert) -w +a
OCAMLOPTFLAGS=$(OCAMLCFLAGS)
CFLAGS=$(and $(filter-out 0,$(DEBUG)),-g$(DEBUG)) \
       $(and $(ASSERT),-DDEBUG -DNDEBUG) -Wall -Wextra -Wno-unused-result \
       $(WIFLAGS)

CCLIB=$(addprefix -cclib ,$(WLFLAGS) -lz)
LDFLAGS=$(WLFLAGS)
LDLIB=-lz

include Makefile.rules
