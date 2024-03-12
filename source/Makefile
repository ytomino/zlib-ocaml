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

OCAMLCFLAGS=$(and $(filter-out 0,$(DEBUG)),-g) $(if $(ASSERT),,-noassert) -w +a
OCAMLOPTFLAGS=$(OCAMLCFLAGS)
OCAML_INCLUDE_FLAGS=
CFLAGS=$(and $(filter-out 0,$(DEBUG)),-g$(DEBUG)) \
       $(and $(ASSERT),-DDEBUG -DNDEBUG) -Wall -Wextra -Wno-unused-result
C_INCLUDE_FLAGS=$(and $(WITH_ZLIB),-I$(WITH_ZLIB)/include)

LD_ZLIB=$(and $(WITH_ZLIB),-L$(WITH_ZLIB)/lib) -lz

CCLIB=$(addprefix -cclib ,$(LD_ZLIB))
LDLIB=$(LD_ZLIB)

include Makefile.rules
