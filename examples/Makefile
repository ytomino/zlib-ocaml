BUILD:=$(shell $(CC) -dumpmachine)
HOST=$(BUILD)
TARGET=$(HOST)

PROGRAMPREFIX=$(addsuffix -,$(filter-out $(BUILD),$(TARGET)))

OCAMLC=$(PROGRAMPREFIX)ocamlc
OCAMLOPT=$(or $(filter-out $(OCAMLC),$(OCAMLC:c=opt)),$(PROGRAMPREFIX)ocamlopt)
RLWRAP?=

DEBUG=
ASSERT=1

OCAMLCFLAGS=$(and $(filter-out 0,$(DEBUG)),-g)
OCAMLOPTFLAGS=$(OCAMLCFLAGS)
OCAMLCFLAGS_EXE=$(and $(filter-out 0,$(DEBUG)),-g)
OCAMLOPTFLAGS_EXE=$(OCAMLCFLAGS_EXE) $(addprefix -ccopt ,$(LDFLAGS))
OCAML_INCLUDE_FLAGS=
LDFLAGS?=

BUILDSUFFIX=.noindex
BUILDDIR=$(TARGET)$(BUILDSUFFIX)
ZLIB_BUILDDIR=$(BUILDDIR)/zlib$(BUILDSUFFIX)

BINLN=$(and $(filter $(BUILD),$(TARGET)), \
        $(if $(findstring mingw,$(BUILD))$(findstring msys,$(BUILD)),,bin))

EXAMPLES1=$(basename $(wildcard *.ml))
EXAMPLES=$(EXAMPLES1) gunzip_s zpipe
TESTS=$(filter test_%,$(EXAMPLES1))

.PHONY: all check interacitve clean $(TESTS)

all: \
	$(and $(OCAMLC),$(patsubst %,$(BUILDDIR)/%.byte.exe,$(EXAMPLES))) \
	$(and $(OCAMLOPT),$(patsubst %,$(BUILDDIR)/%.opt.exe,$(EXAMPLES))) \
	$(BINLN)

$(BUILDDIR)/%.byte.exe: %.ml $(BUILDDIR)/zlib.cma
	$(OCAMLC) $(strip \
		-c $(OCAMLCFLAGS) \
		-o $(BUILDDIR)/$*.cmo $(OCAML_INCLUDE_FLAGS) -I $(BUILDDIR) $<)
	$(OCAMLC) $(strip \
		$(OCAMLCFLAGS_EXE) \
		-o $@ $(OCAML_INCLUDE_FLAGS) -I $(BUILDDIR) zlib.cma $(BUILDDIR)/$*.cmo)

$(BUILDDIR)/%.opt.exe: %.ml $(BUILDDIR)/zlib.cmxa
	$(OCAMLOPT) $(strip \
		-c $(OCAMLOPTFLAGS) \
		-o $(BUILDDIR)/$*.cmx $(OCAML_INCLUDE_FLAGS) -I $(BUILDDIR) $<)
	$(OCAMLOPT) $(strip \
		$(OCAMLOPTFLAGS_EXE) \
		-o $@ $(OCAML_INCLUDE_FLAGS) -I $(BUILDDIR) zlib.cmxa $(BUILDDIR)/$*.cmx)

$(BUILDDIR)/mlzlib.cmo: mlzlib/mlzlib.ml $(BUILDDIR)/zlib.cma
	$(OCAMLC) $(strip \
		-c $(OCAMLCFLAGS) -o $@ $(OCAML_INCLUDE_FLAGS) -I $(BUILDDIR) $<)

$(BUILDDIR)/mlzlib.cmx: mlzlib/mlzlib.ml $(BUILDDIR)/zlib.cmxa
	$(OCAMLOPT) $(strip \
		-c $(OCAMLOPTFLAGS) -o $@ $(OCAML_INCLUDE_FLAGS) -I $(BUILDDIR) $<)

$(BUILDDIR)/%.byte.exe: mlzlib/%.ml $(BUILDDIR)/mlzlib.cmo
	$(OCAMLC) $(strip \
		-c $(OCAMLCFLAGS) \
		-o $(BUILDDIR)/$*.cmo $(OCAML_INCLUDE_FLAGS) -I $(BUILDDIR) $<)
	$(OCAMLC) $(strip \
		$(OCAMLCFLAGS_EXE) \
		-o $@ $(OCAML_INCLUDE_FLAGS) unix.cma -I $(BUILDDIR) zlib.cma \
		$(BUILDDIR)/mlzlib.cmo $(BUILDDIR)/$*.cmo)

$(BUILDDIR)/%.opt.exe: mlzlib/%.ml $(BUILDDIR)/mlzlib.cmx
	$(OCAMLOPT) $(strip \
		-c $(OCAMLOPTFLAGS) \
		-o $(BUILDDIR)/$*.cmx $(OCAML_INCLUDE_FLAGS) -I $(BUILDDIR) $<)
	$(OCAMLOPT) $(strip \
		$(OCAMLOPTFLAGS_EXE) \
		-o $@ $(OCAML_INCLUDE_FLAGS) unix.cmxa -I $(BUILDDIR) zlib.cmxa \
		$(BUILDDIR)/mlzlib.cmx $(BUILDDIR)/$*.cmx)

check: all $(TESTS)

$(TESTS): %: \
		$(and $(OCAMLC),$(BUILDDIR)/%.byte.exe) \
		$(and $(OCAMLOPT),$(BUILDDIR)/%.opt.exe)
	$(and $(OCAMLC),$<)
	$(and $(OCAMLOPT),$(BUILDDIR)/$*.opt.exe)

interactive: $(BUILDDIR)/zlib.cma
	$(RLWRAP) ocaml $(strip \
		$(OCAMLCFLAGS) $(OCAML_INCLUDE_FLAGS) -I $(BUILDDIR) zlib.cma)

$(and $(OCAMLC),$(BUILDDIR)/zlib.cma) \
$(and $(OCAMLOPT),$(BUILDDIR)/zlib.cmxa)&: \
		../source/zlib*
	$(MAKE) $(strip \
		-C ../source install \
		BUILDDIR=$(abspath $(ZLIB_BUILDDIR)) \
		INSTALLDIR=$(abspath $(BUILDDIR)) \
		ASSERT=$(ASSERT))

$(BINLN):
	ln -s $(BUILDDIR) $@

clean:
	-$(and $(BINLN),[ -h "$(BINLN)" ] && rm "$(BINLN)")
	-$(MAKE) $(strip \
		-C ../source clean uninstall \
		BUILDDIR=$(abspath $(ZLIB_BUILDDIR)) \
		INSTALLDIR=$(abspath $(BUILDDIR)))
	-rm $(BUILDDIR)/*.exe $(BUILDDIR)/*.cm? $(BUILDDIR)/*.o
	-rmdir $(BUILDDIR)/stublibs
	-rmdir $(BUILDDIR)
