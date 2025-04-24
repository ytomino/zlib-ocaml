BUILD:=$(shell $(CC) -dumpmachine)
HOST=$(BUILD)
TARGET=$(HOST)

PROGRAMPREFIX=$(addsuffix -,$(filter-out $(BUILD),$(TARGET)))

OCAMLC=$(PROGRAMPREFIX)ocamlc
OCAMLOPT=$(or $(filter-out $(OCAMLC),$(OCAMLC:c=opt)),$(PROGRAMPREFIX)ocamlopt)
RLWRAP?=

ifndef OCAML_VERSION_MAJOR
 OCAML_VERSION_MAJOR:=$(basename \
                        $(basename \
                          $(shell $(or $(OCAMLC),$(OCAMLOPT)) -version)))
endif

DEBUG=
ASSERT=1
SUPPORT_COMPARISON=1

OCAMLCFLAGS=$(and $(filter-out 0,$(DEBUG)),-g)
OCAMLOPTFLAGS=$(OCAMLCFLAGS)
OCAMLCFLAGS_EXE=$(and $(filter-out 0,$(DEBUG)),-g)
OCAMLOPTFLAGS_EXE=$(OCAMLCFLAGS_EXE) $(addprefix -ccopt ,$(LDFLAGS))
OCAML_INCLUDE_FLAGS=
LDFLAGS?=

BYTELIBS=
OPTLIBS=

BUILDSUFFIX=.noindex
BUILDDIR=$(TARGET)$(BUILDSUFFIX)
INSTALLDIR=$(BUILDDIR)

OCAML_BUILDDIR_FLAGS=$(addprefix -I , \
                       $(BUILDDIR) \
                       $(filter-out $(BUILDDIR),$(INSTALLDIR)))

ZLIB_BUILDDIR=$(BUILDDIR)/zlib$(BUILDSUFFIX)
ZLIB_WORLD=$(and $(OCAMLC),$(INSTALLDIR)/zlib.cma) \
           $(and $(OCAMLOPT),$(INSTALLDIR)/zlib.cmxa)

BINLN=$(and $(filter $(BUILD),$(TARGET)), \
        $(if $(findstring mingw,$(BUILD))$(findstring msys,$(BUILD)),,bin))

EXAMPLES1=$(basename $(wildcard *.ml))
EXAMPLES=$(EXAMPLES1) gunzip_s zpipe
TESTS=$(filter test_%,$(EXAMPLES1))

EXAMPLES_BYTE=$(and $(OCAMLC),$(patsubst %,$(BUILDDIR)/%.byte.exe,$(EXAMPLES)))
EXAMPLES_OPT=$(and $(OCAMLOPT),$(patsubst %,$(BUILDDIR)/%.opt.exe,$(EXAMPLES)))

.PHONY: all world check interactive clean $(TESTS)

all: $(EXAMPLES_BYTE) $(EXAMPLES_OPT) $(BINLN)

$(EXAMPLES_BYTE): $(BUILDDIR)/%.byte.exe:
	$(OCAMLC) $(strip \
		-c $(OCAMLCFLAGS) \
		-o $(BUILDDIR)/$*.cmo $(OCAML_BUILDDIR_FLAGS) $(OCAML_INCLUDE_FLAGS) \
		$<)
	$(OCAMLC) $(strip \
		$(OCAMLCFLAGS_EXE) -o $@ $(OCAML_BUILDDIR_FLAGS) $(OCAML_INCLUDE_FLAGS) \
		$(BYTELIBS) $(notdir $(filter-out $<,$^)) $*.cmo)

$(EXAMPLES_OPT): $(BUILDDIR)/%.opt.exe:
	$(OCAMLOPT) $(strip \
		-c $(OCAMLOPTFLAGS) \
		-o $(BUILDDIR)/$*.cmx $(OCAML_BUILDDIR_FLAGS) $(OCAML_INCLUDE_FLAGS) \
		$<)
	$(OCAMLOPT) $(strip \
		$(OCAMLOPTFLAGS_EXE) -o $@ $(OCAML_BUILDDIR_FLAGS) $(OCAML_INCLUDE_FLAGS) \
		$(OPTLIBS) $(notdir $(filter-out $<,$^)) $*.cmx)

$(patsubst %,$(BUILDDIR)/%.byte.exe,$(EXAMPLES1)): $(BUILDDIR)/%.byte.exe \
: %.ml $(INSTALLDIR)/zlib.cma | $(BUILDDIR)
$(patsubst %,$(BUILDDIR)/%.opt.exe,$(EXAMPLES1)): $(BUILDDIR)/%.opt.exe \
: %.ml $(INSTALLDIR)/zlib.cmxa | $(BUILDDIR)

$(BUILDDIR)/gunzip_s.byte.exe $(BUILDDIR)/zpipe.byte.exe \
: $(BUILDDIR)/%.byte.exe \
: mlzlib/%.ml $(INSTALLDIR)/zlib.cma $(BUILDDIR)/mlzlib.cmo
$(BUILDDIR)/gunzip_s.opt.exe $(BUILDDIR)/zpipe.opt.exe: $(BUILDDIR)/%.opt.exe \
: mlzlib/%.ml $(INSTALLDIR)/zlib.cmxa $(BUILDDIR)/mlzlib.cmx

$(BUILDDIR)/gunzip_s.byte.exe $(BUILDDIR)/zpipe.byte.exe \
$(BUILDDIR)/gunzip_s.opt.exe $(BUILDDIR)/zpipe.opt.exe \
: private override OCAML_INCLUDE_FLAGS+=$(and \
                                          $(filter 5,$(OCAML_VERSION_MAJOR)), \
                                          -I +unix)

$(BUILDDIR)/gunzip_s.byte.exe $(BUILDDIR)/zpipe.byte.exe \
: private override BYTELIBS+=unix.cma
$(BUILDDIR)/gunzip_s.opt.exe $(BUILDDIR)/zpipe.opt.exe \
: private override OPTLIBS+=unix.cmxa

$(BUILDDIR)/mlzlib.cmo: mlzlib/mlzlib.ml $(INSTALLDIR)/zlib.cma | $(BUILDDIR)
	$(OCAMLC) $(strip \
		-c $(OCAMLCFLAGS) -o $@ $(OCAML_BUILDDIR_FLAGS) $(OCAML_INCLUDE_FLAGS) $<)

$(BUILDDIR)/mlzlib.cmx: mlzlib/mlzlib.ml $(INSTALLDIR)/zlib.cmxa | $(BUILDDIR)
	$(OCAMLOPT) $(strip \
		-c $(OCAMLOPTFLAGS) -o $@ $(OCAML_BUILDDIR_FLAGS) $(OCAML_INCLUDE_FLAGS) $<)

$(ZLIB_WORLD)&: ../source/zlib*
	$(MAKE) $(strip \
		-C ../source install \
		BUILDDIR=$(abspath $(ZLIB_BUILDDIR)) \
		INSTALLDIR=$(abspath $(INSTALLDIR)) \
		ASSERT=$(ASSERT) \
		SUPPORT_COMPARISON=$(SUPPORT_COMPARISON))

$(BUILDDIR):
	mkdir -p $@

$(BINLN):
	ln -s $(BUILDDIR) $@

world: $(ZLIB_WORLD)

check: all $(TESTS)

$(TESTS): % \
: $(and $(OCAMLC),$(BUILDDIR)/%.byte.exe) \
  $(and $(OCAMLOPT),$(BUILDDIR)/%.opt.exe)
	$(and $(OCAMLC),$<)
	$(and $(OCAMLOPT),$(BUILDDIR)/$*.opt.exe)

interactive: $(INSTALLDIR)/zlib.cma
	$(RLWRAP) ocaml $(strip \
		$(OCAML_BUILDDIR_FLAGS) $(OCAML_INCLUDE_FLAGS) $(BYTELIBS) $(notdir $^))

clean:
	-$(and $(BINLN),[ -h "$(BINLN)" ] && rm "$(BINLN)")
	-$(MAKE) $(strip \
		-C ../source clean $(and $(filter $(BUILDDIR),$(INSTALLDIR)),uninstall) \
		BUILDDIR=$(abspath $(ZLIB_BUILDDIR)) \
		$(addprefix INSTALLDIR=,$(abspath $(filter $(BUILDDIR),$(INSTALLDIR)))))
	-rm $(BUILDDIR)/*.exe $(BUILDDIR)/*.cm? $(BUILDDIR)/*.o
	-$(patsubst %,rmdir %/stublibs,$(filter $(BUILDDIR),$(INSTALLDIR)))
	-rmdir $(BUILDDIR)
