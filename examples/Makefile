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

.PHONY: all world check interactive clean $(TESTS)

all \
: $(and $(OCAMLC),$(patsubst %,$(BUILDDIR)/%.byte.exe,$(EXAMPLES))) \
  $(and $(OCAMLOPT),$(patsubst %,$(BUILDDIR)/%.opt.exe,$(EXAMPLES))) $(BINLN)

$(BUILDDIR)/%.byte.exe: %.ml $(INSTALLDIR)/zlib.cma | $(BUILDDIR)
	$(OCAMLC) $(strip \
		-c $(OCAMLCFLAGS) \
		-o $(BUILDDIR)/$*.cmo $(OCAML_BUILDDIR_FLAGS) $(OCAML_INCLUDE_FLAGS) \
		$<)
	$(OCAMLC) $(strip \
		$(OCAMLCFLAGS_EXE) -o $@ $(OCAML_BUILDDIR_FLAGS) $(OCAML_INCLUDE_FLAGS) \
		$(notdir $(filter-out $<,$^)) $*.cmo)

$(BUILDDIR)/%.opt.exe: %.ml $(INSTALLDIR)/zlib.cmxa | $(BUILDDIR)
	$(OCAMLOPT) $(strip \
		-c $(OCAMLOPTFLAGS) \
		-o $(BUILDDIR)/$*.cmx $(OCAML_BUILDDIR_FLAGS) $(OCAML_INCLUDE_FLAGS) \
		$<)
	$(OCAMLOPT) $(strip \
		$(OCAMLOPTFLAGS_EXE) -o $@ $(OCAML_BUILDDIR_FLAGS) $(OCAML_INCLUDE_FLAGS) \
		$(notdir $(filter-out $<,$^)) $*.cmx)

$(BUILDDIR)/%.byte.exe \
: mlzlib/%.ml $(INSTALLDIR)/zlib.cma $(BUILDDIR)/mlzlib.cmo
	$(OCAMLC) $(strip \
		-c $(OCAMLCFLAGS) \
		-o $(BUILDDIR)/$*.cmo $(OCAML_BUILDDIR_FLAGS) $(OCAML_INCLUDE_FLAGS) \
		$<)
	$(OCAMLC) $(strip \
		$(OCAMLCFLAGS_EXE) -o $@ $(OCAML_BUILDDIR_FLAGS) $(OCAML_INCLUDE_FLAGS) \
		unix.cma $(notdir $(filter-out $<,$^)) $*.cmo)

$(BUILDDIR)/%.opt.exe \
: mlzlib/%.ml $(INSTALLDIR)/zlib.cmxa $(BUILDDIR)/mlzlib.cmx
	$(OCAMLOPT) $(strip \
		-c $(OCAMLOPTFLAGS) \
		-o $(BUILDDIR)/$*.cmx $(OCAML_BUILDDIR_FLAGS) $(OCAML_INCLUDE_FLAGS) \
		$<)
	$(OCAMLOPT) $(strip \
		$(OCAMLOPTFLAGS_EXE) -o $@ $(OCAML_BUILDDIR_FLAGS) $(OCAML_INCLUDE_FLAGS) \
		unix.cmxa $(notdir $(filter-out $<,$^)) $*.cmx)

$(BUILDDIR)/gunzip_s.byte.exe $(BUILDDIR)/zpipe.byte.exe \
$(BUILDDIR)/gunzip_s.opt.exe $(BUILDDIR)/zpipe.opt.exe \
: OCAML_INCLUDE_FLAGS+=$(and $(filter 5,$(OCAML_VERSION_MAJOR)),-I +unix)

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
		$(OCAML_BUILDDIR_FLAGS) $(OCAML_INCLUDE_FLAGS) $(notdir $^))

clean:
	-$(and $(BINLN),[ -h "$(BINLN)" ] && rm "$(BINLN)")
	-$(MAKE) $(strip \
		-C ../source clean $(and $(filter $(BUILDDIR),$(INSTALLDIR)),uninstall) \
		BUILDDIR=$(abspath $(ZLIB_BUILDDIR)) \
		$(addprefix INSTALLDIR=,$(abspath $(filter $(BUILDDIR),$(INSTALLDIR)))))
	-rm $(BUILDDIR)/*.exe $(BUILDDIR)/*.cm? $(BUILDDIR)/*.o
	-$(patsubst %,rmdir %/stublibs,$(filter $(BUILDDIR),$(INSTALLDIR)))
	-rmdir $(BUILDDIR)
