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
ZLIB_BUILDDIR=$(BUILDDIR)/zlib$(BUILDSUFFIX)

BINLN=$(and $(filter $(BUILD),$(TARGET)), \
        $(if $(findstring mingw,$(BUILD))$(findstring msys,$(BUILD)),,bin))

EXAMPLES1=$(basename $(wildcard *.ml))
EXAMPLES=$(EXAMPLES1) gunzip_s zpipe
TESTS=$(filter test_%,$(EXAMPLES1))

.PHONY: all check interactive clean $(TESTS)

all \
: $(and $(OCAMLC),$(patsubst %,$(BUILDDIR)/%.byte.exe,$(EXAMPLES))) \
  $(and $(OCAMLOPT),$(patsubst %,$(BUILDDIR)/%.opt.exe,$(EXAMPLES))) $(BINLN)

$(BUILDDIR)/%.byte.exe: %.ml $(BUILDDIR)/zlib.cma
	$(OCAMLC) $(strip \
		-c $(OCAMLCFLAGS) -o $(BUILDDIR)/$*.cmo -I $(BUILDDIR) $(OCAML_INCLUDE_FLAGS) \
		$<)
	$(OCAMLC) $(strip \
		$(OCAMLCFLAGS_EXE) -o $@ -I $(BUILDDIR) $(OCAML_INCLUDE_FLAGS) \
		zlib.cma $(BUILDDIR)/$*.cmo)

$(BUILDDIR)/%.opt.exe: %.ml $(BUILDDIR)/zlib.cmxa
	$(OCAMLOPT) $(strip \
		-c $(OCAMLOPTFLAGS) \
		-o $(BUILDDIR)/$*.cmx -I $(BUILDDIR) $(OCAML_INCLUDE_FLAGS) \
		$<)
	$(OCAMLOPT) $(strip \
		$(OCAMLOPTFLAGS_EXE) -o $@ -I $(BUILDDIR) $(OCAML_INCLUDE_FLAGS) \
		zlib.cmxa $(BUILDDIR)/$*.cmx)

$(BUILDDIR)/%.byte.exe: mlzlib/%.ml $(BUILDDIR)/mlzlib.cmo
	$(OCAMLC) $(strip \
		-c $(OCAMLCFLAGS) -o $(BUILDDIR)/$*.cmo -I $(BUILDDIR) $(OCAML_INCLUDE_FLAGS) \
		$<)
	$(OCAMLC) $(strip \
		$(OCAMLCFLAGS_EXE) -o $@ -I $(BUILDDIR) $(OCAML_INCLUDE_FLAGS) \
		unix.cma zlib.cma $(BUILDDIR)/mlzlib.cmo $(BUILDDIR)/$*.cmo)

$(BUILDDIR)/%.opt.exe: mlzlib/%.ml $(BUILDDIR)/mlzlib.cmx
	$(OCAMLOPT) $(strip \
		-c $(OCAMLOPTFLAGS) \
		-o $(BUILDDIR)/$*.cmx -I $(BUILDDIR) $(OCAML_INCLUDE_FLAGS) \
		$<)
	$(OCAMLOPT) $(strip \
		$(OCAMLOPTFLAGS_EXE) -o $@ -I $(BUILDDIR) $(OCAML_INCLUDE_FLAGS) \
		unix.cmxa zlib.cmxa $(BUILDDIR)/mlzlib.cmx $(BUILDDIR)/$*.cmx)

$(BUILDDIR)/gunzip_s.byte.exe $(BUILDDIR)/zpipe.byte.exe \
$(BUILDDIR)/gunzip_s.opt.exe $(BUILDDIR)/zpipe.opt.exe \
: OCAML_INCLUDE_FLAGS+=$(and $(filter 5,$(OCAML_VERSION_MAJOR)),-I +unix)

$(BUILDDIR)/mlzlib.cmo: mlzlib/mlzlib.ml $(BUILDDIR)/zlib.cma
	$(OCAMLC) $(strip \
		-c $(OCAMLCFLAGS) -o $@ -I $(BUILDDIR) $(OCAML_INCLUDE_FLAGS) $<)

$(BUILDDIR)/mlzlib.cmx: mlzlib/mlzlib.ml $(BUILDDIR)/zlib.cmxa
	$(OCAMLOPT) $(strip \
		-c $(OCAMLOPTFLAGS) -o $@ -I $(BUILDDIR) $(OCAML_INCLUDE_FLAGS) $<)

$(and $(OCAMLC),$(BUILDDIR)/zlib.cma) \
$(and $(OCAMLOPT),$(BUILDDIR)/zlib.cmxa) \
&: ../source/zlib*
	$(MAKE) $(strip \
		-C ../source install \
		BUILDDIR=$(abspath $(ZLIB_BUILDDIR)) \
		INSTALLDIR=$(abspath $(BUILDDIR)) \
		ASSERT=$(ASSERT) \
		SUPPORT_COMPARISON=$(SUPPORT_COMPARISON))

$(BINLN):
	ln -s $(BUILDDIR) $@

check: all $(TESTS)

$(TESTS): % \
: $(and $(OCAMLC),$(BUILDDIR)/%.byte.exe) \
  $(and $(OCAMLOPT),$(BUILDDIR)/%.opt.exe)
	$(and $(OCAMLC),$<)
	$(and $(OCAMLOPT),$(BUILDDIR)/$*.opt.exe)

interactive: $(BUILDDIR)/zlib.cma
	$(RLWRAP) ocaml $(strip -I $(BUILDDIR) $(OCAML_INCLUDE_FLAGS) zlib.cma)

clean:
	-$(and $(BINLN),[ -h "$(BINLN)" ] && rm "$(BINLN)")
	-$(MAKE) $(strip \
		-C ../source clean uninstall \
		BUILDDIR=$(abspath $(ZLIB_BUILDDIR)) \
		INSTALLDIR=$(abspath $(BUILDDIR)))
	-rm $(BUILDDIR)/*.exe $(BUILDDIR)/*.cm? $(BUILDDIR)/*.o
	-rmdir $(BUILDDIR)/stublibs
	-rmdir $(BUILDDIR)
