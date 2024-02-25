BUILD:=$(shell $(CC) -dumpmachine)
HOST=$(BUILD)
TARGET=$(HOST)

PROGRAMPREFIX=$(addsuffix -,$(filter-out $(BUILD),$(TARGET)))

OCAMLC=$(PROGRAMPREFIX)ocamlc
OCAMLOPT=$(or $(filter-out $(OCAMLC),$(OCAMLC:c=opt)),$(PROGRAMPREFIX)ocamlopt)
RLWRAP?=

DEBUG=

OCAMLCFLAGS=$(and $(filter-out 0,$(DEBUG)),-g)
OCAMLOPTFLAGS=$(OCAMLCFLAGS)
OCAMLCFLAGS_EXE=$(and $(filter-out 0,$(DEBUG)),-g)
OCAMLOPTFLAGS_EXE=$(OCAMLCFLAGS_EXE) $(addprefix -ccopt ,$(LDFLAGS))
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
	$(OCAMLC) -c $(OCAMLCFLAGS) -o $(BUILDDIR)/$*.cmo -I $(BUILDDIR) $<
	$(OCAMLC) $(strip \
		$(OCAMLCFLAGS_EXE) \
		-o $@ -I $(BUILDDIR) zlib.cma $(BUILDDIR)/$*.cmo)

$(BUILDDIR)/%.opt.exe: %.ml $(BUILDDIR)/zlib.cmxa
	$(OCAMLOPT) -c $(OCAMLOPTFLAGS) -o $(BUILDDIR)/$*.cmx -I $(BUILDDIR) $<
	$(OCAMLOPT) $(strip \
		$(OCAMLOPTFLAGS_EXE) \
		-o $@ -I $(BUILDDIR) zlib.cmxa $(BUILDDIR)/$*.cmx)

$(BUILDDIR)/mlzlib.cmo: mlzlib/mlzlib.ml $(BUILDDIR)/zlib.cma
	$(OCAMLC) -c $(OCAMLCFLAGS) -o $@ -I $(BUILDDIR) $<

$(BUILDDIR)/mlzlib.cmx: mlzlib/mlzlib.ml $(BUILDDIR)/zlib.cmxa
	$(OCAMLOPT) -c $(OCAMLOPTFLAGS) -o $@ -I $(BUILDDIR) $<

$(BUILDDIR)/%.byte.exe: mlzlib/%.ml $(BUILDDIR)/mlzlib.cmo
	$(OCAMLC) -c $(OCAMLCFLAGS) -o $(BUILDDIR)/$*.cmo -I $(BUILDDIR) $<
	$(OCAMLC) $(strip \
		$(OCAMLCFLAGS_EXE) \
		-o $@ -I $(BUILDDIR) unix.cma zlib.cma \
		$(BUILDDIR)/mlzlib.cmo $(BUILDDIR)/$*.cmo)

$(BUILDDIR)/%.opt.exe: mlzlib/%.ml $(BUILDDIR)/mlzlib.cmx
	$(OCAMLOPT) -c $(OCAMLOPTFLAGS) -o $(BUILDDIR)/$*.cmx -I $(BUILDDIR) $<
	$(OCAMLOPT) $(strip \
		$(OCAMLOPTFLAGS_EXE) \
		-o $@ -I $(BUILDDIR) unix.cmxa zlib.cmxa \
		$(BUILDDIR)/mlzlib.cmx $(BUILDDIR)/$*.cmx)

check: all $(TESTS)

$(TESTS): %: \
		$(and $(OCAMLC),$(BUILDDIR)/%.byte.exe) \
		$(and $(OCAMLOPT),$(BUILDDIR)/%.opt.exe)
	$(and $(OCAMLC),$<)
	$(and $(OCAMLOPT),$(BUILDDIR)/$*.opt.exe)

interactive: $(BUILDDIR)/zlib.cma
	$(RLWRAP) ocaml $(OCAMLCFLAGS) -I $(BUILDDIR) zlib.cma

$(and $(OCAMLC),$(BUILDDIR)/zlib.cma) \
$(and $(OCAMLOPT),$(BUILDDIR)/zlib.cmxa)&: \
		../source/zlib*
	$(MAKE) $(strip \
		-C ../source install \
		BUILDDIR=$(abspath $(ZLIB_BUILDDIR)) \
		INSTALLDIR=$(abspath $(BUILDDIR)))

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
