BUILD:=$(shell $(CC) -dumpmachine)
HOST=$(BUILD)
TARGET=$(HOST)

PROGRAMPREFIX=$(addsuffix -,$(filter-out $(BUILD),$(TARGET)))

OCAMLC=$(PROGRAMPREFIX)ocamlc
OCAMLOPT=$(or $(filter-out $(OCAMLC),$(OCAMLC:c=opt)),$(PROGRAMPREFIX)ocamlopt)
RLWRAP?=

OCAMLCFLAGS=
OCAMLOPTFLAGS=$(OCAMLCFLAGS)

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
	$(OCAMLC) -c -o $(BUILDDIR)/$*.cmo -I $(BUILDDIR) $(OCAMLCFLAGS) $<
	$(OCAMLC) -o $@ -I $(BUILDDIR) zlib.cma $(BUILDDIR)/$*.cmo

$(BUILDDIR)/%.opt.exe: %.ml $(BUILDDIR)/zlib.cmxa
	$(OCAMLOPT) -c -o $(BUILDDIR)/$*.cmx -I $(BUILDDIR) $(OCAMLOPTFLAGS) $<
	$(OCAMLOPT) -o $@ -I $(BUILDDIR) zlib.cmxa $(BUILDDIR)/$*.cmx

$(BUILDDIR)/mlzlib.cmo: mlzlib/mlzlib.ml $(BUILDDIR)/zlib.cma
	$(OCAMLC) -c -o $@ -I $(BUILDDIR) $(OCAMLCFLAGS) $<

$(BUILDDIR)/mlzlib.cmx: mlzlib/mlzlib.ml $(BUILDDIR)/zlib.cmxa
	$(OCAMLOPT) -c -o $@ -I $(BUILDDIR) $(OCAMLOPTFLAGS) $<

$(BUILDDIR)/%.byte.exe: mlzlib/%.ml $(BUILDDIR)/mlzlib.cmo
	$(OCAMLC) -c -o $(BUILDDIR)/$*.cmo -I $(BUILDDIR) $(OCAMLCFLAGS) $<
	$(OCAMLC) -o $@ -I $(BUILDDIR) \
		unix.cma zlib.cma $(BUILDDIR)/mlzlib.cmo $(BUILDDIR)/$*.cmo

$(BUILDDIR)/%.opt.exe: mlzlib/%.ml $(BUILDDIR)/mlzlib.cmx
	$(OCAMLOPT) -c -o $(BUILDDIR)/$*.cmx -I $(BUILDDIR) $(OCAMLOPTFLAGS) $<
	$(OCAMLOPT) -o $@ -I $(BUILDDIR) \
		unix.cmxa zlib.cmxa $(BUILDDIR)/mlzlib.cmx $(BUILDDIR)/$*.cmx

check: all $(TESTS)

$(TESTS): %: \
		$(and $(OCAMLC),$(BUILDDIR)/%.byte.exe) \
		$(and $(OCAMLOPT),$(BUILDDIR)/%.opt.exe)
	$(and $(OCAMLC),$<)
	$(and $(OCAMLOPT),$(BUILDDIR)/$*.opt.exe)

interactive: $(BUILDDIR)/zlib.cma
	$(RLWRAP) ocaml -I $(BUILDDIR) $(OCAMLCFLAGS) zlib.cma

$(and $(OCAMLC),$(BUILDDIR)/zlib.cma) \
$(and $(OCAMLOPT),$(BUILDDIR)/zlib.cmxa)&: \
		../source/zlib*
	$(MAKE) -C ../source install \
		BUILDDIR=$(abspath $(ZLIB_BUILDDIR)) \
		INSTALLDIR=$(abspath $(BUILDDIR))

$(BINLN):
	ln -s $(BUILDDIR) $@

clean:
	-$(and $(BINLN),[ -h "$(BINLN)" ] && rm "$(BINLN)")
	-$(MAKE) -C ../source clean uninstall \
		BUILDDIR=$(abspath $(ZLIB_BUILDDIR)) \
		INSTALLDIR=$(abspath $(BUILDDIR))
	-rm $(BUILDDIR)/*.exe $(BUILDDIR)/*.cm? $(BUILDDIR)/*.o
	-rmdir $(BUILDDIR)/stublibs
	-rmdir $(BUILDDIR)
