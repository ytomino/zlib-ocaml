HOST:=$(shell $(CC) -dumpmachine)
TARGET=$(HOST)

ifneq ($(TARGET),$(HOST))
 PROGRAMPREFIX=$(TARGET)-
 BINLN=
else
 PROGRAMPREFIX=
 ifneq ($(findstring mingw,$(HOST))$(findstring msys,$(HOST)),)
  BINLN=
 else
  BINLN=bin
 endif
endif

OCAMLC=$(PROGRAMPREFIX)ocamlc
OCAMLOPT=$(if $(OCAMLC),$(OCAMLC:c=opt),$(PROGRAMPREFIX)ocamlopt)
RLWRAP?=

OCAMLCFLAGS=-unsafe-string

BUILDSUFFIX=.noindex
BUILDDIR=$(TARGET)$(BUILDSUFFIX)
ZLIB_BUILDDIR=$(BUILDDIR)/zlib$(BUILDSUFFIX)

EXAMPLES=$(basename $(wildcard *.ml))
TESTS=$(filter test%,$(EXAMPLES))

.PHONY: all check interacitve clean $(TESTS)

all: \
	$(if $(OCAMLC),$(patsubst %,$(BUILDDIR)/%.byte.exe,$(EXAMPLES))) \
	$(if $(OCAMLOPT),$(patsubst %,$(BUILDDIR)/%.opt.exe,$(EXAMPLES))) \
	$(BINLN)

$(BUILDDIR)/%.byte.exe: %.ml $(BUILDDIR)/zlib.cma
	$(OCAMLC) -c -o $(BUILDDIR)/$*.cmo -I $(BUILDDIR) $(OCAMLCFLAGS) $<
	$(OCAMLC) -o $@ -I $(BUILDDIR) zlib.cma $(BUILDDIR)/$*.cmo

$(BUILDDIR)/%.opt.exe: %.ml $(BUILDDIR)/zlib.cmxa
	$(OCAMLOPT) -c -o $(BUILDDIR)/$*.cmx -I $(BUILDDIR) $(OCAMLCFLAGS) $<
	$(OCAMLOPT) -o $@ -I $(BUILDDIR) zlib.cmxa $(BUILDDIR)/$*.cmx

check: all $(TESTS)

$(TESTS): %: \
		$(if $(OCAMLC),$(BUILDDIR)/%.byte.exe) $(if $(OCAMLOPT),$(BUILDDIR)/%.opt.exe)
ifneq ($(OCAMLC),)
	$<
endif
ifneq ($(OCAMLOPT),)
	$(patsubst %.byte.exe,%.opt.exe,$<)
endif

interactive: $(BUILDDIR)/zlib.cma
	$(RLWRAP) ocaml -I $(BUILDDIR) $(OCAMLCFLAGS) zlib.cma

$(BUILDDIR)/zlib.cma $(BUILDDIR)/zlib.cmxa&: $(wildcard ../source/zlib*)
	$(MAKE) -C ../source install \
		BUILDDIR=$(abspath $(ZLIB_BUILDDIR)) \
		INSTALLDIR=$(abspath $(BUILDDIR))

$(BINLN):
	ln -s $(BUILDDIR) $@

clean:
	-$(if $(BINLN),[ -h "$(BINLN)" ] && rm "$(BINLN)")
	-$(MAKE) -C ../source clean uninstall \
		BUILDDIR=$(abspath $(ZLIB_BUILDDIR)) \
		INSTALLDIR=$(abspath $(BUILDDIR))
	-rm $(BUILDDIR)/*.exe $(BUILDDIR)/*.cm? $(BUILDDIR)/*.o
	-rmdir $(BUILDDIR)/stublibs
	-rmdir $(BUILDDIR)
