
BOOTDIR ?= boot2
ROOTDIR=../../..
CAMLRUN:=$(ROOTDIR)/boot/ocamlrun

PLUGINDIR:= $(ROOTDIR)/$(BOOTDIR)
PLUGINS=\
  -plugin $(PLUGINDIR)/ocpp-plugin.cma \
  -plugin $(PLUGINDIR)/sempatch-plugin.cma \
  -plugin $(PLUGINDIR)/last-plugin.cma
CAMLC:=$(CAMLRUN) $(PLUGINDIR)/ocamlc $(PLUGINS)
CAMLDEP:=$(CAMLRUN) $(ROOTDIR)/boot/ocamldep
CAMLLEX=$(CAMLRUN) $(ROOTDIR)/boot/ocamllex
CAMLYACC:=$(ROOTDIR)/yacc/ocamlyacc

INCLUDES=-I . -I ../stdlib

COMPFLAGS=$(INCLUDES)

CMIS=asttypes.cmi longident.cmi clflags.cmi docstrings.cmi parser.cmi	\
  config.cmi lexer.cmi parsetree.cmi misc.cmi location.cmi		\
  syntaxerr.cmi warnings.cmi pparse.cmi identifiable.cmi ident.cmi	\
  outcometree.cmi primitive.cmi path.cmi types.cmi cmi_format.cmi	\
  consistbl.cmi tbl.cmi subst.cmi env.cmi lambda.cmi cmo_format.cmi	\
  symtable.cmi memprof.cmi typedtree.cmi

DIFFS=$(CMIS:.cmi=.diff)
MLIS=$(CMIS:.cmi=.mli)

all: $(CMIS)

mlis: $(MLIS)
diffs: $(DIFFS)
	echo $(DIFFS)

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(CAMLC) $(COMPFLAGS) -c $<

.mli.cmi:
	$(CAMLC) $(COMPFLAGS) -c $<
	touch witness

%.mli: ../../../utils/%.mli
	cp -f $< $@

%.mli: ../../../parsing/%.mli
	cp -f $< $@

%.mli: ../../../typing/%.mli
	cp -f $< $@

%.mli: ../../../bytecomp/%.mli
	cp -f $< $@

%.diff: ../../../utils/%.mli
	diff $< `basename $<`

%.diff: ../../../parsing/%.mli
	diff $< `basename $<`

%.diff: ../../../typing/%.mli
	diff $< `basename $<`

%.diff: ../../../bytecomp/%.mli
	diff $< `basename $<`

%.diff: ../../../driver/%.mli
	diff $< `basename $<`

-include .depend

partialclean:
	rm -f *.cm? *.cm?? *~

depend: $(MLIS)
	$(CAMLDEP) $(INCLUDES) *.mli > .depend

