
ROOTDIR=../../..
CAMLRUN:=$(ROOTDIR)/boot/ocamlrun
CAMLC:=$(CAMLRUN) $(ROOTDIR)/$(BOOTDIR)/ocamlc
CAMLDEP:=$(CAMLRUN) $(ROOTDIR)/boot/ocamldep
CAMLLEX=$(CAMLRUN) $(ROOTDIR)/boot/ocamllex
CAMLYACC:=$(ROOTDIR)/yacc/ocamlyacc

INCLUDES=-I ../stdlib

COMPFLAGS=$(INCLUDES)

CMIS=asttypes.cmi longident.cmi clflags.cmi docstrings.cmi parser.cmi	\
  config.cmi lexer.cmi parsetree.cmi misc.cmi location.cmi		\
  syntaxerr.cmi warnings.cmi pparse.cmi identifiable.cmi ident.cmi	\
  outcometree.cmi primitive.cmi path.cmi types.cmi cmi_format.cmi	\
  consistbl.cmi tbl.cmi subst.cmi env.cmi lambda.cmi cmo_format.cmi	\
  symtable.cmi

all: $(CMIS)

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

-include .depend

partialclean:
	rm -f *.cm? *.cm?? *~

depend:
	$(CAMLDEP) $(INCLUDES) *.mli > .depend

