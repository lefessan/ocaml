
ROOTDIR=../../..
CAMLRUN:=$(ROOTDIR)/boot/ocamlrun
CAMLC:=$(CAMLRUN) $(ROOTDIR)/$(BOOTDIR)/ocamlc
CAMLDEP:=$(CAMLRUN) $(ROOTDIR)/boot/ocamldep
CAMLLEX=$(CAMLRUN) $(ROOTDIR)/boot/ocamllex
CAMLYACC:=$(ROOTDIR)/yacc/ocamlyacc

INCLUDES=-I ../stdlib
COMPFLAGS=$(INCLUDES)

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(CAMLC) $(COMPFLAGS) -c $<

.mli.cmi:
	$(CAMLC) $(COMPFLAGS) -c $<
	touch witness

-include .depend

partialclean:
	rm -f *.cm? *.cm?? *~

depend:
	$(CAMLDEP) $(INCLUDES) *.mli > .depend

