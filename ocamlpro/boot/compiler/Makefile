
BOOTDIR=../../../boot
CAMLRUN:=$(BOOTDIR)/ocamlrun
CAMLC:=$(CAMLRUN) $(BOOTDIR)/ocamlc
CAMLDEP:=$(CAMLRUN) $(BOOTDIR)/ocamldep
CAMLLEX=$(CAMLRUN) $(BOOTDIR)/ocamllex
CAMLYACC:=../../../yacc/ocamlyacc

INCLUDES=-I ../stdlib
COMPFLAGS=$(INCLUDES)

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(CAMLC) $(COMPFLAGS) -c $<

.mli.cmi:
	$(CAMLC) $(COMPFLAGS) -c $<

-include .depend

partialclean:
	rm -f *.cm? *.cm?? *~

depend:
	$(CAMLDEP) $(INCLUDES) *.mli > .depend

