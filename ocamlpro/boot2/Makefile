
UTILS_MLIS=

MLIS=$(CMIS:.cmi=.mli)

BOOTDIR ?= boot2

ROOTDIR=../..
CAMLRUN:=$(ROOTDIR)/boot/ocamlrun
CAMLC:=$(CAMLRUN) $(ROOTDIR)/$(BOOTDIR)/ocamlc
CAMLYACC:=$(ROOTDIR)/yacc/ocamlyacc
CAMLLEX=$(CAMLRUN) $(ROOTDIR)/boot/ocamllex

default: all

update:
	rm -rf compiler compiler-srcs
	mkdir compiler
	mkdir compiler-srcs
	cp -f ../../utils/*.mli compiler-srcs/
	cp -f ../../parsing/*.mli compiler-srcs/
	cp -f ../../parsing/parser.mly compiler-srcs/
	cp -f ../../typing/*.mli compiler-srcs/
	cp -f ../../bytecomp/*.mli compiler-srcs/
	cp -f ../../driver/*.mli compiler-srcs/
	cd compiler-srcs/; ../$(CAMLYACC) parser.mly
	cd compiler-srcs/; cp $(MLIS) ../compiler
	rm -rf compiler-srcs/
	cp Makefile.compiler compiler/Makefile
	cd compiler; $(MAKE) depend

all:
	cd compiler; $(MAKE) all

depend:
	cd compiler; $(MAKE) depend

partialclean:
	rm -f *.cma *.cmi
	cd compiler; $(MAKE) partialclean
