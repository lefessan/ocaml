
UTILS_MLIS=

CMIS=asttypes.cmi longident.cmi clflags.cmi docstrings.cmi parser.cmi	\
  config.cmi lexer.cmi parsetree.cmi misc.cmi location.cmi		\
  syntaxerr.cmi warnings.cmi pparse.cmi identifiable.cmi ident.cmi	\
  outcometree.cmi primitive.cmi path.cmi types.cmi cmi_format.cmi	\
  consistbl.cmi tbl.cmi subst.cmi env.cmi lambda.cmi cmo_format.cmi	\
  symtable.cmi

MLIS=$(CMIS:.cmi=.mli)

BOOTDIR=boot2

ROOTDIR=../..
CAMLRUN:=$(ROOTDIR)/boot/ocamlrun
CAMLC:=$(CAMLRUN) $(ROOTDIR)/$(BOOTDIR)/ocamlc
CAMLYACC:=$(ROOTDIR)/yacc/ocamlyacc
CAMLLEX=$(CAMLRUN) $(ROOTDIR)/boot/ocamllex

default:
	@echo No default rule

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
	cd compiler; $(MAKE) $(CMIS)

depend:
	cd compiler; $(MAKE) depend

partialclean:
	rm -f *.cma *.cmi
	cd compiler; $(MAKE) partialclean
