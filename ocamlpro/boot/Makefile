# To update this directory:
# * locate the last commit that updated the bootstrap compiler
#     `git log boot/ocamlc`
# * checkout that commit
# * create a new ocamlpro/boot with Makefile.boot only
# * use the `make -f Makefile.boot update` rule
# * use the `make -f Makefile.boot depend` to create a .depend file

# current:
# 10b0afb4e61ce5ddc3e43b2dc0ea70b7  boot/ocamlc


CMIS=asttypes.cmi longident.cmi clflags.cmi docstrings.cmi parser.cmi	\
  config.cmi lexer.cmi parsetree.cmi misc.cmi location.cmi		\
  syntaxerr.cmi warnings.cmi pparse.cmi identifiable.cmi ident.cmi	\
  outcometree.cmi primitive.cmi path.cmi types.cmi cmi_format.cmi	\
  consistbl.cmi tbl.cmi subst.cmi env.cmi lambda.cmi cmo_format.cmi	\
  symtable.cmi

MLIS=$(CMIS:.cmi=.mli)

BOOTDIR=../../boot
CAMLRUN:=$(BOOTDIR)/ocamlrun
CAMLC:=$(CAMLRUN) $(BOOTDIR)/ocamlc
CAMLYACC:=../../yacc/ocamlyacc
CAMLLEX=$(CAMLRUN) ../../boot/ocamllex

MAKE_STDLIB:=$(MAKE) \
	CAMLRUN="../$(CAMLRUN)" \
	CAMLDEP="../$(CAMLRUN) ../$(BOOTDIR)/ocamldep" \
	COMPILER="../$(BOOTDIR)/ocamlc -use-prims ../../../byterun/primitives"

default:
	@echo No default rule

update:
	rm -rf stdlib
	cp -r ../../stdlib .
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
	mkdir -p config
	cd config; ln -sf ../../../config/Makefile
	ln -sf ../../VERSION
	cd stdlib; $(MAKE_STDLIB)
	cd compiler; $(MAKE) $(CMIS)

depend:
	$(MAKE_STDLIB) depend
	cd compiler; $(MAKE) depend

partialclean:
	cd stdlib; $(MAKE) clean
	cd compiler; $(MAKE) partialclean
