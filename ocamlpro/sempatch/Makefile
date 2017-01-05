## Modifications for the preprocessor
## The idea is to use this Makefile as a first step in patching OCaml

ifeq "$(HAS_OCPP)$(HAS_STRINGCOMPAT)" "truetrue"

SEMPATCH_CMOS:= \
  parsing/parsetree_iter.cmo \
  parsing/parsetree_map.cmo \
  parsing/patch_types.cmo parsing/patch_lexer.cmo		\
  parsing/patch_parser.cmo parsing/patch_engine.cmo	\
  parsing/patch_main.cmo

SEMPATCH_MLS:=$(SEMPATCH_CMOS:.cmo=.ml)

SEM_DIR:=$(OCAMLPRO)/sempatch
SEM_COPY:=$(SEM_DIR)/copy.sh
beforedepend:: $(SEMPATCH_MLS)

partialclean::
	rm -f $(SEMPATCH_MLS) parsing/patch_parser.mli
	rm -f $(SEM_DIR)/patch_parser.mli $(SEM_DIR)/patch_parser.ml
	rm -f $(SEM_DIR)/patch_lexer.ml

parsing/parsetree_map.ml: $(SEM_DIR)/parsetree_map.ml
	$(SEM_COPY) $(SEM_DIR)/parsetree_map.ml parsing/parsetree_map.ml
parsing/parsetree_iter.ml: $(SEM_DIR)/parsetree_iter.ml
	$(SEM_COPY) $(SEM_DIR)/parsetree_iter.ml parsing/parsetree_iter.ml
parsing/patch_types.ml: $(SEM_DIR)/patch_types.ml
	$(SEM_COPY) $(SEM_DIR)/patch_types.ml parsing/patch_types.ml
parsing/patch_parser.ml: $(SEM_DIR)/patch_parser.ml
	$(SEM_COPY) $(SEM_DIR)/patch_parser.ml parsing/patch_parser.ml
parsing/patch_parser.mli: $(SEM_DIR)/patch_parser.ml
	$(SEM_COPY) $(SEM_DIR)/patch_parser.mli parsing/patch_parser.mli
parsing/patch_lexer.ml: $(SEM_DIR)/patch_lexer.ml
	$(SEM_COPY) $(SEM_DIR)/patch_lexer.ml parsing/patch_lexer.ml
parsing/patch_engine.ml: $(SEM_DIR)/patch_engine.ml
	$(SEM_COPY) $(SEM_DIR)/patch_engine.ml parsing/patch_engine.ml
parsing/patch_main.ml: $(SEM_DIR)/patch_main.ml
	$(SEM_COPY) $(SEM_DIR)/patch_main.ml parsing/patch_main.ml

$(SEM_DIR)/patch_parser.ml: $(SEM_DIR)/patch_parser.mly
	$(CAMLYACC) $(SEM_DIR)/patch_parser.mly
$(SEM_DIR)/patch_lexer.ml: $(SEM_DIR)/patch_lexer.mll
	$(CAMLLEX) $(SEM_DIR)/patch_lexer.mll

NEW_CAMLC=$(CAMLRUN) ./ocamlc -g -nostdlib -I boot -use-prims byterun/primitives
test_patch:
	$(NEW_CAMLC) $(COMPFLAGS) $(LINKFLAGS) -o $(OCAMLPRO)/test_patch $(OCAMLPRO)/test_patch.ml
	$(CAMLRUN) ./$(OCAMLPRO)/test_patch > $(OCAMLPRO)/test_patch.output && diff $(OCAMLPRO)/test_patch.output $(OCAMLPRO)/test_patch.reference && echo "Good, PATCH works"
	rm -f ./$(OCAMLPRO)/test_patch
	rm -f ./$(OCAMLPRO)/test_patch.cmo

endif
