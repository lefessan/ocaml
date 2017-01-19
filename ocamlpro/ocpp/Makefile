## Modifications for the preprocessor
## The idea is to use this Makefile as a first step in patching OCaml

# Note that ocpp_main is always linked because the compiler-libs libraries
# are created with -linkall

OCPP_CMOS:= parsing/ocpp_types.cmo parsing/ocpp_version.cmo \
        parsing/ocpp_parser.cmo parsing/ocpp_lexer.cmo parsing/ocpp.cmo \
        parsing/ocpp_main.cmo

OCPP_MLS:=$(OCPP_CMOS:.cmo=.ml)

OCPP_DIR:=$(OCAMLPRO)/ocpp
OCPP_COPY:=$(OCPP_DIR)/copy.sh

beforedepend:: $(OCPP_MLS)

partialclean::
	rm -f $(OCPP_MLS) parsing/ocpp_parser.mli
	rm -f $(OCPP_DIR)/ocpp_parser.mli $(OCPP_DIR)/ocpp_parser.ml
	rm -f $(OCPP_DIR)/ocpp_lexer.ml
	rm -f $(OCPP_DIR)/test_ocpp $(OCPP_DIR)test_ocpp.cm?

parsing/ocpp_types.ml: $(OCPP_DIR)/ocpp_types.ml $(OCPP_DIR)/ocpp_types.mli
	$(OCPP_COPY) $(OCPP_DIR)/ocpp_types.ml parsing/ocpp_types.ml
	$(OCPP_COPY) $(OCPP_DIR)/ocpp_types.mli parsing/ocpp_types.mli
parsing/ocpp_version.ml: $(OCPP_DIR)/ocpp_version.ml
	$(OCPP_COPY) $(OCPP_DIR)/ocpp_version.ml parsing/ocpp_version.ml
parsing/ocpp_parser.ml: $(OCPP_DIR)/ocpp_parser.ml
	$(OCPP_COPY) $(OCPP_DIR)/ocpp_parser.ml parsing/ocpp_parser.ml
parsing/ocpp_parser.mli: $(OCPP_DIR)/ocpp_parser.ml
	$(OCPP_COPY) $(OCPP_DIR)/ocpp_parser.mli parsing/ocpp_parser.mli
parsing/ocpp_lexer.ml: $(OCPP_DIR)/ocpp_lexer.ml
	$(OCPP_COPY) $(OCPP_DIR)/ocpp_lexer.ml parsing/ocpp_lexer.ml
parsing/ocpp.ml: $(OCPP_DIR)/ocpp.ml
	$(OCPP_COPY) $(OCPP_DIR)/ocpp.ml parsing/ocpp.ml
parsing/ocpp_main.ml: $(OCPP_DIR)/ocpp_main.ml
	$(OCPP_COPY) $(OCPP_DIR)/ocpp_main.ml parsing/ocpp_main.ml

$(OCPP_DIR)/ocpp_parser.ml: $(OCPP_DIR)/ocpp_parser.mly
	$(CAMLYACC) $(OCPP_DIR)/ocpp_parser.mly
$(OCPP_DIR)/ocpp_lexer.ml: $(OCPP_DIR)/ocpp_lexer.mll
	$(CAMLLEX) $(OCPP_DIR)/ocpp_lexer.mll

NEW_CAMLC=$(CAMLRUN) ./ocamlc -g -nostdlib -I boot -use-prims byterun/primitives
test_ocpp:
	$(NEW_CAMLC) $(COMPFLAGS) $(LINKFLAGS) -o $(OCPP_DIR)/test_ocpp $(OCPP_DIR)/test_ocpp.ml
	$(CAMLRUN) $(OCPP_DIR)/test_ocpp > $(OCPP_DIR)/test_ocpp.output && diff $(OCPP_DIR)/test_ocpp.output $(OCPP_DIR)/test_ocpp.reference && echo "Good, OCPP works"

include $(OCPP_DIR)/.depend
