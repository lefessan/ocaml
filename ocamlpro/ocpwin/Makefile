ifeq "$(WANT_OCPWIN)" "true"

OCPWIN_CMOS:= utils/msvc.cmo
OCPWIN_MLS:=$(OCPWIN_CMOS:.cmo=.ml)
OCPWIN_MLIS:=$(OCPWIN_CMOS:.cmo=.mli)

OCPWIN_DIR:=$(OCAMLPRO)/ocpp

beforedepend:: $(OCPWIN_MLS) $(OCPWIN_MLIS)

partialclean::
	rm -f $(OCPWIN_MLS) $(OCPWIN_MLIS)

utils/msvc.ml: $(OCPWIN_DIR)/msvc.ml
	$(OCAMLPRO)/copy.sh $(OCPWIN_DIR)/msvc.ml utils/msvc.ml
utils/msvc.mli: $(OCPWIN_DIR)/msvc.mli
	$(OCAMLPRO)/copy.sh $(OCPWIN_DIR)/msvc.mli utils/msvc.mli

endif
