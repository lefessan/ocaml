
all-targets: all

ifeq "$(HAS_STRINGCOMPAT)" "true"

OCPSTD_CMOS=stringCompat.cmo ocpstd.cmo

OCPSTD_DIR:=$(OCAMLPRO)/stdlib

ifeq "$(HAS_SEMPATCH)" "true"

OCPPS:=pervasives.mli.ocpp pervasives.ml.ocpp

pervasives.cmo: $(OCPPS)

%.ocpp: $(OCPSTD_DIR)/%.ocpp
	$(OCAMLPRO)/copy.sh $? $@

endif

stringCompat.ml: $(OCPSTD_DIR)/stringCompat.ml
	$(OCAMLPRO)/copy.sh $? $@
ocpstd.ml: $(OCPSTD_DIR)/ocpstd.ml
	$(OCAMLPRO)/copy.sh $? $@

endif
