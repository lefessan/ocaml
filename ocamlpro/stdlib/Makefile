
ifeq "$(HAS_OCPP)" "true"

OCPSTD_CMOS=stringCompat.cmo ocpstd.cmo

OCPSTD_MLS:=$(OCPSTD_CMOS:.cmo=.ml)

OCPSTD_DIR:=$(OCAMLPRO)/stdlib

all-stdlib: all

stringCompat.ml: $(OCPSTD_DIR)/stringCompat.ml
	$(OCAMLPRO)/copy.sh $(OCPSTD_DIR)/stringCompat.ml stringCompat.ml
ocpstd.ml: $(OCPSTD_DIR)/ocpstd.ml
	$(OCAMLPRO)/copy.sh $(OCPSTD_DIR)/ocpstd.ml ocpstd.ml

clean::
	rm -f $(OCPSTD_MLS)

endif

ifeq "$(HAS_SEMPATCH)" "true"

OCPPS=arrayLabels.mli.ocpp  int32.mli.ocpp	pervasives.mli.ocpp \
  array.mli.ocpp	  int32.ml.ocpp	pervasives.ml.ocpp          \
  array.ml.ocpp	  int64.mli.ocpp	random.ml.ocpp              \
  bytes.mli.ocpp	  int64.ml.ocpp	scanf.ml.ocpp               \
  bytes.ml.ocpp	  lazy.ml.ocpp	stringLabels.mli.ocpp               \
  camlinternalFormat.ml.ocpp  marshal.mli.ocpp	string.mli.ocpp     \
  char.ml.ocpp	  marshal.ml.ocpp	string.ml.ocpp              \
  digest.mli.ocpp	  nativeint.mli.ocpp	sys.mli.ocpp        \
  digest.ml.ocpp	  nativeint.ml.ocpp	sys.ml.ocpp         \
  gc.mli.ocpp	  obj.mli.ocpp	weak.ml.ocpp                        \
  gc.ml.ocpp	  obj.ml.ocpp

pervasives.cmi: $(OCPPS)
%.ocpp: $(OCPSTD_DIR)/%.ocpp
	$(OCAMLPRO)/copy.sh $? $@

endif
