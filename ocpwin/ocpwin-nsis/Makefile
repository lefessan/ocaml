include ../config/Makefile

BUILD_TARGET=ocpwin-nsis-pre

COMMON_SOURCES=cmaFile.ml  byteMisc.ml  byteFile.ml \
   installMisc.ml installPre.ml installPost.ml \
   installWorker.ml

BUILD_SOURCES= cmaFile.ml byteMisc.ml byteFile.ml 	\
   nsis.ml nsisPrepare.ml

BUILD_INCLUDES= -I ../stdlib -I ../otherlibs/unix -I ../byterun \
  -I ../../bytecomp -I ../../typing -I ../../utils -I ../../compilerlibs
BUILD_LIBRARIES=-cclib ../byterun/libcamlrun.a ../otherlibs/unix/libunix.a unix.cma ocamlcommon.cma  ocamlbytecomp.cma

OCAMLC=../byterun/ocamlrun ../../boot/ocamlc

all: $(BUILD_TARGET)

build: $(BUILD_TARGET)

$(BUILD_TARGET): $(BUILD_SOURCES)
	$(OCAMLC) -cc "$(BYTECC)" -custom $(BUILD_INCLUDES) -I $(SRC) $(BUILD_LIBRARIES) -o $(BUILD_TARGET) $(BUILD_SOURCES)

clean::
	rm -f *.cm? *.o
	rm -f $(BUILD_TARGET)
	rm -f *~

