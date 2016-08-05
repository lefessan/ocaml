OCAMLSRC=../..
OCAMLPROSRC=../../../ocamlpro

include $(OCAMLSRC)/config/Makefile

host: ocpwin.asm bytecode.byte ocp-wrapper.exe

OCAMLOPT=$(OCAMLSRC)/boot/ocamlrun $(OCAMLSRC)/ocamlopt -I $(OCAMLSRC)/stdlib
OCAMLC=$(OCAMLSRC)/boot/ocamlrun $(OCAMLSRC)/ocamlc -I $(OCAMLSRC)/stdlib

INCLUDES=\
	-I $(OCAMLSRC)/compilerlibs \
	-I $(OCAMLSRC)/utils \
	-I $(OCAMLSRC)/parsing \
	-I $(OCAMLSRC)/typing \
	-I $(OCAMLSRC)/bytecomp \
	-I $(OCAMLSRC)/otherlibs/win32unix \
	-I $(OCAMLSRC)/byterun \
	-I $(OCAMLPROSRC)

LIBS=ocamlcommon.cmxa unix.cmxa
BUILD_SOURCES= win32Registry_c.c win32Registry.mli win32Registry.ml main.ml

ocpwin.asm: $(BUILD_SOURCES)
	$(OCAMLOPT) -o ocpwin.asm $(INCLUDES) $(LIBS) $(BUILD_SOURCES)

bytecode.byte: bytecode.ml Makefile.cross
	OCAMLPARAM=_,g=0 $(OCAMLC) -o bytecode.byte $(INCLUDES) -nostdlib -dllib -lunix bytecode.ml

# Use -s and -0s to decrease binary size
ocp-wrapper.exe: wrapper.c
	$(BYTECC) -s -Os  -o ocp-wrapper.exe wrapper.c

# trailer.byte is included in GIT, since (1) ocp-byteclean is required to
# generate it, and (2) the trailer will not change much.
update-trailer: bytecode.byte
	rm -f trailer.byte
	ocp-byteclean -o trailer.byte -only-bytecode bytecode.byte

clean::
	rm -f *.o *.cm? *.exe *~ *.asm *.cmti
