OCAMLC?=../../boot/ocamlrun ../../ocamlc

# First, use "make cross-install" to fill the cross/local directory,
# then, run this Makefile to build a matching bytecode version of
# ocp-build

all: ocp-build

ocp-build.boot: ../../../../ocp-build/boot/ocp-build.boot
	rm -f ocp-build.boot
	cp -f ../../../../ocp-build/boot/ocp-build.boot ocp-build.boot

commonUnix_c.c:  ../../../../ocp-build/boot/commonUnix_c.c
	rm -f commonUnix_c.c
	cp -f ../../../../ocp-build/boot/commonUnix_c.c commonUnix_c.c


ocp-build: primitives ocp-build.boot commonUnix_c.c
	awk '{ print "external "$$1": unit -> unit = \"" $$1 "\"" }' primitives > primitives.ml
	$(OCAMLC) -I ../local/lib -o ocp-build.run -custom -make-runtime commonUnix_c.c primitives.ml unix.cma -cclib -lunix
	cat ocp-build.run ocp-build.boot > ocp-build
	chmod +x ocp-build
	./ocp-build -help >/dev/null 2>&1

clean:
	rm -f *~ ocp-build ocp-build.run primitives.cm? commonUnix_c.o commonUnix_c.obj primitives.ml ocp-build.boot commonUnix_c.c

