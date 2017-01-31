all: byte opt
tests:  byte-test opt-test

byte:  ocp-memprof-bytecode.so
byte-test: byte ocp-memprof-native.test
opt: ocp-memprof-native.so
opt-test: opt ocp-memprof-bytecode.test

include ../config/Makefile

CDEPS=commit.h c_gcprof.h c_heapprof.h
FILES=\
  c_heapprof.c \
  c_process_info.c \
  c_control.c \
  c_allocprof.c \
  c_gcprof.c \
  c_common.c

BYTEOBJS=$(FILES:.c=.byte.o)
NATOBJS=$(FILES:.c=.native.o)

ifneq ($(shell cat COMMIT 2>/dev/null),$(shell git rev-parse --short HEAD))
.PHONY: COMMIT
endif

COMMIT:
	git rev-parse --short HEAD > COMMIT
	date +"%Y%m%d-%H%M" > DATE

commit.h: COMMIT
	echo '#define OCP_COMMIT "'$(shell cat COMMIT)'"' > commit.h
	echo '#define OCP_DATE "'$(shell cat DATE)'"' >> commit.h

ocp-memprof-native.so: $(NATOBJS) $(CDEPS) 
	$(MKDLL) -o ocp-memprof-native.so $(NATOBJS)

ocp-memprof-native.test: ocp-memprof-native.so test.ml
	ocamlopt -o ocp-memprof-native.test test.ml -cclib ocp-memprof-native.so

ocp-memprof-bytecode.so: $(BYTEOBJS) $(CDEPS) 
	$(MKDLL) -o ocp-memprof-bytecode.so $(BYTEOBJS)

$(BYTEOBJS): $(CDEPS)
$(NATOBJS): $(CDEPS)

test.ml: Makefile
	echo > test.ml

ocp-memprof-bytecode.test: ocp-memprof-bytecode.so test.ml
	ocamlc -custom -o ocp-memprof-bytecode.test test.ml -cclib ocp-memprof-bytecode.so

clean:
	rm -f *~ *.o *.so *.test test.ml
	rm -f *.byte.c *.native.c *.cm?

INCLUDES=-I ../byterun
OCAMLC=../boot/ocamlrun ../ocamlc $(INCLUDES)

.SUFFIXES: .native.o .c .byte.o

.c.byte.o:
	echo '#1 "'"$*.c"'"' > $*.byte.c
	cat $*.c >> $*.byte.c
	 $(OCAMLC) -verbose -ccopt "-shared -fPIC" -c $*.byte.c
	rm -f $*.byte.c


.c.native.o:
	cp -f $*.c $*.native.c
	../boot/ocamlrun ../ocamlopt -verbose -ccopt "-DNATIVE_CODE -DTARGET_$(ARCH) -DSYS_$(SYSTEM) -shared -fPIC" -c $*.native.c
	rm -f $*.native.c
