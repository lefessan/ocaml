all:
	../boot/ocamlrun ../ocamlopt -g -I ../stdlib -o test1.asm test1.ml
	../boot/ocamlrun ../ocamlopt -g -I ../stdlib -o test2.asm test2.ml
	../boot/ocamlrun ../ocamlopt -g -I ../stdlib -o test3.asm test3.ml
	../boot/ocamlrun ../ocamlopt -g -I ../stdlib -o test4.asm test4.ml
	../boot/ocamlrun ../ocamlopt -g -I ../stdlib -o test5.asm test5.ml
	../boot/ocamlrun ../ocamlopt -g -I ../stdlib -o test6.asm test6.ml

nog:
	../boot/ocamlrun ../ocamlopt -I ../stdlib -o test1 test1.ml
	../boot/ocamlrun ../ocamlopt -I ../stdlib -o test2 test2.ml

clean:
	rm -f test?.asm *.o *.cm?
	rm -f a.out core *~
