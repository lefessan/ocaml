Use "make all". It should give something like:

╰─➤ ./test1.asm
Fatal error: exception Stack_overflow
Raised by primitive operation at file "test1.ml", line 4, characters 8-18
Called from file "test1.ml", line 4, characters 8-18
Called from file "test1.ml", line 4, characters 8-18
Recursion called 8380784 times:
  * Called from file "test1.ml", line 4, characters 8-18
Called from file "test1.ml", line 6, characters 2-13

╰─➤ ./test2.asm
Fatal error: exception Stack_overflow
Raised by primitive operation at file "test2.ml", line 4, characters 8-19
Mutual recursion called 4191121 times:
  [1] Called from file "test2.ml", line 7, characters 8-19
  [2] Called from file "test2.ml", line 4, characters 8-19
Called from file "test2.ml", line 9, characters 2-14

╰─➤ ./test3.asm
Fatal error: exception Not_found
Raised at file "test3.ml", line 8, characters 18-27

╰─➤ ./test4.asm
Should print the old backtrace (passed through Printexc.get_raw_backtrace)
╰─➤ ./test5.asm
Should print the old backtrace (passed through Printexc.get_raw_backtrace)

╰─➤ ./test6.asm
Fatal error: exception Stack_overflow
Called from file "test6.ml", line 10, characters 8-19
Called from file "test6.ml", line 7, characters 8-19
Called from file "test6.ml", line 4, characters 8-19
Called from file "test6.ml", line 10, characters 8-19
Called from file "test6.ml", line 7, characters 8-19
Called from file "test6.ml", line 4, characters 8-19
Called from file "test6.ml", line 10, characters 8-19
Called from file "test6.ml", line 7, characters 8-19
Called from file "test6.ml", line 4, characters 8-19
Re-raised at file "long backtrace cut here =========", line -1, characters 0-0
Called from file "test6.ml", line 7, characters 8-19
Called from file "test6.ml", line 4, characters 8-19
Called from file "test6.ml", line 10, characters 8-19
Called from file "test6.ml", line 7, characters 8-19
Called from file "test6.ml", line 4, characters 8-19
Called from file "test6.ml", line 10, characters 8-19
Called from file "test6.ml", line 7, characters 8-19
Called from file "test6.ml", line 4, characters 8-19
...
Called from file "test6.ml", line 7, characters 8-19
Called from file "test6.ml", line 4, characters 8-19
Called from file "test6.ml", line 10, characters 8-19
Called from file "test6.ml", line 7, characters 8-19
Called from file "test6.ml", line 4, characters 8-19
Called from file "test6.ml", line 10, characters 8-19
Called from file "test6.ml", line 7, characters 8-19
Called from file "test6.ml", line 4, characters 8-19
Called from file "test6.ml", line 12, characters 2-14
