cleanup.cmo : ../boot/compiler/symtable.cmi
cleanup.cmx : ../boot/compiler/symtable.cmi
ocpstd.cmo : ../boot/stdlib/sys.cmi ../boot/stdlib/string.cmi \
    ../boot/stdlib/list.cmi ../boot/stdlib/filename.cmi ocpstd.cmi
ocpstd.cmx : ../boot/stdlib/sys.cmx ../boot/stdlib/string.cmx \
    ../boot/stdlib/list.cmx ../boot/stdlib/filename.cmx ocpstd.cmi
ocpstd.cmi :
parsetree_iter.cmo : ../boot/compiler/parsetree.cmi ../boot/stdlib/list.cmi \
    ../boot/compiler/asttypes.cmi parsetree_iter.cmi
parsetree_iter.cmx : ../boot/compiler/parsetree.cmi ../boot/stdlib/list.cmx \
    ../boot/compiler/asttypes.cmi parsetree_iter.cmi
parsetree_iter.cmi : ../boot/compiler/parsetree.cmi \
    ../boot/compiler/asttypes.cmi
parsetree_map.cmo : ../boot/compiler/parsetree.cmi ../boot/compiler/misc.cmi \
    ../boot/stdlib/list.cmi parsetree_map.cmi
parsetree_map.cmx : ../boot/compiler/parsetree.cmi ../boot/compiler/misc.cmi \
    ../boot/stdlib/list.cmx parsetree_map.cmi
parsetree_map.cmi : ../boot/compiler/parsetree.cmi
patch_engine.cmo : ../boot/stdlib/sys.cmi ../boot/stdlib/string.cmi \
    ../boot/stdlib/printf.cmi ../boot/stdlib/printexc.cmi patch_types.cmo \
    ../boot/stdlib/parsing.cmi parsetree_map.cmi \
    ../boot/compiler/parsetree.cmi ../boot/compiler/parser.cmi \
    ../boot/stdlib/map.cmi ../boot/compiler/longident.cmi \
    ../boot/compiler/location.cmi ../boot/stdlib/list.cmi \
    ../boot/stdlib/lexing.cmi ../boot/compiler/lexer.cmi \
    ../boot/compiler/asttypes.cmi
patch_engine.cmx : ../boot/stdlib/sys.cmx ../boot/stdlib/string.cmx \
    ../boot/stdlib/printf.cmx ../boot/stdlib/printexc.cmx patch_types.cmx \
    ../boot/stdlib/parsing.cmx parsetree_map.cmx \
    ../boot/compiler/parsetree.cmi ../boot/compiler/parser.cmi \
    ../boot/stdlib/map.cmx ../boot/compiler/longident.cmi \
    ../boot/compiler/location.cmi ../boot/stdlib/list.cmx \
    ../boot/stdlib/lexing.cmx ../boot/compiler/lexer.cmi \
    ../boot/compiler/asttypes.cmi
patch_lexer.cmo : stringCompat.cmo ../boot/stdlib/string.cmi \
    ../boot/stdlib/list.cmi ../boot/stdlib/lexing.cmi \
    ../boot/stdlib/hashtbl.cmi ../boot/stdlib/format.cmi \
    ../boot/stdlib/char.cmi ../boot/stdlib/bytes.cmi
patch_lexer.cmx : stringCompat.cmx ../boot/stdlib/string.cmx \
    ../boot/stdlib/list.cmx ../boot/stdlib/lexing.cmx \
    ../boot/stdlib/hashtbl.cmx ../boot/stdlib/format.cmx \
    ../boot/stdlib/char.cmx ../boot/stdlib/bytes.cmx
patch_main.cmo : ../boot/stdlib/sys.cmi stringCompat.cmo \
    ../boot/stdlib/string.cmi ../boot/stdlib/printf.cmi \
    ../boot/compiler/pparse.cmi patch_types.cmo patch_parser.cmi \
    patch_lexer.cmo patch_engine.cmo ../boot/stdlib/parsing.cmi ocpstd.cmi \
    ../ocpp/ocpp.cmi ../boot/compiler/misc.cmi ../boot/stdlib/list.cmi \
    ../boot/stdlib/lexing.cmi ../boot/stdlib/filename.cmi
patch_main.cmx : ../boot/stdlib/sys.cmx stringCompat.cmx \
    ../boot/stdlib/string.cmx ../boot/stdlib/printf.cmx \
    ../boot/compiler/pparse.cmi patch_types.cmx patch_parser.cmx \
    patch_lexer.cmx patch_engine.cmx ../boot/stdlib/parsing.cmx ocpstd.cmx \
    ../ocpp/ocpp.cmx ../boot/compiler/misc.cmi ../boot/stdlib/list.cmx \
    ../boot/stdlib/lexing.cmx ../boot/stdlib/filename.cmx
patch_parser.cmo : patch_types.cmo ../boot/stdlib/parsing.cmi \
    ../boot/stdlib/obj.cmi ../boot/stdlib/lexing.cmi patch_parser.cmi
patch_parser.cmx : patch_types.cmx ../boot/stdlib/parsing.cmx \
    ../boot/stdlib/obj.cmx ../boot/stdlib/lexing.cmx patch_parser.cmi
patch_parser.cmi : patch_types.cmo ../boot/stdlib/lexing.cmi
patch_types.cmo :
patch_types.cmx :
stringCompat.cmo : ../boot/stdlib/string.cmi ../boot/stdlib/set.cmi \
    ../boot/stdlib/map.cmi ../boot/stdlib/list.cmi ../boot/stdlib/bytes.cmi \
    ../boot/stdlib/buffer.cmi
stringCompat.cmx : ../boot/stdlib/string.cmx ../boot/stdlib/set.cmx \
    ../boot/stdlib/map.cmx ../boot/stdlib/list.cmx ../boot/stdlib/bytes.cmx \
    ../boot/stdlib/buffer.cmx
