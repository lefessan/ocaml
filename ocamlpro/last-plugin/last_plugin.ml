

(* HACK: loading plugins in bytecode (dynlink) is incompatible with
   linking, thus we need to call Symtable.reset after the last plugin
   to avoid bad interactions with linking. *)
let () = Symtable.reset ()
