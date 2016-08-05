(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Compilation environments for compilation units *)

open Config
open Misc
open Clambda
open Cmx_format

type error =
    Not_a_unit_info of string
  | Corrupted_unit_info of string
  | Illegal_renaming of string * string * string

exception Error of error

let global_infos_table =
  (Hashtbl.create 17 : (string, unit_infos option) Hashtbl.t)

module CstMap =
  Map.Make(struct
    type t = Clambda.ustructured_constant
    let compare = Clambda.compare_structured_constants
    (* PR#6442: it is incorrect to use Pervasives.compare on values of type t
       because it compares "0.0" and "-0.0" equal. *)
  end)

type structured_constants =
  {
    strcst_shared: string CstMap.t;
    strcst_all: (string * Clambda.ustructured_constant) list;
  }

let structured_constants_empty  =
  {
    strcst_shared = CstMap.empty;
    strcst_all = [];
  }

let structured_constants = ref structured_constants_empty


let exported_constants = Hashtbl.create 17

let current_unit =
  { ui_name = "";
    ui_symbol = "";
    ui_defines = [];
    ui_imports_cmi = [];
    ui_imports_cmx = [];
    ui_approx = Value_unknown;
    ui_curry_fun = [];
    ui_apply_fun = [];
    ui_send_fun = [];
    ui_force_link = false }

let symbolname_for_pack pack name =
  match pack with
  | None -> name
  | Some p ->
      let b = Buffer.create 64 in
      for i = 0 to String.length p - 1 do
        match p.[i] with
        | '.' -> Buffer.add_string b "__"
        |  c  -> Buffer.add_char b c
      done;
      Buffer.add_string b "__";
      Buffer.add_string b name;
      Buffer.contents b


let reset ?packname name =
  Hashtbl.clear global_infos_table;
  let symbol = symbolname_for_pack packname name in
  current_unit.ui_name <- name;
  current_unit.ui_symbol <- symbol;
  current_unit.ui_defines <- [symbol];
  current_unit.ui_imports_cmi <- [];
  current_unit.ui_imports_cmx <- [];
  current_unit.ui_curry_fun <- [];
  current_unit.ui_apply_fun <- [];
  current_unit.ui_send_fun <- [];
  current_unit.ui_force_link <- false;
  Hashtbl.clear exported_constants;
  structured_constants := structured_constants_empty

let current_unit_infos () =
  current_unit

let current_unit_name () =
  current_unit.ui_name

let current_unit_path () =
  Path.Pident (Ident.create_persistent (current_unit_name ()))

let make_symbol ?(unitname = current_unit.ui_symbol) idopt =
  let prefix = "caml" ^ unitname in
  match idopt with
  | None -> prefix
  | Some id -> prefix ^ "__" ^ id

let symbol_in_current_unit name =
  let prefix = "caml" ^ current_unit.ui_symbol in
  name = prefix ||
  (let lp = String.length prefix in
   String.length name >= 2 + lp
   && String.sub name 0 lp = prefix
   && name.[lp] = '_'
   && name.[lp + 1] = '_')

let read_unit_info filename =
  let ic = open_in_bin filename in
  try
    let buffer = really_input_string ic (String.length cmx_magic_number) in
    if buffer <> cmx_magic_number then begin
      close_in ic;
      raise(Error(Not_a_unit_info filename))
    end;
    let ui = (input_value ic : unit_infos) in
    let crc = Digest.input ic in
    close_in ic;
    (ui, crc)
  with End_of_file | Failure _ ->
    close_in ic;
    raise(Error(Corrupted_unit_info(filename)))

let read_library_info filename =
  let ic = open_in_bin filename in
  let buffer = really_input_string ic (String.length cmxa_magic_number) in
  if buffer <> cmxa_magic_number then
    raise(Error(Not_a_unit_info filename));
  let infos = (input_value ic : library_infos) in
  close_in ic;
  infos


(* Read and cache info on global identifiers *)

let get_global_info global_ident = (
  let modname = Ident.name global_ident in
  if modname = current_unit.ui_name then
    Some current_unit
  else begin
    try
      Hashtbl.find global_infos_table modname
    with Not_found ->
      let (infos, crc) =
        try
          let filename =
            find_in_path_uncap !load_path (modname ^ ".cmx") in
          let (ui, crc) = read_unit_info filename in
          if ui.ui_name <> modname then
            raise(Error(Illegal_renaming(modname, ui.ui_name, filename)));
          (Some ui, Some crc)
        with Not_found ->
          (None, None) in
      current_unit.ui_imports_cmx <-
        (modname, crc) :: current_unit.ui_imports_cmx;
      Hashtbl.add global_infos_table modname infos;
      infos
  end
)

let cache_unit_info ui =
  Hashtbl.add global_infos_table ui.ui_name (Some ui)

(* Return the approximation of a global identifier *)

let toplevel_approx = Hashtbl.create 16

let record_global_approx_toplevel id =
  Hashtbl.add toplevel_approx current_unit.ui_name current_unit.ui_approx

let global_approx id =
  if Ident.is_predef_exn id then Value_unknown
  else try Hashtbl.find toplevel_approx (Ident.name id)
  with Not_found ->
    match get_global_info id with
      | None -> Value_unknown
      | Some ui -> ui.ui_approx

(* Return the symbol used to refer to a global identifier *)

let symbol_for_global id =
  if Ident.is_predef_exn id then
    "caml_exn_" ^ Ident.name id
  else begin
    match get_global_info id with
    | None -> make_symbol ~unitname:(Ident.name id) None
    | Some ui -> make_symbol ~unitname:ui.ui_symbol None
  end

(* Register the approximation of the module being compiled *)

let set_global_approx approx =
  current_unit.ui_approx <- approx

(* Record that a currying function or application function is needed *)

let need_curry_fun n =
  if not (List.mem n current_unit.ui_curry_fun) then
    current_unit.ui_curry_fun <- n :: current_unit.ui_curry_fun

let need_apply_fun n =
  if not (List.mem n current_unit.ui_apply_fun) then
    current_unit.ui_apply_fun <- n :: current_unit.ui_apply_fun

let need_send_fun n =
  if not (List.mem n current_unit.ui_send_fun) then
    current_unit.ui_send_fun <- n :: current_unit.ui_send_fun

(* Write the description of the current unit *)

let write_unit_info info filename =
  let oc = open_out_bin filename in
  output_string oc cmx_magic_number;
  output_value oc info;
  flush oc;
  let crc = Digest.file filename in
  WatcherUtils.register_crc filename crc;
  Src_cache.register_cmx crc;
  Digest.output oc crc;
  close_out oc

let save_unit_info filename =
  current_unit.ui_imports_cmi <- Env.imports();
  write_unit_info current_unit filename



let const_label = ref 0

let new_const_label () =
  incr const_label;
  !const_label

let new_const_symbol () =
  incr const_label;
  make_symbol (Some (string_of_int !const_label))

let snapshot () = !structured_constants
let backtrack s = structured_constants := s

let new_structured_constant cst ~shared =
  let {strcst_shared; strcst_all} = !structured_constants in
  if shared then
    try
      CstMap.find cst strcst_shared
    with Not_found ->
      let lbl = new_const_symbol() in
      structured_constants :=
        {
          strcst_shared = CstMap.add cst lbl strcst_shared;
          strcst_all = (lbl, cst) :: strcst_all;
        };
      lbl
  else
    let lbl = new_const_symbol() in
    structured_constants :=
      {
        strcst_shared;
        strcst_all = (lbl, cst) :: strcst_all;
      };
    lbl

let add_exported_constant s =
  Hashtbl.replace exported_constants s ()

let structured_constants () =
  List.map
    (fun (lbl, cst) ->
       (lbl, Hashtbl.mem exported_constants lbl, cst)
    ) (!structured_constants).strcst_all

(** *)

let wrap_locid locid = match Memprof.get_alloc locid.Lambda.id with
  | Lambda.NoAlloc -> Clambda.NoAlloc
  | Lambda.LocId ofs -> Clambda.LocId (
    locid.Lambda.loc.Lambda.l,
    make_symbol (Some "locid_base"), ofs)

let wrap_prim =
  let open Lambda in
  function
  | Pidentity -> Pidentity
  | Pignore -> Pignore
  | Prevapply loc -> Prevapply loc
  | Pdirapply loc -> Pdirapply loc
  | Ploc _ -> assert false
  | Pgetglobal id -> Pgetglobal id
  | Psetglobal id -> Psetglobal id
  | Pmakeblock (sz, mut, locid) -> Pmakeblock (sz, mut, wrap_locid locid)
  | Pfield ofs -> Pfield ofs
  | Psetfield (ofs, ptr) -> Psetfield (ofs, ptr)
  | Pfloatfield (ofs, locid) -> Pfloatfield (ofs, wrap_locid locid)
  | Psetfloatfield ofs -> Psetfloatfield ofs
  | Pduprecord (r, sz, locid) -> Pduprecord (r, sz, wrap_locid locid)
  | Plazyforce -> Plazyforce
  | Pccall (descr, None) -> Pccall (descr, None)
  | Pccall (descr, Some locid) -> Pccall (descr, Some (wrap_locid locid))
  | Praise kind -> Praise kind
  | Psequand -> Psequand
  | Psequor -> Psequor
  | Pnot -> Pnot
  | Pnegint -> Pnegint
  | Paddint -> Paddint
  | Psubint -> Psubint
  | Pmulint -> Pmulint
  | Pdivint -> Pdivint
  | Pmodint -> Pmodint
  | Pandint -> Pandint
  | Porint -> Porint
  | Pxorint -> Pxorint
  | Plslint -> Plslint
  | Plsrint -> Plsrint
  | Pasrint -> Pasrint
  | Pintcomp cmp -> Pintcomp cmp
  | Poffsetint i -> Poffsetint i
  | Poffsetref i -> Poffsetref i
  | Pintoffloat -> Pintoffloat
  | Pfloatofint locid -> Pfloatofint (wrap_locid locid)
  | Pnegfloat locid -> Pnegfloat (wrap_locid locid)
  | Pabsfloat locid -> Pabsfloat (wrap_locid locid)
  | Paddfloat locid -> Paddfloat (wrap_locid locid)
  | Psubfloat locid -> Psubfloat (wrap_locid locid)
  | Pmulfloat locid -> Pmulfloat (wrap_locid locid)
  | Pdivfloat locid -> Pdivfloat (wrap_locid locid)
  | Pfloatcomp cmp -> Pfloatcomp cmp
  | Pstringlength -> Pstringlength
  | Pstringrefu -> Pstringrefu
  | Pstringsetu -> Pstringsetu
  | Pstringrefs -> Pstringrefs
  | Pstringsets -> Pstringsets
  | Pmakearray (kd, locid) -> Pmakearray (kd, wrap_locid locid)
  | Parraylength kd -> Parraylength kd
  | Parrayrefu (kd, locid) -> Parrayrefu (kd, wrap_locid locid)
  | Parraysetu kd -> Parraysetu kd
  | Parrayrefs (kd, locid) -> Parrayrefs (kd, wrap_locid locid)
  | Parraysets kd -> Parraysets kd
  | Pisint -> Pisint
  | Pisout -> Pisout
  | Pbittest -> Pbittest
  | Pbintofint (bi, locid) -> Pbintofint (bi, wrap_locid locid)
  | Pintofbint bi -> Pintofbint bi
  | Pcvtbint (bi1, bi2, locid) -> Pcvtbint (bi1, bi2, wrap_locid locid)
  | Pnegbint (bi, locid) -> Pnegbint (bi, wrap_locid locid)
  | Paddbint (bi, locid) -> Paddbint (bi, wrap_locid locid)
  | Psubbint (bi, locid) -> Psubbint (bi, wrap_locid locid)
  | Pmulbint (bi, locid) -> Pmulbint (bi, wrap_locid locid)
  | Pdivbint (bi, locid) -> Pdivbint (bi, wrap_locid locid)
  | Pmodbint (bi, locid) -> Pmodbint (bi, wrap_locid locid)
  | Pandbint (bi, locid) -> Pandbint (bi, wrap_locid locid)
  | Porbint (bi, locid) -> Porbint (bi, wrap_locid locid)
  | Pxorbint (bi, locid) -> Pxorbint (bi, wrap_locid locid)
  | Plslbint (bi, locid) -> Plslbint (bi, wrap_locid locid)
  | Plsrbint (bi, locid) -> Plsrbint (bi, wrap_locid locid)
  | Pasrbint (bi, locid) -> Pasrbint (bi, wrap_locid locid)
  | Pbintcomp (bi, cmp) -> Pbintcomp (bi, cmp)
  | Pbigarrayref (unsafe, n, kind, layout, loc) ->
      Pbigarrayref (unsafe, n, kind, layout, loc)
  | Pbigarrayset (unsafe, n, kind, layout) ->
      Pbigarrayset (unsafe, n, kind, layout)
  | Pbigarraydim dim -> Pbigarraydim dim
  | Pstring_load_16 b -> Pstring_load_16 b
  | Pstring_load_32 (b, loc) -> Pstring_load_32 (b, loc)
  | Pstring_load_64 (b, loc) -> Pstring_load_64 (b, loc)
  | Pstring_set_16 b -> Pstring_set_16 b
  | Pstring_set_32 b -> Pstring_set_32 b
  | Pstring_set_64 b -> Pstring_set_64 b
  | Pbigstring_load_16 b -> Pbigstring_load_16 b
  | Pbigstring_load_32 (b,loc) -> Pbigstring_load_32 (b,loc)
  | Pbigstring_load_64 (b,loc) -> Pbigstring_load_64 (b,loc)
  | Pbigstring_set_16 b -> Pbigstring_set_16 b
  | Pbigstring_set_32 b -> Pbigstring_set_32 b
  | Pbigstring_set_64 b -> Pbigstring_set_64 b
  | Pctconst cst -> Pctconst cst
  | Pbswap16 -> Pbswap16
  | Pbbswap (bi, locid) -> Pbbswap (bi, wrap_locid locid)
  | Pint_as_pointer -> Pint_as_pointer

(* Error report *)

open Format

let report_error ppf = function
  | Not_a_unit_info filename ->
      fprintf ppf "%a@ is not a compilation unit description."
        Location.print_filename filename
  | Corrupted_unit_info filename ->
      fprintf ppf "Corrupted compilation unit description@ %a"
        Location.print_filename filename
  | Illegal_renaming(name, modname, filename) ->
      fprintf ppf "%a@ contains the description for unit\
                   @ %s when %s was expected"
        Location.print_filename filename name modname

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error err)
      | _ -> None
    )
