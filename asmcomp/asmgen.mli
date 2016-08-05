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

(* From lambda to assembly code *)

val compile_implementation :
    ?toplevel:(string -> bool) ->
    ?outputprefix:string ->
    string -> Format.formatter -> int * Lambda.lambda -> unit
val compile_phrase :
    Format.formatter -> Cmm.phrase -> unit

type error = Assembler_error of string
exception Error of error
val report_error: Format.formatter -> error -> unit

(* Extra optimization passes to be performed on a function at different
stage of compilation. *)
type 'a passes
val cmm_passes : Cmm.fundecl passes   (* init:[] *)
val mach_passes : Mach.fundecl passes (* init:[allocation combining] *)
val linear_passes : Linearize.fundecl passes (* init:[instruction scheduling] *)
(* declare an extra pass to be run after the ones already declared *)
val add_pass : 'a passes ->
  string ->      (* pass name *)
  bool ref ->    (* debugging flag for verbosity *)
  ('a -> 'a) ->  (* optimization pass *)
  unit
