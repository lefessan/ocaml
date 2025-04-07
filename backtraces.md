# Mécanisme de gestion des backtraces dans OCaml 4.14

Valide depuis la version ???

## Interface externe `Printexc`

```
(* 3.11 *)
val print_backtrace: out_channel -> unit
val get_backtrace: unit -> string
val record_backtrace: bool -> unit
val backtrace_status: unit -> bool

(* 4.01 *)
type raw_backtrace (* = raw_backtrace_entry array *)
val get_raw_backtrace: unit -> raw_backtrace
val print_raw_backtrace: out_channel -> raw_backtrace -> unit
val raw_backtrace_to_string: raw_backtrace -> string
external get_callstack: int -> raw_backtrace = "caml_get_current_callstack"

(* 4.02 *)
type backtrace_slot (*  =
  | Known_location of {
      is_raise    : bool;
      filename    : string;
      line_number : int;
      start_char  : int;
      end_char    : int;
      is_inline   : bool;
      defname     : string;
    }
  | Unknown_location of {
      is_raise : bool
    }
*)

val backtrace_slots : raw_backtrace -> backtrace_slot array option
val set_uncaught_exception_handler: (exn -> raw_backtrace -> unit) -> unit

type raw_backtrace_slot
val raw_backtrace_length : raw_backtrace -> int
val get_raw_backtrace_slot : raw_backtrace -> int -> raw_backtrace_slot
val convert_raw_backtrace_slot : raw_backtrace_slot -> backtrace_slot

type Slot.t = backtrace_slot
val Slot.is_raise: Slot.t -> bool
val Slot.location : Slot.t -> location option
val Slot.format : int -> Slot.t -> string option

(* 4.04 *)
val get_raw_backtrace_next_slot : raw_backtrace_slot ->
  raw_backtrace_slot option
val Slot.is_inline : Slot.t -> bool

(* 4.05 *)
external raise_with_backtrace: exn -> raw_backtrace -> 'a

(* 4.11 *)
val default_uncaught_exception_handler: exn -> raw_backtrace -> unit
val Slot.name : Slot.t -> string option

(* 4.12 *)
type raw_backtrace_entry = private int
val raw_backtrace_entries : raw_backtrace -> raw_backtrace_entry array
val backtrace_slots_of_raw_entry :
  raw_backtrace_entry -> backtrace_slot array option
```

## FFI

```
external get_raw_backtrace:
  unit -> raw_backtrace = "caml_get_exception_raw_backtrace"
external raise_with_backtrace: exn -> raw_backtrace -> 'a
  = "%raise_with_backtrace"
external convert_raw_backtrace_slot:
  raw_backtrace_slot -> backtrace_slot = "caml_convert_raw_backtrace_slot"
external convert_raw_backtrace:
  raw_backtrace -> backtrace_slot array = "caml_convert_raw_backtrace"
external get_raw_backtrace_slot :
  raw_backtrace -> int -> raw_backtrace_slot = "caml_raw_backtrace_slot"
external get_raw_backtrace_next_slot :
  raw_backtrace_slot -> raw_backtrace_slot option
  = "caml_raw_backtrace_next_slot"
external record_backtrace: bool -> unit = "caml_record_backtrace"
external backtrace_status: unit -> bool = "caml_backtrace_status"
external get_callstack: int -> raw_backtrace = "caml_get_current_callstack"
```

## Fichiers sources

`backtrace.h` contains some description of the internal behavior.

* `stdlib/printexc.ml`: interface OCaml pour les backtraces
* `runtime/`:
  * `backtrace.c`:
  * `backtrace_byt.c`:
  * `backtrace_nat.c`:
  * `caml/`:
    * `backtrace.h`:
    * `backtrace_prim.h`:
```
/* In order to prevent the GC from walking through the debug
   information (which have no headers), we transform slots to 31/63 bits
   ocaml integers by shifting them by 1 to the right. We do not lose
   information as slots are aligned.

   In particular, we do not need to use [caml_modify] when setting
   an array element with such a value.
 */
#define Val_backtrace_slot(bslot) (Val_long(((uintnat)(bslot))>>1))
#define Backtrace_slot_val(vslot) ((backtrace_slot)(Long_val(vslot) << 1))
```
    * `misc.h`:
```
 * The [backtrace_slot] type represents values stored in
 * [Caml_state->backtrace_buffer].  In bytecode, it is the same as a
 * [code_t], in native code it is either a [frame_descr *] or a [debuginfo],
 * depending on the second-lowest bit.  In any case, the lowest bit must
 * be 0.
 * The representation doesn't matter for code outside [backtrace_{byt,nat}.c],
 * so it is just exposed as a [void *].
 */
typedef void * backtrace_slot;
```

Dans `caml/stack.h`:
```
/* Structure of frame descriptors */
typedef struct {

/* return address associated with this frame_descr */
  uintnat retaddr;

/* size of the frame (in values), so that adding (this
   value & 0XFFFC) to SP will move you to the next stack frame.
   if frame_size & 2 <> 0, alloc info is present
   if frame_size & 1 <> 0, debug info is present
*/
  unsigned short frame_size;

/* size of live registers, saved into live_ofs (in bytes) */
  unsigned short num_live;

/* saved lived registers */
  unsigned short live_ofs[1 /* num_live */];

/*
    If frame_size & 2, then allocation info follows:
  unsigned char num_allocs;
  unsigned char alloc_lengths[num_alloc];
*/

/*
    If frame_size & 1, then debug info follows:
  uint32_t debug_info_offset[num_debug];

    Debug info is stored as relative offsets to debuginfo structures.
    num_debug is num_alloc if frame_size & 2, otherwise 1.
*/
} frame_descr;
```

Debuginfo format (int64) (from lowest bit to highest bit)
```
* 1 bit: 0 for last debuginfo, 1 if there is another one next
* 1 bit: 0 for CALL, 1 for RAISE
* 24 bits: offset of file name relative to debuginfo pointer (in dwords)
* 20 bits: line number
* 8 bits: begin char
* 10 bits: last char
```

Une backtrace_slot est soit un `debuginfo (uint64*)`, soit un
`frame_descr*`. Le premier cas n'apparaît que quand on utilise
`caml_collect_current_callstack` avec un `alloc_idx>=0` (i.e. cela
correspond à l'allocation d'une certaine value dans comballoc).

Dans `Caml_state`, on va utiliser:
* `backtrace_buffer`: un buffer contenant la backtrace courante
* `backtrace_pos`: la position de la prochaine frame à stocker dans
    la `backtrace_buffer`.

Le remplissage de `backtrace_buffer` se fait généralement dans
`caml_stack_backtrace()` (limitée à BACKTRACE_BUFFER_SIZE) et dans
`caml_collect_current_callstack()`.

## Remarques

* En bytecode, l'association entre backtrace_slot et debug_info se fait
  par une recherche linéaire au lieu d'un dichotomie (`find_debug_info`
  dans `backtrace_byt.c`)

## Solution

On va ajouter un nouveau type de slot:
| Repeated of { cycle : int ; occurrences : int }

Slot:
slot & 2 <> 0 => debuginfo (pointer to debuginfo)
slot & 4 <> 0 


	  int cycle_len = (((uintnat)dbg) >> 3) & 0xF;
	  int ncycles =   (((uintnat)dbg) >> 7) & 0xFFFFFFFF;


