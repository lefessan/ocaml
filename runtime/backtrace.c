/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 2000 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

/* Stack backtrace for uncaught exceptions */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "caml/alloc.h"
#include "caml/memory.h"
#include "caml/backtrace.h"
#include "caml/backtrace_prim.h"
#include "caml/fail.h"
#include "caml/debugger.h"
#include "caml/startup.h"

void caml_init_backtrace(void)
{
  caml_register_global_root(&Caml_state->backtrace_last_exn);
}

/* Start or stop the backtrace machinery */
CAMLexport void caml_record_backtraces(int flag)
{
  int is_limited = flag & 2 ;
  int no_cycles = flag & 4 ;
  flag &= 1;
  if (flag != Caml_state->backtrace_active
      || is_limited != Caml_state->backtrace_is_limited
      || no_cycles != Caml_state->backtrace_no_cycles
    ) {
    Caml_state->backtrace_active = flag;

    /* is_limited can only be set, not unset. Otherwise,
    Printexc.record_backtrace would unset it all the time. */
    if (is_limited && !Caml_state->backtrace_is_limited)
      Caml_state->backtrace_is_limited = 1 ;
    if (no_cycles && !Caml_state->backtrace_no_cycles)
      Caml_state->backtrace_no_cycles = 1 ;
    Caml_state->backtrace_pos = 0;
    Caml_state->backtrace_last_exn = Val_unit;
    /* Note: We do lazy initialization of Caml_state->backtrace_buffer when
       needed in order to simplify the interface with the thread
       library (thread creation doesn't need to allocate
       Caml_state->backtrace_buffer). So we don't have to allocate it here.
    */
  }
  return;
}

/* Funily, caml_record_backtraces can take any value in flag, but some
   assembler implementations will only work with 1. We extend this
   behavior so that bit 2 means that backtraces should be limited. */
CAMLprim value caml_record_backtrace(value flag)
{
  caml_record_backtraces(Int_val(flag));
  return Val_unit;
}

/* Return the status of the backtrace machinery */
CAMLprim value caml_backtrace_status(value vunit)
{
  return Val_bool(Caml_state->backtrace_active);
}

/* Print location information -- same behavior as in Printexc

   note that the test for compiler-inserted raises is slightly redundant:
     (!li->loc_valid && li->loc_is_raise)
   caml_debuginfo_location guarantees that when li->loc_valid is
   0, then li->loc_is_raise is always 1, so the latter test is
   useless. We kept it to keep code identical to the runtime/
   implementation. */
static void print_location(struct caml_loc_info * li, int index)
{
  char * info;
  char * inlined;

  /* Ignore compiler-inserted raise */
  if (li->loc_kind == CAML_LOC_KIND_UNKNOWN && li->loc_is_raise) return;

#ifdef ARCH_SIXTYFOUR
  /* WARNING: because of the previous line, the line for some loc_info
     may not be printed, causing this message to be wrong. */
  if (li->loc_kind == CAML_LOC_KIND_REPEATED){
    int cycle_len = li->loc_is_raise ;
    int ncycles = li->loc_lnum;
    if (cycle_len == 0) {
      if (ncycles == 0) {
        fprintf (stderr, "Skipped many calls");
      } else {
        fprintf (stderr, "Skipped %d calls", ncycles);
      }
    } else
    if (cycle_len == 1) {
      fprintf (stderr, "Next call repeated %d times", ncycles);
    } else {
      fprintf (stderr, "Next cycle of %d calls repeated %d times", cycle_len, ncycles);
    }
    return;
  }
#endif

  if (li->loc_is_raise) {
    /* Initial raise if index == 0, re-raise otherwise */
    if (index == 0)
      info = "Raised at";
    else
      info = "Re-raised at";
  } else {
    if (index == 0)
      info = "Raised by primitive operation at";
    else
      info = "Called from";
  }
  if (li->loc_is_inlined) {
    inlined = " (inlined)";
  } else {
    inlined = "";
  }
  if (li->loc_kind == CAML_LOC_KIND_UNKNOWN) {
    fprintf(stderr, "%s unknown location%s\n", info, inlined);
  } else {
    fprintf (stderr, "%s %s in file \"%s\"%s, line %d, characters %d-%d\n",
             info, li->loc_defname, li->loc_filename, inlined, li->loc_lnum,
             li->loc_startchr, li->loc_endchr);
  }
}

/* Print a backtrace */
CAMLexport void caml_print_exception_backtrace(void)
{
  int i;
  struct caml_loc_info li;
  debuginfo dbg;

  if (!caml_debug_info_available()) {
    fprintf(stderr, "(Cannot print stack backtrace: "
                    "no debug information available)\n");
    return;
  }

  caml_backtrace_ring_finish ();

  for (i = 0; i < Caml_state->backtrace_pos; i++) {
    for (dbg = caml_debuginfo_extract(Caml_state->backtrace_buffer[i]);
         dbg != NULL;
         dbg = caml_debuginfo_next(dbg))
    {
      caml_debuginfo_location(dbg, &li);
      print_location(&li, i);
    }
  }

  /* See also printexc.ml */
  switch (caml_debug_info_status()) {
  case FILE_NOT_FOUND:
    fprintf(stderr,
            "(Cannot print locations:\n "
             "bytecode executable program file not found)\n");
    break;
  case BAD_BYTECODE:
    fprintf(stderr,
            "(Cannot print locations:\n "
             "bytecode executable program file appears to be corrupt)\n");
    break;
  case WRONG_MAGIC:
    fprintf(stderr,
            "(Cannot print locations:\n "
             "bytecode executable program file has wrong magic number)\n");
    break;
  case NO_FDS:
    fprintf(stderr,
            "(Cannot print locations:\n "
             "bytecode executable program file cannot be opened;\n "
             "-- too many open files. Try running with OCAMLRUNPARAM=b=2)\n");
    break;
  }
}

/* Return the status of loading backtrace information (error reporting in
   bytecode) */
CAMLprim value caml_ml_debug_info_status(value unit)
{
  return Val_int(caml_debug_info_status());
}

/* Get a copy of the latest backtrace */
CAMLprim value caml_get_exception_raw_backtrace(value unit)
{
  CAMLparam0();
  CAMLlocal1(res);

  if (!Caml_state->backtrace_active ||
      Caml_state->backtrace_buffer == NULL ||
      Caml_state->backtrace_pos == 0) {
    res = caml_alloc(0, 0);
  }
  else {
    intnat i, len;

    caml_backtrace_ring_finish ();
    len = Caml_state->backtrace_pos;

    res = caml_alloc(len, 0);
    for (i = 0; i < len; i++)
      Field(res, i) = Val_backtrace_slot(Caml_state->backtrace_buffer[i]);
  }

  CAMLreturn(res);
}

/* Copy back a backtrace and exception to the global state.
   This function should be used only with Printexc.raw_backtrace */
/* noalloc (caml value): so no CAMLparam* CAMLreturn* */
CAMLprim value caml_restore_raw_backtrace(value exn, value backtrace)
{
  intnat i;
  mlsize_t bt_size;

  Caml_state->backtrace_last_exn = exn;

  bt_size = Wosize_val(backtrace);
  if(bt_size > BACKTRACE_BUFFER_SIZE){
    bt_size = BACKTRACE_BUFFER_SIZE;
  }

  /* We don't allocate if the backtrace is empty (no -g or backtrace
     not activated) */
  if(bt_size == 0){
    Caml_state->backtrace_pos = 0;
    return Val_unit;
  }

  /* Allocate if needed and copy the backtrace buffer */
  if (Caml_state->backtrace_buffer == NULL &&
      caml_alloc_backtrace_buffer() == -1) {
    return Val_unit;
  }

  Caml_state->backtrace_pos = bt_size;
  for(i=0; i < bt_size; i++){
    Caml_state->backtrace_buffer[i] = Backtrace_slot_val(Field(backtrace, i));
  }
  caml_backtrace_ring_restore ();

  return Val_unit;
}

#define Val_debuginfo(bslot) (Val_long((uintnat)(bslot)>>1))
#define Debuginfo_val(vslot) ((debuginfo)(Long_val(vslot) << 1))

/* Convert the raw backtrace to a data structure usable from OCaml */
static value caml_convert_debuginfo(debuginfo dbg)
{
  CAMLparam0();
  CAMLlocal3(p, fname, dname);
  struct caml_loc_info li;

  caml_debuginfo_location(dbg, &li);

  if (li.loc_kind == CAML_LOC_KIND_KNOWN) {
    fname = caml_copy_string(li.loc_filename);
    dname = caml_copy_string(li.loc_defname);
    p = caml_alloc_small(7, 0);
    Field(p, 0) = Val_bool(li.loc_is_raise);
    Field(p, 1) = fname;
    Field(p, 2) = Val_int(li.loc_lnum);
    Field(p, 3) = Val_int(li.loc_startchr);
    Field(p, 4) = Val_int(li.loc_endchr);
    Field(p, 5) = Val_bool(li.loc_is_inlined);
    Field(p, 6) = dname;
#ifdef ARCH_SIXTYFOUR
  } else
    if (li.loc_kind == CAML_LOC_KIND_REPEATED) {
      int cycle_len = li.loc_is_raise ;
      int ncycles = li.loc_lnum;
      p = caml_alloc_small(2, 2);
      Field(p, 0) = Val_int(cycle_len);
      Field(p, 1) = Val_int(ncycles);
#endif
  } else {
    p = caml_alloc_small(1, 1);
    Field(p, 0) = Val_bool(li.loc_is_raise);
  }

  CAMLreturn(p);
}

CAMLprim value caml_convert_raw_backtrace_slot(value slot)
{
  if (!caml_debug_info_available())
    caml_failwith("No debug information available");

  return (caml_convert_debuginfo(Debuginfo_val(slot)));
}

/* Convert the raw backtrace to a data structure usable from OCaml */
CAMLprim value caml_convert_raw_backtrace(value bt)
{
  CAMLparam1(bt);
  CAMLlocal1(array);
  intnat i, index;

  if (!caml_debug_info_available())
    caml_failwith("No debug information available");

  for (i = 0, index = 0; i < Wosize_val(bt); ++i)
  {
    debuginfo dbg;
    //fprintf (stderr, "caml_convert_raw_backtrace field[%ld]\n", i);
    for (dbg = caml_debuginfo_extract(Backtrace_slot_val(Field(bt, i)));
         dbg != NULL;
         dbg = caml_debuginfo_next(dbg))
      index++;
  }

  array = caml_alloc(index, 0);

  for (i = 0, index = 0; i < Wosize_val(bt); ++i)
  {
    debuginfo dbg;
    //fprintf (stderr, "caml_convert_raw_backtrace field[%ld]\n", i);
    for (dbg = caml_debuginfo_extract(Backtrace_slot_val(Field(bt, i)));
         dbg != NULL;
         dbg = caml_debuginfo_next(dbg))
    {
      Store_field(array, index, caml_convert_debuginfo(dbg));
      index++;
    }
  }

  CAMLreturn(array);
}

CAMLprim value caml_raw_backtrace_length(value bt)
{
  return Val_int(Wosize_val(bt));
}

CAMLprim value caml_raw_backtrace_slot(value bt, value index)
{
  uintnat i;
  debuginfo dbg;

  i = Long_val(index);
  if (i >= Wosize_val(bt))
    caml_invalid_argument("Printexc.get_raw_backtrace_slot: "
                          "index out of bounds");
  dbg = caml_debuginfo_extract(Backtrace_slot_val(Field(bt, i)));
  return Val_debuginfo(dbg);
}

CAMLprim value caml_raw_backtrace_next_slot(value slot)
{
  debuginfo dbg;

  CAMLparam1(slot);
  CAMLlocal1(v);

  dbg = Debuginfo_val(slot);
  dbg = caml_debuginfo_next(dbg);

  if (dbg == NULL)
    v = Val_int(0); /* None */
  else
  {
    v = caml_alloc(1, 0);
    Field(v, 0) = Val_debuginfo(dbg);
  }

  CAMLreturn(v);
}

/* the function below is deprecated: we previously returned directly
   the OCaml-usable representation, instead of the raw backtrace as an
   abstract type, but this has a large performance overhead if you
   store a lot of backtraces and print only some of them.

   It is not used by the Printexc library anymore, or anywhere else in
   the compiler, but we have kept it in case some user still depends
   on it as an external.  */
CAMLprim value caml_get_exception_backtrace(value unit)
{
  CAMLparam0();
  CAMLlocal3(arr, res, backtrace);
  intnat i;

  if (!caml_debug_info_available()) {
    res = Val_int(0); /* None */
  } else {
    backtrace = caml_get_exception_raw_backtrace(Val_unit);

    arr = caml_alloc(Wosize_val(backtrace), 0);
    for (i = 0; i < Wosize_val(backtrace); i++) {
      backtrace_slot slot = Backtrace_slot_val(Field(backtrace, i));
      debuginfo dbg = caml_debuginfo_extract(slot);
      Store_field(arr, i, caml_convert_debuginfo(dbg));
    }

    res = caml_alloc_small(1, 0); Field(res, 0) = arr; /* Some */
  }

  CAMLreturn(res);
}

CAMLprim value caml_get_current_callstack(value max_frames_value)
{
  CAMLparam1(max_frames_value);
  CAMLlocal1(res);
  value* callstack = NULL;
  intnat callstack_alloc_len = 0;
  intnat callstack_len =
    caml_collect_current_callstack(&callstack, &callstack_alloc_len,
                                   Long_val(max_frames_value), -1);
  res = caml_alloc(callstack_len, 0);
  memcpy(Op_val(res), callstack, sizeof(value) * callstack_len);
  caml_stat_free(callstack);
  CAMLreturn(res);
}
