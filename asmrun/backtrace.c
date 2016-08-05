/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Gallium, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2006 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* Stack backtrace for uncaught exceptions */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "alloc.h"
#include "stack.h"
#include "backtrace.h"
#include "memory.h"
#include "misc.h"
#include "mlvalues.h"
#include "memprof.h"

struct backtrace_item {
  frame_descr *backtrace_descriptor;
  int backtrace_count;
};

CAMLexport code_t * caml_after_stackoverflow = NULL;
int caml_backtrace_active = 0;
int caml_backtrace_pos = 0;
backtrace_item_t * caml_backtrace_buffer = NULL;
value caml_backtrace_last_exn = Val_unit;
#define BACKTRACE_BUFFER_SIZE 1024

/* In order to prevent the GC from walking through the debug information
   (which have no headers), we transform frame_descr pointers into
   31/63 bits ocaml integers by shifting them by 1 to the right. We do
   not lose information as descr pointers are aligned.

   In particular, we do not need to use [caml_initialize] when setting
   an array element with such a value.
*/
#define Val_Descrptr(descr) Val_long((uintnat)descr>>1)
#define Descrptr_Val(v) ((frame_descr *) (Long_val(v)<<1))

/* Start or stop the backtrace machinery */

CAMLprim value caml_record_backtrace(value vflag)
{
  int flag = Int_val(vflag);

  if (flag != caml_backtrace_active) {
    caml_backtrace_active = flag;
    caml_backtrace_pos = 0;
    if (flag) {
      caml_register_global_root(&caml_backtrace_last_exn);
    } else {
      caml_remove_global_root(&caml_backtrace_last_exn);
    }
  }
  return Val_unit;
}

/* Return the status of the backtrace machinery */

CAMLprim value caml_backtrace_status(value vunit)
{
  return Val_bool(caml_backtrace_active);
}

/* When [caml_backtrace_pos] reaches [BACKTRACE_BUFFER_SIZE], we have
normally no space to record the top of the backtrace. Here, we create
a space with [TOP_OF_BACKTRACE] entries, called
[top_of_backtrace]. [caml_backtrace_pos] is set to a negative
value. Descriptors are stored in this *cyclic* buffer. When leaving
[caml_stash_backtrace], we call [clean_backtrace] to copy
[top_of_backtrace] to the standard backtrace buffer, taking only the
[TOP_OF_BACKTRACE_SIZE] top entries. We add a NULL pointer as a frame
descriptor, that is printed as "<<< backtrace cut here >>>".
 */
#define TOP_OF_BACKTRACE_SIZE 64
static frame_descr* top_of_backtrace[TOP_OF_BACKTRACE_SIZE];
static void clean_backtrace()
{
  if(caml_backtrace_pos < 0){
    int i;
    backtrace_item_t* p = &caml_backtrace_buffer[BACKTRACE_BUFFER_SIZE-1];
    caml_backtrace_pos++;
    for(i=0; i<TOP_OF_BACKTRACE_SIZE && caml_backtrace_pos < 0; i++){
      frame_descr* d = 
        top_of_backtrace[ (1-caml_backtrace_pos) % TOP_OF_BACKTRACE_SIZE ];
      caml_backtrace_pos++;
      p->backtrace_descriptor = d;
      p->backtrace_count = 0x10;
      p--;
    }
    p->backtrace_descriptor = NULL;
    p->backtrace_count = 0x10;
    caml_backtrace_pos = BACKTRACE_BUFFER_SIZE;
  }
}

static void really_store_backtrace_item(frame_descr* d)
{
  if(caml_backtrace_pos < 0){
    top_of_backtrace[ (1-caml_backtrace_pos) % TOP_OF_BACKTRACE_SIZE ] = d;
    caml_backtrace_pos--;
  } else {
    backtrace_item_t *p = &caml_backtrace_buffer[caml_backtrace_pos++];
    p -> backtrace_descriptor = d;
    p -> backtrace_count = 0x10;
  }
}

static void store_backtrace_descr(frame_descr* d)
{
  backtrace_item_t *prev1, *prev2, *prev3;
  if( caml_backtrace_pos < 4 )
    return really_store_backtrace_item(d);

  prev1 = &caml_backtrace_buffer[caml_backtrace_pos-1];
  if ( prev1->backtrace_descriptor == d && 
       (prev1->backtrace_count & 0xf) == 0 ){
    prev1->backtrace_count += 0x10;
    return;
  } 

  prev2 = &caml_backtrace_buffer[caml_backtrace_pos-2];
  prev3 = &caml_backtrace_buffer[caml_backtrace_pos-3];
  if(
     (prev2->backtrace_descriptor == d) &&
     (prev1->backtrace_descriptor == prev3->backtrace_descriptor) &&
     (prev2->backtrace_count == prev3->backtrace_count) &&
     ( (prev2->backtrace_count & 0x01) == 0x01 ||
      prev2->backtrace_count == 0x10) &&
     (prev1->backtrace_count == 0x10)
     ) {
       caml_backtrace_pos--;
       prev2->backtrace_count = (prev2->backtrace_count + 0x10) | 0x01;
       prev3->backtrace_count = prev2->backtrace_count;
       return;
  }
  really_store_backtrace_item(d);
}


/* returns the next frame descriptor (or NULL if none is available),
   and updates *pc and *sp to point to the following one.  */

frame_descr * caml_next_frame_descriptor(uintnat * pc, char ** sp)
{
  frame_descr * d;
  uintnat h;

  if (caml_frame_descriptors == NULL) caml_init_frame_descriptors();

  while (1) {
    h = Hash_retaddr(*pc);
    while (1) {
      d = caml_frame_descriptors[h];
      if (d == 0) return NULL; /* can happen if some code compiled without -g */
      if (d->retaddr == *pc) break;
      h = (h+1) & caml_frame_descriptors_mask;
    }
    /* Skip to next frame */
    if (d->frame_size != 0xFFFF) {
      /* Regular frame, update sp/pc and return the frame descriptor */
#ifndef Stack_grows_upwards
      *sp += (d->frame_size & 0xFFFC);
#else
      *sp -= (d->frame_size & 0xFFFC);
#endif
      *pc = Saved_return_address(*sp);
#ifdef Mask_already_scanned
      *pc = Mask_already_scanned(*pc);
#endif
      return d;
    } else {
      /* Special frame marking the top of a stack chunk for an ML callback.
         Skip C portion of stack and continue with next ML stack chunk. */
      struct caml_context * next_context = Callback_link(*sp);
      *sp = next_context->bottom_of_stack;
      *pc = next_context->last_retaddr;
      /* A null sp means no more ML stack chunks; stop here. */
      if (*sp == NULL) return NULL;
    }
  }
}

/* Stores the return addresses contained in the given stack fragment
   into the backtrace array ; this version is performance-sensitive as
   it is called at each [raise] in a program compiled with [-g], so we
   preserved the global, statically bounded buffer of the old
   implementation -- before the more flexible
   [caml_get_current_callstack] was implemented. */

extern int caml_in_sigaltstack(char* ptr);

void caml_stash_backtrace(value exn, uintnat pc, char * sp, char * trapsp)
{
  if (exn != caml_backtrace_last_exn) {
    caml_backtrace_pos = 0;
    caml_backtrace_last_exn = exn;
  }
  if (caml_backtrace_buffer == NULL) {
    Assert(caml_backtrace_pos == 0);
    caml_backtrace_buffer = malloc(BACKTRACE_BUFFER_SIZE * sizeof(backtrace_item_t));
    if (caml_backtrace_buffer == NULL) return;
  }

  if( caml_in_sigaltstack((char*) &sp)
      && ((char*)caml_bottom_of_stack - (char*)caml_after_stackoverflow) != 32776 ){

  /* 

     We are after a stack-overflow: [caml_in_sigaltstack] is true, so we
     can use the value of [caml_after_stackoverflow].

     After a stack-overflow, we arrive here using
     [caml_bottom_of_stack] for [sp]. However, [caml_bottom_of_stack]
     is only updated on [caml_c_call], but we might have done a lot of
     work since the last C call.  In Typerex, we save the value of
     %rsp in [caml_after_stackoverflow] in the [segv_handler], so it
     might be possible to recover some information. Unfortunately,
     this %rsp is not a frame descriptor, so we should probably try to
     walk the stack upward to find the return address.
     
     Since calls to C touch the stack with an offset of 32776, we can
     compare the two values, and use the heuristics that a different
     offset means that OCaml code has overflowed the stack, in which
     case we cannot use [caml_bottom_of_stack].

     We should then use [caml_after_stackoverflow] to discover a
     correct starting position in the stack. We can just search the
     stack for a correct frame_descr.

   */
    frame_descr * d;
    uintnat h;
    int found = 0;
    
    // fprintf(stderr, "STACK OVERFLOW BACKTRACE RECOVERY MODE...\n");
    if (caml_frame_descriptors == NULL) caml_init_frame_descriptors();

    sp = (char*)caml_after_stackoverflow;

    /* let's forget the top of the stack */
    sp += 1024 * sizeof(void*);
    while(!found && sp < caml_bottom_of_stack ){
      /* No stack grows upwards these days... */
      sp += sizeof(void*);
      pc = Saved_return_address(sp);
      h = Hash_retaddr(pc);
      while (1) {
        d = caml_frame_descriptors[h];
        if (d == 0) break;
        if (d->retaddr == pc){ found = 1; break; }
        h = (h+1) & caml_frame_descriptors_mask;
      }
    }
    pc = Saved_return_address(sp);
  }

  /* iterate on each frame  */
  while (1) {
    frame_descr * descr = caml_next_frame_descriptor(&pc, &sp);
    if (descr == NULL) {
      return;
    }
    /* store its descriptor in the backtrace buffer */
    if (caml_backtrace_pos >= BACKTRACE_BUFFER_SIZE){
      /* OCaml would just stop here, but we prefer to do something 
         different */
      caml_backtrace_pos = -1;
    }
    store_backtrace_descr(descr);
    /* Stop when we reach the current exception handler */
#ifndef Stack_grows_upwards
    if (sp > trapsp) return;
#else
    if (sp < trapsp) return;
#endif
  }
}

/* Stores upto [max_frames_value] frames of the current call stack to
   return to the user. This is used not in an exception-raising
   context, but only when the user requests to save the trace
   (hopefully less often). Instead of using a bounded buffer as
   [caml_stash_backtrace], we first traverse the stack to compute the
   right size, then allocate space for the trace. */

CAMLprim value caml_get_current_callstack(value max_frames_value) {
  CAMLparam1(max_frames_value);
  CAMLlocal1(trace);

  /* we use `intnat` here because, were it only `int`, passing `max_int`
     from the OCaml side would overflow on 64bits machines. */
  intnat max_frames = Long_val(max_frames_value);
  intnat trace_size;

  /* first compute the size of the trace */
  {
    uintnat pc = caml_last_return_address;
    /* note that [caml_bottom_of_stack] always points to the most recent
     * frame, independently of the [Stack_grows_upwards] setting */
    char * sp = caml_bottom_of_stack;
    char * limitsp = caml_top_of_stack;

    trace_size = 0;
    while (1) {
      frame_descr * descr = caml_next_frame_descriptor(&pc, &sp);
      if (descr == NULL) break;
      if (trace_size >= max_frames) break;
      ++trace_size;

#ifndef Stack_grows_upwards
      if (sp > limitsp) break;
#else
      if (sp < limitsp) break;
#endif
    }
  }

  trace = caml_alloc((mlsize_t) trace_size, 0);

  /* then collect the trace */
  {
    uintnat pc = caml_last_return_address;
    char * sp = caml_bottom_of_stack;
    intnat trace_pos;

    for (trace_pos = 0; trace_pos < trace_size; trace_pos++) {
      frame_descr * descr = caml_next_frame_descriptor(&pc, &sp);
      Assert(descr != NULL);
      Field(trace, trace_pos) = Val_Descrptr(descr);
    }
  }

  CAMLreturn(trace);
}

/* Extract location information for the given frame descriptor */

void caml_extract_location_info(frame_descr * d,
                                  /*out*/ struct caml_loc_info * li)
{
  uintnat infoptr;
  uint32 info1, info2;

  if(d == NULL){
    li->loc_valid = 1;
    li->loc_is_raise = 1;
    li->loc_filename = "long backtrace cut here =========";
    li->loc_lnum = -1;
    li->loc_startchr = 0;
    li->loc_endchr = 0;
    return;
  }
  /* If no debugging information available, print nothing.
     When everything is compiled with -g, this corresponds to
     compiler-inserted re-raise operations. */
  if ((d->frame_size & 1) == 0) {
    li->loc_valid = 0;
    li->loc_is_raise = 1;
    return;
  }
  /* Recover debugging info */
  infoptr = ((uintnat) d +
             sizeof(char *) + sizeof(short) + sizeof(short) +
             sizeof(short) * d->num_live + sizeof(frame_descr *) - 1)
            & -sizeof(frame_descr *);
  info1 = ((uint32 *)infoptr)[0];
  info2 = ((uint32 *)infoptr)[1];
  /* Format of the two info words:
       llllllllllllllllllll aaaaaaaa bbbbbbbbbb nnnnnnnnnnnnnnnnnnnnnnnn kk
                          44       36         26                       2  0
                       (32+12)    (32+4)
     k ( 2 bits): 0 if it's a call, 1 if it's a raise
     n (24 bits): offset (in 4-byte words) of file name relative to infoptr
     l (20 bits): line number
     a ( 8 bits): beginning of character range
     b (10 bits): end of character range */
  li->loc_valid = 1;
  li->loc_is_raise = (info1 & 3) != 0;
  li->loc_filename = (char *) infoptr + (info1 & 0x3FFFFFC);
  li->loc_lnum = info2 >> 12;
  li->loc_startchr = (info2 >> 4) & 0xFF;
  li->loc_endchr = ((info2 & 0xF) << 6) | (info1 >> 26);
}

/* Print location information -- same behavior as in Printexc

   note that the test for compiler-inserted raises is slightly redundant:
     (!li->loc_valid && li->loc_is_raise)
   caml_extract_location_info above guarantees that when li->loc_valid is
   0, then li->loc_is_raise is always 1, so the latter test is
   useless. We kept it to keep code identical to the byterun/
   implementation. */

static void print_location(struct caml_loc_info * li, int index, char* indent)
{
  char * info;

  /* Ignore compiler-inserted raise */
  if (!li->loc_valid && li->loc_is_raise){
    return;
  }

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
  if (! li->loc_valid) {
    fprintf(stderr, "%s%s unknown location\n", indent, info);
  } else {
    fprintf (stderr, "%s%s file \"%s\", line %d, characters %d-%d\n",
             indent,
             info, li->loc_filename, li->loc_lnum,
             li->loc_startchr, li->loc_endchr);
  }
}

/* Print a backtrace */

void caml_print_exception_backtrace(void)
{
  int i;
  struct caml_loc_info li;
  int count;

  clean_backtrace();
  for (i = 0; i < caml_backtrace_pos; i++) {
    caml_extract_location_info(caml_backtrace_buffer[i].backtrace_descriptor, &li);
    count = caml_backtrace_buffer[i].backtrace_count;
    if( count == 0x10 ){
      print_location(&li, i,"");
    } else
      if( (count & 0x01) == 0x01 ){
        fprintf(stderr, "Mutual recursion called %d times:\n", count >> 4);
        print_location(&li, i,"  [1] ");
        i++;
        caml_extract_location_info(caml_backtrace_buffer[i].backtrace_descriptor, &li);
        print_location(&li, i,"  [2] ");
      } else {
        fprintf(stderr, "Recursion called %d times:\n", count >> 4);
        print_location(&li, i,"  * ");
      }
  }
}

/* Convert the raw backtrace to a data structure usable from OCaml */

CAMLprim value caml_convert_raw_backtrace_slot(value backtrace_slot) {
  CAMLparam1(backtrace_slot);
  CAMLlocal2(p, fname);
  struct caml_loc_info li;

  caml_extract_location_info(Descrptr_Val(backtrace_slot), &li);

  if (li.loc_valid) {
    fname = caml_copy_string(li.loc_filename);
    p = caml_alloc_small(5, 0);
    Field(p, 0) = Val_bool(li.loc_is_raise);
    Field(p, 1) = fname;
    Field(p, 2) = Val_int(li.loc_lnum);
    Field(p, 3) = Val_int(li.loc_startchr);
    Field(p, 4) = Val_int(li.loc_endchr);
  } else {
    p = caml_alloc_small(1, 1);
    Field(p, 0) = Val_bool(li.loc_is_raise);
  }

  CAMLreturn(p);
}

/* Get a copy of the latest backtrace */

CAMLprim value caml_get_exception_raw_backtrace(value unit)
{
  CAMLparam0();
  CAMLlocal1(res);
  const int tag = 0;

  /* Beware: the allocations below may cause finalizers to be run, and another
     backtrace---possibly of a different length---to be stashed (for example
     if the finalizer raises then catches an exception).  We choose to ignore
     any such finalizer backtraces and return the original one. */

  if (caml_backtrace_buffer == NULL || caml_backtrace_pos == 0) {
    res = caml_alloc(0, tag);
  } else {
    int backtrace_size = 0;

    if(caml_backtrace_buffer != NULL){
      int i;
      clean_backtrace();
      for(i=0; i<caml_backtrace_pos; i++)
        backtrace_size += caml_backtrace_buffer[i].backtrace_count >> 4;    
      if(backtrace_size > BACKTRACE_BUFFER_SIZE) 
        backtrace_size = BACKTRACE_BUFFER_SIZE;
    }

    {
      code_t saved_caml_backtrace_buffer[BACKTRACE_BUFFER_SIZE];
      int i;
      
      backtrace_size = 0;
      for(i=0; i<caml_backtrace_pos; i++){
        int count = caml_backtrace_buffer[i].backtrace_count >> 4;
        int repeat = caml_backtrace_buffer[i].backtrace_count & 0x01;
        int j;
        for(j=0; j < count; j++){
          saved_caml_backtrace_buffer[backtrace_size] = (code_t)
            caml_backtrace_buffer[i].backtrace_descriptor;
          backtrace_size++;
          if(backtrace_size == BACKTRACE_BUFFER_SIZE){
            i = caml_backtrace_pos; break; /* end of both for loops */
          }
          if( repeat ){
            saved_caml_backtrace_buffer[backtrace_size] = (code_t)
              caml_backtrace_buffer[i+1].backtrace_descriptor;
            backtrace_size++;
            if(backtrace_size == BACKTRACE_BUFFER_SIZE){
              i = caml_backtrace_pos; break; /* end of both for loops */
            }
          }
        }
        if(repeat) i++;
      }
            
      res = caml_alloc(backtrace_size, tag);
      for (i = 0; i < backtrace_size; i++) {
        /* [Val_Descrptr] always returns an immediate. */
        Field(res, i) = Val_Descrptr(saved_caml_backtrace_buffer[i]);
      }
    }
  }

  CAMLreturn(res);
}

/* the function below is deprecated: we previously returned directly
   the OCaml-usable representation, instead of the raw backtrace as an
   abstract type, but this has a large performance overhead if you
   store a lot of backtraces and print only some of them.

   It is not used by the Printexc library anymore, or anywhere else in
   the compiler, but we have kept it in case some user still depends
   on it as an external.
*/

CAMLprim value caml_get_exception_backtrace(value unit)
{
  CAMLparam0();
  CAMLlocal3(arr, res, backtrace);
  intnat i;

  backtrace = caml_get_exception_raw_backtrace(Val_unit);

  arr = caml_alloc(Wosize_val(backtrace), 0);
  for (i = 0; i < Wosize_val(backtrace); i++) {
    Store_field(arr, i, caml_convert_raw_backtrace_slot(Field(backtrace, i)));
  }

  res = caml_alloc_small(1, 0);
  Field(res, 0) = arr; /* Some */
  CAMLreturn(res);
}

