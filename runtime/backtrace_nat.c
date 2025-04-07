/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 2006 Institut National de Recherche en Informatique et     */
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
#include "caml/backtrace.h"
#include "caml/backtrace_prim.h"
#include "caml/memory.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/stack.h"

/* Returns the next frame descriptor (or NULL if none is available),
   and updates *pc and *sp to point to the following one.  */
frame_descr * caml_next_frame_descriptor(uintnat * pc, char ** sp)
{
  frame_descr * d;
  uintnat h;

  while (1) {
    h = Hash_retaddr(*pc);
    while (1) {
      d = caml_frame_descriptors[h];
      if (d == NULL) return NULL; /* happens if some code compiled without -g */
      if (d->retaddr == *pc) break;
      h = (h+1) & caml_frame_descriptors_mask;
    }
    /* Skip to next frame */
    if (d->frame_size != 0xFFFF) {
      /* Regular frame, update sp/pc and return the frame descriptor */
      *sp += (d->frame_size & 0xFFFC);
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

int caml_alloc_backtrace_buffer(void){
  CAMLassert(Caml_state->backtrace_pos == 0);
  Caml_state->backtrace_buffer =
    caml_stat_alloc_noexc(BACKTRACE_BUFFER_SIZE * sizeof(backtrace_slot));
  if (Caml_state->backtrace_buffer == NULL) return -1;
  return 0;
}


/* A backtrace_slot is either a debuginfo or a frame_descr* */
#define Slot_is_debuginfo(s) ((uintnat)(s) & 2)
#define Debuginfo_slot(s) ((debuginfo)((uintnat)(s) - 2))
#define Slot_debuginfo(d) ((backtrace_slot)((uintnat)(d) + 2))
#define Frame_descr_slot(s) ((frame_descr*)(s))
#define Slot_frame_descr(f) ((backtrace_slot)(f))
static debuginfo debuginfo_extract(frame_descr* d, int alloc_idx);

#define BACKTRACE_RING_SIZE 64
#define BACKTRACE_BUFFER_HEAD (BACKTRACE_BUFFER_SIZE - BACKTRACE_RING_SIZE - 1)

/* Repeated only exists in 64 bits.
   It has two encodings:
   * As a backtrace_slot, it is tagged with `100` as ending bigs (4)
     (debuginfo has `10` and frame_descr has `000`)
   * As a debuginfo, it is tagged with `1`, whereas debuginfo is a pointer
     with 4-align, i.e. bits `00`.
   * Encoding of repeated is (lowest to hightest bits):
     * bit[3]: marker for slots/debuginfo
     * bit[4]: cycle_len
     * bit[32]: ncycles
     * bit[24]: unused
     * bit[1]:  removed when transformed into a value
    As a specific encoding, cycle_len = 0 means that we just skipped ncycles
      slots.
*/

#ifdef ARCH_SIXTYFOUR
#define Slot_is_repeated(s) ((uintnat)(s) & 4)
#define Repeated_of_slot(s) ((uintnat)(s) - 4)
#define Slot_of_repeated(cycle_len, ncycles) ( (backtrace_slot)(((cycle_len)<<3) + ((intnat)(ncycles)<<7) + 4))
#define Slot_repeated_cycle_len(d) ((((uintnat)d) >> 3 ) & 0xF)
#define Slot_repeated_ncycles(d) ((((uintnat)d) >> 7 ) & 0xFFFFFFFF)

#define Debuginfo_is_repeated(d) ((uintnat)(d) & 1)
#define Debuginfo_of_repeated(d) ((uintnat)(d) + 1)
#define Repeated_of_debuginfo(d) ((uintnat)(d) - 1)
#endif

/*
  Merged state:
  * empty ring
  * BACKTRACE_BUFFER_SIZE >= backtrace_pos > BACKTRACE_BUFFER_HEAD
    [
    X1;
    ...
    X899 ;
    X900 ;          // at position BACKTRACE_BUFFER_HEAD-1
    SKIPPED_MARKER ; // at position BACKTRACE_BUFFER_HEAD
    X970 ; // last entry of ring at position BACKTRACE_BUFFER_HEAD+1
    ...
    X998 ;
    X999 ; // head of buffer at position BACKTRACE_BUFFER_SIZE-1
    ];

  Unmerged state:
  * backtrace_pos < 0
  * ring:
    * last entry is at pos (backtrace_pos+1) % BACKTRACE_RING_SIZE
    * oldest entry is at pos (backtrace_pos+BACKTRACE_RING_SIZE) % BACKTRACE_RING_SIZE
  * buffer
    * entries are stored from 0 to BACKTRACE_BUFFER_HEAD-1

*/

/* In 32 bits, we can't know how many frames we skipped, we just use
   this marker. */
frame_descr skipped32bit_marker = { 0 };

/* copy the ring back into the buffer */
void caml_backtrace_ring_finish ()
{
  //fprintf(stderr, "caml_backtrace_ring_finish...\n");
  if (Caml_state->backtrace_pos < 0){
    int i;
    int ring_pos = -1-Caml_state->backtrace_pos;
    int ring_size = BACKTRACE_RING_SIZE;
    Caml_state->backtrace_pos = BACKTRACE_BUFFER_HEAD;

    /* TODO: use a different marker containing the size of
     * the ring */
    //fprintf(stderr, "caml_backtrace_ring_finish set[%ld]=MARKER\n", Caml_state->backtrace_pos);
#ifdef ARCH_SIXTYFOUR
    Caml_state->backtrace_buffer[ Caml_state->backtrace_pos++ ] =
      Slot_of_repeated(0,ring_pos);
#else
    Caml_state->backtrace_buffer[ Caml_state->backtrace_pos++ ] =
      &skipped32bit_marker;
#endif

    if (ring_pos < BACKTRACE_RING_SIZE){
      ring_size = ring_pos;
      ring_pos = 0;
    }
    for(i=0; i<ring_size; i++){
      //fprintf(stderr, "caml_backtrace_ring_finish set[%ld]=ring[%d]\n", Caml_state->backtrace_pos,
      //  (ring_pos + i) % BACKTRACE_RING_SIZE);
      Caml_state->backtrace_buffer[
        Caml_state->backtrace_pos++ ] =
        Caml_state->backtrace_ring[
          (ring_pos + i) % BACKTRACE_RING_SIZE ];
    }
  }
  //fprintf(stderr, "caml_backtrace_ring_finish done\n");
}

void caml_backtrace_ring_restore (void)
{
  int ring_size ;

  if (Caml_state->backtrace_is_limited) return;
  if (Caml_state->backtrace_pos <= BACKTRACE_BUFFER_HEAD) return;

  //fprintf(stderr, "caml_backtrace_ring_restore\n");
  ring_size =
    Caml_state->backtrace_pos - BACKTRACE_BUFFER_HEAD - 1;

  if (ring_size < BACKTRACE_BUFFER_SIZE){
    int i;

    for(i=0; i < ring_size; i++){
      //fprintf(stderr, "caml_backtrace_ring_restore ring[%d]=buffer[%d]\n",
      //  i, BACKTRACE_BUFFER_HEAD+1 + i
      //  );
      Caml_state->backtrace_ring[i] =
        Caml_state->backtrace_buffer[
          BACKTRACE_BUFFER_HEAD+1 + i];
    }
    Caml_state->backtrace_pos = -1-ring_size;
  } else {
    int i;
    int ring_pos = 0; /* TODO */

    for(i=0; i < ring_size; i++){
      Caml_state->backtrace_ring[
        (ring_pos + i) % BACKTRACE_RING_SIZE ] =
        Caml_state->backtrace_buffer[
          BACKTRACE_BUFFER_HEAD+1 + i];
    }
    Caml_state->backtrace_pos = -1-ring_size;
  }
  //fprintf(stderr, "caml_backtrace_ring_restore done\n");
}

void caml_backtrace_store (backtrace_slot d)
{
    /* TODO Fabrice maybe store in top of backtrace_buffer, maybe
       detect cycles.  Note that backtrace_pos should never reach
       BACKTRACE_BUFFER_HEAD, as this is a signal to know that the top
       of the backtrace_buffer was copied from the backtrace_ring.
     */
  //fprintf(stderr, "caml_backtrace_store...\n");
  /* In case the ring was merged into the buffer, we need to
     restore it before adding new entries */
  int backtrace_pos = Caml_state->backtrace_pos;
  if (Caml_state->backtrace_pos > BACKTRACE_BUFFER_HEAD){
    caml_backtrace_ring_restore ();
    backtrace_pos = Caml_state->backtrace_pos;
  }

  /* If we reached the head of the buffer, start storing into
   * the ring by setting a negative value to
   * backtrace_pos. Alloc the ring if needed */
  if (backtrace_pos == BACKTRACE_BUFFER_HEAD){
    if (Caml_state->backtrace_ring == NULL)
      Caml_state->backtrace_ring =
        caml_stat_alloc_noexc(BACKTRACE_RING_SIZE * sizeof(backtrace_slot));
    backtrace_pos = -1;
  }

  if (backtrace_pos < 0){
    //fprintf(stderr, "caml_backtrace_store ring[%ld]= frame\n",
    //  (-1 - Caml_state->backtrace_pos) % BACKTRACE_RING_SIZE);
    Caml_state->backtrace_ring[
      (-1 - backtrace_pos--) % BACKTRACE_RING_SIZE] = d;
  } else {
    /* TODO: we do not check for misaligned cycles,
     i.e. we could match the end of a cycle with a smaller cycle by
     mistake.
    */
    if (backtrace_pos>3 && !Caml_state->backtrace_no_cycles){
      backtrace_slot d0 = Caml_state->backtrace_buffer[backtrace_pos-1];
      backtrace_slot d1 = Caml_state->backtrace_buffer[backtrace_pos-2];
      if (d0 == d){
        if( Slot_is_repeated(d1) &&
            Slot_repeated_cycle_len(d1) == 1
          ){
          Caml_state->backtrace_buffer[backtrace_pos-2] =
            Slot_of_repeated(
              1,
              Slot_repeated_ncycles(d1)+1
              );
        } else {
          Caml_state->backtrace_buffer[backtrace_pos-1] =
            Slot_of_repeated(1,2);
          Caml_state->backtrace_buffer[backtrace_pos++] = d;
        }
      } else {
        backtrace_slot d2 = Caml_state->backtrace_buffer[backtrace_pos-3];
        backtrace_slot d3 = Caml_state->backtrace_buffer[backtrace_pos-4];
        if (d1 == d && d2 == d0){
          if( Slot_is_repeated(d3) &&
              Slot_repeated_cycle_len(d3) == 2
            ){
            Caml_state->backtrace_buffer[backtrace_pos-4] =
              Slot_of_repeated(
                2,
                Slot_repeated_ncycles(d3)+1
                );
            backtrace_pos--;
          } else {
            Caml_state->backtrace_buffer[backtrace_pos-4] =
              Slot_of_repeated(2,2);
            Caml_state->backtrace_buffer[backtrace_pos-3] = d2;
            Caml_state->backtrace_buffer[backtrace_pos-2] = d1;
            backtrace_pos--;
          }
        } else {
          backtrace_slot d4 = Caml_state->backtrace_buffer[backtrace_pos-5];
          backtrace_slot d5 = Caml_state->backtrace_buffer[backtrace_pos-6];
          if (d2 == d && d3 == d0 && d4 == d1){
            if( Slot_is_repeated(d5) &&
                Slot_repeated_cycle_len(d5) == 3
              ){
              Caml_state->backtrace_buffer[backtrace_pos-6] =
                Slot_of_repeated(
                  3,
                  Slot_repeated_ncycles(d5)+1
                  );
              backtrace_pos -= 2;
            } else {
              Caml_state->backtrace_buffer[backtrace_pos-6] =
                Slot_of_repeated(3,2);
              Caml_state->backtrace_buffer[backtrace_pos-5] = d4;
              Caml_state->backtrace_buffer[backtrace_pos-4] = d3;
              Caml_state->backtrace_buffer[backtrace_pos-3] = d2;
              backtrace_pos -= 2;
            }
          } else {
            Caml_state->backtrace_buffer[backtrace_pos++] = d;
          }
        }
      }
    } else {
      Caml_state->backtrace_buffer[backtrace_pos++] = d;
    }
  }
  Caml_state->backtrace_pos = backtrace_pos;
}


/* Stores the return addresses contained in the given stack fragment
   into the backtrace array ; this version is performance-sensitive as
   it is called at each [raise] in a program compiled with [-g], so we
   preserved the global, statically bounded buffer of the old
   implementation -- before the more flexible
   [caml_get_current_callstack] was implemented. */
void caml_stash_backtrace(value exn, uintnat pc, char * sp, char * trapsp)
{
  //fprintf(stderr, "caml_stash_backtrace...\n");
  if (exn != Caml_state->backtrace_last_exn) {
    Caml_state->backtrace_pos = 0;
    Caml_state->backtrace_last_exn = exn;
  }

  if (Caml_state->backtrace_buffer == NULL &&
      caml_alloc_backtrace_buffer() == -1)
    return;

  /* iterate on each frame  */
  while (1) {
    frame_descr * descr = caml_next_frame_descriptor(&pc, &sp);
    if (descr == NULL) return;

    if (Caml_state->backtrace_is_limited){
      if (Caml_state->backtrace_pos >= BACKTRACE_BUFFER_SIZE) return;
      Caml_state->backtrace_buffer[Caml_state->backtrace_pos++] =
        (backtrace_slot) descr;
    } else {
      /* store its descriptor in the backtrace buffer */
      caml_backtrace_store (Slot_frame_descr(descr));
    }
    /* Stop when we reach the current exception handler */
    if (sp > trapsp) return;
  }
}

#define Default_callstack_size 32
intnat caml_collect_current_callstack(value** ptrace, intnat* plen,
                                      intnat max_frames, int alloc_idx)
{
  uintnat pc = Caml_state->last_return_address;
  char * sp = Caml_state->bottom_of_stack;
  intnat trace_pos = 0;

  if (max_frames <= 0) return 0;
  if (*plen == 0) {
    value* trace =
      caml_stat_alloc_noexc(Default_callstack_size * sizeof(value));
    if (trace == NULL) return 0;
    *ptrace = trace;
    *plen = Default_callstack_size;
  }

  if (alloc_idx >= 0) {
    /* First frame has a Comballoc selector */
    frame_descr * descr = caml_next_frame_descriptor(&pc, &sp);
    debuginfo info;
    if (descr == NULL) return 0;
    info = debuginfo_extract(descr, alloc_idx);
    if (info != NULL) {
      CAMLassert(((uintnat)info & 3) == 0);
      (*ptrace)[trace_pos++] = Val_backtrace_slot(Slot_debuginfo(info));
    } else {
      (*ptrace)[trace_pos++] = Val_backtrace_slot(Slot_frame_descr(descr));
    }
  }

  while (trace_pos < max_frames) {
    frame_descr * descr = caml_next_frame_descriptor(&pc, &sp);
    if (descr == NULL) break;
    CAMLassert(((uintnat)descr & 3) == 0);
    if (trace_pos == *plen) {
      intnat new_len = *plen * 2;
      value * trace = caml_stat_resize_noexc(*ptrace, new_len * sizeof(value));
      if (trace == NULL) break;
      *ptrace = trace;
      *plen = new_len;
    }
    (*ptrace)[trace_pos++] = Val_backtrace_slot(Slot_frame_descr(descr));
  }

  return trace_pos;
}

static debuginfo debuginfo_extract(frame_descr* d, int alloc_idx)
{
  unsigned char* infoptr;
  uint32_t debuginfo_offset;

  /* The special frames marking the top of an ML stack chunk are never
     returned by caml_next_frame_descriptor, so should never reach here. */
  CAMLassert(d->frame_size != 0xffff);

  if ((d->frame_size & 1) == 0) {
    return NULL;
  }
  /* Recover debugging info */
  infoptr = (unsigned char*)&d->live_ofs[d->num_live];
  if (d->frame_size & 2) {
    CAMLassert(alloc_idx == -1 || (0 <= alloc_idx && alloc_idx < *infoptr));
    /* skip alloc_lengths */
    infoptr += *infoptr + 1;
    /* align to 32 bits */
    infoptr = Align_to(infoptr, uint32_t);
    /* select the right debug info for this allocation */
    if (alloc_idx != -1) {
      infoptr += alloc_idx * sizeof(uint32_t);
      if (*(uint32_t*)infoptr == 0) {
        /* No debug info for this particular allocation */
        return NULL;
      }
    } else {
      /* We don't care which alloc_idx we use, so use the first
         that has debug info. (e.g. this is a backtrace through a
         finaliser/signal handler triggered via a Comballoc alloc) */
      while (*(uint32_t*)infoptr == 0) {
        infoptr += sizeof(uint32_t);
      }
    }
  } else {
    /* align to 32 bits */
    infoptr = Align_to(infoptr, uint32_t);
    CAMLassert(alloc_idx == -1);
  }
  debuginfo_offset = *(uint32_t*)infoptr;
  CAMLassert(debuginfo_offset != 0 && (debuginfo_offset & 3) == 0);
  return (debuginfo)(infoptr + debuginfo_offset);
}

debuginfo caml_debuginfo_extract(backtrace_slot slot)
{
  if (Slot_is_debuginfo(slot)) {
    /* already a decoded debuginfo */
    return Debuginfo_slot(slot);
  } else
    if (slot == &skipped32bit_marker){
      return (debuginfo)Debuginfo_of_repeated(0);
    } else
#ifdef ARCH_SIXTYFOUR
  if (Slot_is_repeated(slot)) {
    return (debuginfo)Debuginfo_of_repeated(Repeated_of_slot(slot));
  } else
#endif
  {
    return debuginfo_extract(Frame_descr_slot(slot), -1);
  }
}

debuginfo caml_debuginfo_next(debuginfo dbg)
{
  uint32_t * infoptr;

  if (dbg == NULL)
    return NULL;

#ifdef ARCH_SIXTYFOUR
  if (Debuginfo_is_repeated(dbg)) return NULL;
#endif

  infoptr = dbg;
  if ((infoptr[0] & 1) == 0)
    /* No next debuginfo */
    return NULL;
  else
    /* Next debuginfo is after the two packed info fields */
    return (debuginfo*)(infoptr + 2);
}

/* Multiple names may share the same filename,
   so it is referenced as an offset instead of stored inline */
struct name_info {
  int32_t filename_offs;
  char name[1];
};

/* Extract location information for the given frame descriptor */
void caml_debuginfo_location(debuginfo dbg, /*out*/ struct caml_loc_info * li)
{
  uint32_t info1, info2;
  struct name_info * name_info;

  /* If no debugging information available, print nothing.
     When everything is compiled with -g, this corresponds to
     compiler-inserted re-raise operations. */
  if (dbg == NULL) {
    li->loc_kind = CAML_LOC_KIND_UNKNOWN;
    li->loc_is_raise = 1;
    li->loc_is_inlined = 0;
    return;
  }
#ifdef ARCH_SIXTYFOUR
  if (Debuginfo_is_repeated(dbg)) {
    int cycle_len = Slot_repeated_cycle_len ( dbg );
    int ncycles = Slot_repeated_ncycles (dbg);
    li->loc_kind = CAML_LOC_KIND_REPEATED;
    li->loc_is_raise = cycle_len ;
    li->loc_lnum = ncycles;
    return;
  }
#endif
  /* Recover debugging info */
  info1 = ((uint32_t *)dbg)[0];
  info2 = ((uint32_t *)dbg)[1];
  name_info = (struct name_info*)((char *) dbg + (info1 & 0x3FFFFFC));
  /* Format of the two info words:
       llllllllllllllllllll aaaaaaaa bbbbbbbbbb ffffffffffffffffffffffff k n
                         44       36         26                        2 1 0
                       (32+12)    (32+4)
     n ( 1 bit ): 0 if this is the final debuginfo
                  1 if there's another following this one
     k ( 1 bit ): 0 if it's a call
                  1 if it's a raise
     f (24 bits): offset (in 4-byte words) of file name relative to dbg
     l (20 bits): line number
     a ( 8 bits): beginning of character range
     b (10 bits): end of character range */
  li->loc_kind = CAML_LOC_KIND_KNOWN;
  li->loc_is_raise = (info1 & 2) == 2;
  li->loc_is_inlined = caml_debuginfo_next(dbg) != NULL;
  li->loc_defname = name_info->name;
  li->loc_filename =
    (char *)name_info + name_info->filename_offs;
  li->loc_lnum = info2 >> 12;
  li->loc_startchr = (info2 >> 4) & 0xFF;
  li->loc_endchr = ((info2 & 0xF) << 6) | (info1 >> 26);
}

value caml_add_debug_info(backtrace_slot start, value size, value events)
{
  return Val_unit;
}

value caml_remove_debug_info(backtrace_slot start)
{
  return Val_unit;
}

int caml_debug_info_available(void)
{
  return 1;
}

int caml_debug_info_status(void)
{
  return 1;
}
