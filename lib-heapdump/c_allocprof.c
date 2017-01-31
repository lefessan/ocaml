/**************************************************************************/
/*                                                                        */
/*   Copyright 2014-2017 OCamlPro SAS --- typerex-memprof                 */
/*                                                                        */
/*   Do not redistribute, without permission of OCamlPro, source          */
/*   or binary copies of this software.                                   */
/*                                                                        */
/**************************************************************************/

#define MEMPROF_INSIDE

#include "caml/internals/mlvalues.h"

/* ocpwin: for getpid */
#ifdef HAS_UNISTD
#include <unistd.h>
#endif
#ifdef _WIN32
#include <wtypes.h>
#include <winbase.h>
/* for getpid: */
#include <process.h>
#endif

#include <string.h>
#include "caml/internals/alloc.h"
#include "caml/internals/intext.h"
#include "caml/internals/memory.h"
#include "caml/internals/memprof.h"
#include "caml/internals/sys.h"

#include "c_gcprof.h"

#include <stdio.h>
#include <errno.h>

/********************************************************************/
/*                                                                  */
/*                     Allocation Profiling                         */
/*                                                                  */
/********************************************************************/

#ifndef DISABLE_ALLOCPROF


#ifdef NATIVE_CODE

#include "caml/internals/stack.h"
#define OCP_NEED_LOCINFO
#include "caml/internals/backtrace.h"

#define CAML_MORE_ALLOC_H
#include "caml/internals/memprof.h"

static char* allocprof_filename = NULL;

static uintnat max_stats_level = 0;
static uintnat max_stats_friends = 0;
CAMLexport uintnat ocp_gcprof_profile_period = 0;
CAMLexport int ocp_gcprof_profile_major = 0;

static alloc_stats * caml_frame_stats_old = NULL;

static void init_caml_frame_stats()
{
  intnat tblsize, i;

  if( caml_frame_descriptors == NULL ) caml_init_frame_descriptors();

  /* by definition of caml_frame_descriptors_mask */
  tblsize = caml_frame_descriptors_mask + 1;

  caml_frame_stats =
    (alloc_stats*) caml_stat_alloc( (tblsize+1) * sizeof(alloc_stats));
  memset(caml_frame_stats, 0, (tblsize+1) * sizeof(alloc_stats));
  
  for (i = 0; i < tblsize; i++) {
    frame_descr *d = caml_frame_descriptors[i];
    if( d != NULL){
      caml_frame_stats[i].d = d;
    }
  }
  /* Use the position of caml_frame_stats as a marker of end ! */
  caml_frame_stats[tblsize].d = (frame_descr*) &caml_frame_stats;

  if( caml_frame_stats_old != NULL){
    /* We need to remember the previous stats */

    alloc_stats* ptr = caml_frame_stats_old;
    while( ptr->d != (frame_descr*) &caml_frame_stats ){
      if( ptr->d != NULL ){
	uintnat pc = ptr->d->retaddr;
	uintnat h = Hash_retaddr(pc);
	while(1) {
	  frame_descr *d = caml_frame_descriptors[h];
	  if (d->retaddr == pc) break;
	  h = (h+1) & caml_frame_descriptors_mask;
	}
	/*	caml_frame_stats[h].top_pos = ptr->top_pos;
		caml_frame_stats[h].any_pos = ptr->any_pos; */
	memcpy( &caml_frame_stats[h], ptr, sizeof( alloc_stats ) );
      }
      ptr++;
    }
    caml_stat_free(caml_frame_stats_old);
  }
  caml_frame_stats_old = caml_frame_stats;
  

}

CAMLexport void ocp_allocprof_record_alloc(uintnat size){
  char * sp;
  uintnat retaddr;
  frame_descr * d;
  uintnat first_frame = 1;
  static uintnat current_sample = 1;
  uintnat level = 0;
  uintnat last_frame = 0;

  /*  fprintf(stderr, "caml_record_minor_gc!\n"); */
  if (caml_frame_descriptors == NULL) caml_init_frame_descriptors();

  /* pass 1 : walk the stack to measure its size and fill some stats */

  current_sample++;
  sp = caml_bottom_of_stack;
  retaddr = caml_last_return_address;

  if (sp != NULL) {

    if( caml_frame_stats == NULL) init_caml_frame_stats();
    
    while (1) {
      /* Find the descriptor corresponding to the return address */
      uintnat h = Hash_retaddr(retaddr);
      while(1) {
        d = caml_frame_descriptors[h];
        if (d->retaddr == retaddr) break;
        h = (h+1) & caml_frame_descriptors_mask;
      }
      if (d->frame_size != 0xFFFF) {
	if ((d->frame_size & 1) != 0) {
	  if( caml_frame_stats[h].last_sample != current_sample ){
	    alloc_stats *s = &caml_frame_stats[h];

	    if ( first_frame > 0 ){
	      s->top_pos = s->top_pos+size;
	      first_frame = 0;
	    }
	    s->any_pos = s->any_pos+size;
	    s->last_sample = current_sample;
	    level++;

	    if( last_frame > 0 ){
	      int i;
	      uintnat current_stats = size;

	      for( i=0; i< MAX_STATS_FRIENDS; i++){
		if( s->friends[i] == last_frame ){
		  current_stats = s->friends_stats[i]+size;
		  break;
		}
		if( s->friends[i] == 0 ) break;
	      }
	      if( i == MAX_STATS_FRIENDS ) {
		i = MAX_STATS_FRIENDS-1;
		max_stats_friends = 1;
	      }

	      while(i>0 &&
		    current_stats > s->friends_stats[i-1]){
		i--;
		s->friends[i+1] = s->friends[i];
		s->friends_stats[i+1] = s->friends_stats[i];
	      }
	      s->friends[i] = last_frame;
	      s->friends_stats[i] = current_stats;
	    }
	    last_frame = h+1;
	  }
	  /*
	    {
	    struct caml_loc_info li;
	    caml_extract_location_info(d, &li);
	    fprintf(stderr, "frame(%lx):", d);
	    fprintf (stderr, "%ld in file \"%s\", line %d, characters %d-%d\n",
   	    caml_frame_stats[h].minor_gcs,
		   li.loc_filename, li.loc_lnum,
		   li.loc_startchr, li.loc_endchr);
		   }
	  */
	}

        /* Move to next frame */
#ifndef Stack_grows_upwards
        sp += (d->frame_size & 0xFFFC);
#else
        sp -= (d->frame_size & 0xFFFC);
#endif
        retaddr = Saved_return_address(sp);
      } else {
        /* This marks the top of a stack chunk for an ML callback.
           Skip C portion of stack and continue with next ML stack chunk. */
        struct caml_context * next_context = Callback_link(sp);
        sp = next_context->bottom_of_stack;
        retaddr = next_context->last_retaddr;
        /* A null sp means no more ML stack chunks; stop here. */
        if (sp == NULL) break;
      }
    }
  }
  if( level > max_stats_level && level < MAX_STATS_LEVELS ) max_stats_level = level;

  /* pass 2 : walk the stack to measure its size and fill some stats */
  current_sample++;
  sp = caml_bottom_of_stack;
  retaddr = caml_last_return_address;

  if (sp != NULL) {
    while (1) {
      /* Find the descriptor corresponding to the return address */
      uintnat h = Hash_retaddr(retaddr);
      while(1) {
        d = caml_frame_descriptors[h];
        if (d->retaddr == retaddr) break;
        h = (h+1) & caml_frame_descriptors_mask;
      }
      if (d->frame_size != 0xFFFF) {
	if ((d->frame_size & 1) != 0) {

	  if( caml_frame_stats[h].last_sample != current_sample ){
	    caml_frame_stats[h].last_sample = current_sample;
	    level--;

	    if( level < MAX_STATS_LEVELS ){
	      caml_frame_stats[h].levels[level] = caml_frame_stats[h].levels[level] + size;
	    }
	  }
	  /*
	    {
	    struct caml_loc_info li;
	    caml_extract_location_info(d, &li);
	    fprintf(stderr, "frame(%lx):", d);
	    fprintf (stderr, "%ld in file \"%s\", line %d, characters %d-%d\n",
   	    caml_frame_stats[h].minor_gcs,
		   li.loc_filename, li.loc_lnum,
		   li.loc_startchr, li.loc_endchr);
		   }
	  */
	}

        /* Move to next frame */
#ifndef Stack_grows_upwards
        sp += (d->frame_size & 0xFFFC);
#else
        sp -= (d->frame_size & 0xFFFC);
#endif
        retaddr = Saved_return_address(sp);
      } else {
        /* This marks the top of a stack chunk for an ML callback.
           Skip C portion of stack and continue with next ML stack chunk. */
        struct caml_context * next_context = Callback_link(sp);
        sp = next_context->bottom_of_stack;
        retaddr = next_context->last_retaddr;
        /* A null sp means no more ML stack chunks; stop here. */
        if (sp == NULL) break;
      }
    }
  }
}

void dump_alloc_stats(void){
  if( ocp_gcprof_profile_period > 0 ){

    alloc_stats* ptr = caml_frame_stats;
    struct caml_loc_info li;
    int i;
    uintnat loc = 0;
    FILE *output = fopen(allocprof_filename, "w");

    if( caml_frame_stats == NULL && caml_frame_stats_old != NULL )
      init_caml_frame_stats();
    if( caml_frame_stats != NULL ){
      if( max_stats_friends > 0 ) max_stats_friends = 65536;
      
      while( ptr->d != (frame_descr*) &caml_frame_stats ){
        if( ptr->d != NULL &&
            ((ptr->d->frame_size & 1) != 0) &&
            ptr->any_pos > 0){
          caml_extract_location_info(ptr->d, &li);
	  /*	  fprintf(output, "frame(%lx):", ptr->d); */
          fprintf (output, "LOC %ld in file \"%s\", line %d, characters %d-%d\n",
		   loc,
		   li.loc_filename, li.loc_lnum,
		   li.loc_startchr, li.loc_endchr);
          fprintf (output, "ANY %ld %ld\n", ptr->any_pos, loc);
          if( ptr->top_pos > 0 )
            fprintf (output, "TOP %ld %ld\n", ptr->top_pos, loc);
          for(i=0; i<max_stats_level; i++){
            if( ptr->levels[i] > 0 ){
              fprintf (output, "LVL %d %ld %ld\n", i, ptr->levels[i], loc);
            }
          }
          for(i=0; i < MAX_STATS_FRIENDS; i++){
            if( ptr->friends[i] > 0 ){
              if(i>max_stats_friends) max_stats_friends = i;
              fprintf (output, "LNK %d %ld %ld %ld\n",
		       i, loc, ptr->friends[i]-1, ptr->friends_stats[i]);
            }
          }
        }
        ptr++;
        loc++;
      }
    }
    fprintf(output, "MXF %ld\n", max_stats_friends);
    fclose(output);
  }
}

CAMLexport void ocp_allocprof_init()
{
  if( ocp_gcprof_profile_period == 0){
    char *filename = getenv("OCP_ALLOCPROF_FILE");

    if( filename != NULL){
      char *opt = getenv("OCP_ALLOCPROF_PARAMS");

      allocprof_filename = filename;

      ocp_gcprof_profile_period = 10*1024; /* 10k */
      ocp_gcprof_profile_major = 0; /* don't profile major allocs by default */

      if (opt != NULL){
        while (*opt != '\0'){
          switch (*opt++){
          case 'P': ocp_memprof_scanmult (opt, &ocp_gcprof_profile_period);
            break;
          case 'M': ocp_gcprof_profile_major = 1;
            break;
          }
        }
      }
      if(ocp_gcprof_profile_period < 1024){
        fprintf(stderr, "Warning: you cannot set the profile period under 1k in\n");
        fprintf(stderr, "ocp-allocprof. You should use ocp-gcprof for this.\n");
        fprintf(stderr, "Using profile period to 1k.\n");
        ocp_gcprof_profile_period = 1024;
      }

    }
  }
}

CAMLexport void ocp_allocprof_exit()
{
  if( ocp_gcprof_profile_period > 1){
    dump_alloc_stats();
  }
}

CAMLexport void ocp_allocprof_future_minor_alloc(char* young_ptr, 
                                                  mlsize_t size_plus_header)
{
  if( caml_gcprof_flag & GCPROF_MODE_BACKTRACES){
    ocp_gcprof_future_minor_alloc(young_ptr, size_plus_header);
  }
  /*
  fprintf(stderr, "alloc: min: 0x%lx lim: 0x%lx old: 0x%lx ptr: 0x%lx max: 0x%lx\n",
          caml_young_start, caml_young_limit, caml_allocprof_young_ptr,
          caml_young_ptr, caml_young_end);
  fprintf(stderr, "alloc block: 0x%lx + %d = 0x%lx\n",
          young_ptr, size_plus_header,
          young_ptr + size_plus_header);
  */
}


/* Here, we should backup previously allocated minor block. */
CAMLexport void ocp_allocprof_minor_collection()
{
  if( caml_gcprof_flag & GCPROF_MODE_BACKTRACES){
    ocp_gcprof_minor_collection();
  }
  /*
  fprintf(stderr, "minor: min: 0x%lx lim: 0x%lx old: 0x%lx ptr: 0x%lx max: 0x%lx\n",
          caml_young_start, caml_young_limit, caml_allocprof_young_ptr,
          caml_young_ptr, caml_young_end);
  */
}

CAMLexport void ocp_allocprof_alloc_major(mlsize_t sz, tag_t tag,
                                           profiling_t locid)
{
  if( caml_gcprof_flag & GCPROF_MODE_BACKTRACES){
    ocp_gcprof_alloc_major(sz, tag, locid);
  }
}

#else // NATIVE_CODE

CAMLexport void ocp_allocprof_init(int from_gcprof){
}

CAMLexport void ocp_allocprof_exit(void){
}

CAMLexport void ocp_allocprof_future_minor_alloc(char* young_ptr, 
                                                  mlsize_t size_plus_header)
{
}
CAMLexport void ocp_allocprof_minor_collection()
{
}
CAMLexport void ocp_allocprof_alloc_major(mlsize_t sz, tag_t tag,
                                           profiling_t locid)
{
}













#endif  // NATIVE_CODE

#endif  // DISABLE_ALLOCPROF


