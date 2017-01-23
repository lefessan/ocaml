/**************************************************************************/
/*                                                                        */
/*  Copyright 2014, OCamlPro. All rights reserved.                        */
/*                                                                        */
/* Contributors:                                                          */
/* * Gregoire Henry (OCamlPro)                                            */
/* * Cagdas Bozman  (OCamlPro)                                            */
/* * Fabrice Le Fessant (INRIA/OCamlPro)                                  */
/*                                                                        */
/**************************************************************************/


#ifndef CAML_OCP_ALLOCPROF_H
#define CAML_OCP_ALLOCPROF_H

#include "mlvalues.h"

#ifdef __cplusplus
extern "C" {
#endif
  

/********************************************************************/
/*                                                                  */
/*                     Allocation Profiling                         */
/*                                                                  */
/********************************************************************/

/* This MACRO can be used to completely remove the GcProf patch */
#define DISABLE_ALLOCPROF

#if defined(DISABLE_ALLOCPROF) || !defined(NATIVE_CODE)

#define ALLOCPROF_SMALL_ALLOC(wosize)
#define ALLOCPROF_BEGIN_GARBAGE_COLLECTION()
  /*  caml_young_limit = caml_young_start; */
#define ALLOCPROF_END_GARBAGE_COLLECTION()
#define ALLOCPROF_MINOR_COLLECTION()
#define ALLOCPROF_SET_YOUNG_LIMIT()           
  /*  caml_young_limit = caml_young_start; */
#define ALLOCPROF_ALLOC_MAJOR(wosize, tag, locid)



#else // DISABLE_ALLOCPROF

#define GCPROF_MODE_ACTIVE        1
#define GCPROF_MODE_LOGGING       2
#define GCPROF_MODE_COUNTERS      4
#define GCPROF_MODE_COUNTERS_GC   8
#define GCPROF_MODE_MINOR_GC     16
#define GCPROF_MODE_THREADS      32
#define GCPROF_MODE_BACKTRACES   64
#define GCPROF_MODE_GCTIME      128


/* This variable is non-null if alloc-profiling is activated */
CAMLextern char *caml_allocprof_young_ptr;

/* in signals_asm.c:caml_garbage_collection()  */
#define ALLOCPROF_BEGIN_GARBAGE_COLLECTION()                          \
  int size_plus_header = 0;                                           \
  char *old_caml_young_ptr = caml_young_ptr;                          \
  if( caml_gcprof_flag &  GCPROF_MODE_BACKTRACES){                    \
    size_plus_header = caml_allocprof_young_ptr - caml_young_ptr;     \
  } else {                                                            \
    caml_young_limit = caml_young_start;                              \
  }

/* in signals_asm.c:caml_garbage_collection()  
   We need to set caml_young_limit so that it will not trigger
   a new call to caml_garbage_collection, unless some signals
   did allocate.
*/
#define ALLOCPROF_END_GARBAGE_COLLECTION()                              \
  if( (caml_gcprof_flag &  GCPROF_MODE_BACKTRACES) &&                   \
      caml_young_ptr == old_caml_young_ptr ){                           \
    caml_young_ptr = caml_allocprof_young_ptr;                          \
    caml_allocprof_young_ptr = caml_young_ptr - size_plus_header;       \
    if( caml_allocprof_young_ptr >= caml_young_start ){                 \
      caml_young_limit = caml_allocprof_young_ptr - 1;                  \
      caml_allocprof_future_minor_alloc(caml_allocprof_young_ptr,       \
                                        size_plus_header);              \
    } else {                                                            \
      caml_young_limit = caml_young_end;                                \
    }                                                                   \
  }

#define ALLOCPROF_SMALL_ALLOC(wosize)                                   \
  if( caml_gcprof_flag &  GCPROF_MODE_BACKTRACES ){                     \
    caml_allocprof_future_minor_alloc(caml_young_ptr,Bhsize_wosize (wosize)); \
    caml_allocprof_young_ptr = caml_young_ptr;                          \
    caml_young_limit = caml_allocprof_young_ptr - 1;                    \
  }

/* in minor_gc.c:caml_minor_collection() */
#define ALLOCPROF_MINOR_COLLECTION()                          \
  if( caml_gcprof_flag &  GCPROF_MODE_BACKTRACES )            \
    caml_allocprof_minor_collection();

/* in minor_gc.c:caml_set_minor_heap_size() and caml_empty_minor_heap()

  If alloc-profiling is on, set [caml_young_limit] so that it will
  trigger a call to [caml_garbage_collection] immediatly at the next
  minor allocation. Also save current value of caml_young_ptr in
  caml_allocprof_young_ptr.
*/
#define ALLOCPROF_SET_YOUNG_LIMIT()                                   \
  if( caml_gcprof_flag &  GCPROF_MODE_BACKTRACES ){                   \
      caml_young_limit = caml_young_end-1;                            \
      caml_allocprof_young_ptr = caml_young_end;                      \
  } else {                                                            \
    caml_young_limit = caml_young_start;                              \
  }

/* in memory.c: caml_alloc_shr_loc() */
#define ALLOCPROF_ALLOC_MAJOR(wosize, tag, locid)                     \
  if( (caml_gcprof_flag &  GCPROF_MODE_BACKTRACES)                    \
      && !caml_in_minor_collection )                                  \
    caml_allocprof_alloc_major(wosize, tag, locid)

#endif // DISABLE_ALLOCPROF || !NATIVE_CODE

#ifdef MEMPROF_INSIDE

CAMLextern void ocp_allocprof_init();
CAMLextern void ocp_allocprof_exit();
CAMLextern void ocp_allocprof_minor_collection();
CAMLextern void ocp_allocprof_future_minor_alloc(char* young_ptr, 
                                                  mlsize_t size_plus_header);
CAMLextern void ocp_allocprof_alloc_major(mlsize_t sz, tag_t tag, uintnat locid);

#else /* !MEMPROF_INSIDE */

CAMLextern void caml_allocprof_init();
CAMLextern void caml_allocprof_exit();
CAMLextern void caml_allocprof_minor_collection();
CAMLextern void caml_allocprof_future_minor_alloc(char* young_ptr, 
                                                  mlsize_t size_plus_header);
CAMLextern void caml_allocprof_alloc_major(mlsize_t sz, tag_t tag, uintnat locid);

#endif /* MEMPROF_INSIDE */

#ifdef __cplusplus
}
#endif

#endif // CAML_OCP_ALLOCPROF_H
