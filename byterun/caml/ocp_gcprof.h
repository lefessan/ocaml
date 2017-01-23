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


#ifndef CAML_OCP_GCPROF_H
#define CAML_OCP_GCPROF_H

#include "mlvalues.h"

#ifdef __cplusplus
extern "C" {
#endif

/********************************************************************/
/*                                                                  */
/*                     Continuous Profiling                         */
/*                                                                  */
/********************************************************************/

/* This MACRO can be used to completely remove the GcProf patch */
#define DISABLE_GCPROF

#ifdef DISABLE_GCPROF

#define GCPROF_HEADER(hd,kind)
#define GCPROF_GC_PHASE(gc_phase, gc_subphase)
#define GCPROF_PREPARE_MINOR()
#define GCPROF_MINOR_SCAN()
#define GCPROF_MAJOR_SCAN(kind)
#define GCPROF_INTERN_INIT(wosize)
#define GCPROF_INTERN_ALLOC(hp, kind)
#define GCPROF_INTERN_FINISH()
#define GCPROF_WATERMARK(name,kind)

#else /* DISABLE_GCPROF */

#define GCPROF_MINOR_ALLOC     0
#define GCPROF_MINOR_PROMOTE   1
#define GCPROF_MAJOR_ALLOC     2
#define GCPROF_MAJOR_FREE      3
#define GCPROF_MAJOR_SCANNED   4
#define GCPROF_MAJOR_DARKEN    5
#define GCPROF_NTABLES         6
#define GCPROF_COMPACT_ALLOC   7
#define GCPROF_COMPACT_FREE    8

#define Phase_minor            4

/* We still have some work to do with compaction: we should scan all
blocks before compaction, mark them in MAJOR_FREE, and then scan
them again after compaction, marking them as MAJOR_ALLOC. */

#define Phase_compact          5
#define Subphase_retry        15

/* This variable is non-nul if GC profiling has been activated. */
extern int caml_gcprof_flag;

#define GCPROF_HEADER(hd,kind)                              \
  if ( caml_gcprof_flag )				    \
    caml_gcprof_header( hd, GCPROF_##kind);

#define GCPROF_GC_PHASE(gc_phase, gc_subphase)   	    \
  if ( caml_gcprof_flag )      			            \
    caml_gcprof_gc_phase(gc_phase,gc_subphase);

#define GCPROF_PREPARE_MINOR()                              \
  if ( caml_gcprof_flag )                                   \
    caml_gcprof_minor_prepare();

#define GCPROF_MINOR_SCAN()                                 \
  if ( caml_gcprof_flag )                                   \
    caml_gcprof_minor_scan();

#define GCPROF_MAJOR_SCAN(kind)                             \
  if ( caml_gcprof_flag )                                   \
    caml_gcprof_major_scan( GCPROF_##kind);

/* store the wosize of the block being created, and ask to
   skip recording the next allocation.  */
#define GCPROF_INTERN_INIT(wosize)                          \
  if ( caml_gcprof_flag )				    \
    caml_gcprof_intern_init(wosize)
/* store the block allocated for interning the values, and
   the kind of tables in which records should be stored. */
#define GCPROF_INTERN_ALLOC(hp, kind)                       \
  if ( caml_gcprof_flag )				    \
    caml_gcprof_intern_alloc(hp, GCPROF_##kind)
/* iter in the block on the values newly created, and
   call GCPROF_HEADER on each of them. */
#define GCPROF_INTERN_FINISH()                              \
  if ( caml_gcprof_flag )				    \
    caml_gcprof_intern_finish()

#endif /* DISABLE_GCPROF */

  
/* #define GCPROF_WATERMARK(s) caml_gcprof_watermark(s) */
#ifdef MEMPROF_INSIDE

CAMLextern void ocp_gcprof_header(value hp, int kind);
CAMLextern void ocp_gcprof_gc_phase(int gc_phase, int gc_subphase);
CAMLextern void ocp_gcprof_minor_prepare();
CAMLextern void ocp_gcprof_minor_scan();
CAMLextern void ocp_gcprof_major_scan(int kind);
CAMLextern void ocp_gcprof_init();
CAMLextern void ocp_gcprof_exit();

/* These macros are used in the specific case of intern.c, where
   allocations are performed inline. */
CAMLextern void ocp_gcprof_intern_init(mlsize_t wosize);
CAMLextern void ocp_gcprof_intern_alloc(header_t* hp, int kind);
CAMLextern void ocp_gcprof_intern_finish();
CAMLextern void ocp_gcprof_watermark(char*,int);


#else /* !MEMPROF_INSIDE */

CAMLextern void caml_gcprof_header(value hp, int kind);
CAMLextern void caml_gcprof_gc_phase(int gc_phase, int gc_subphase);
CAMLextern void caml_gcprof_minor_prepare();
CAMLextern void caml_gcprof_minor_scan();
CAMLextern void caml_gcprof_major_scan(int kind);
CAMLextern void caml_gcprof_init();
CAMLextern void caml_gcprof_exit();

/* These macros are used in the specific case of intern.c, where
   allocations are performed inline. */
CAMLextern void caml_gcprof_intern_init(mlsize_t wosize);
CAMLextern void caml_gcprof_intern_alloc(header_t* hp, int kind);
CAMLextern void caml_gcprof_intern_finish();
CAMLextern void caml_gcprof_watermark(char*,int);

#endif /* MEMPROF_INSIDE */
  
#ifdef __cplusplus
}
#endif

#endif // CAML_OCP_GCPROF_H
