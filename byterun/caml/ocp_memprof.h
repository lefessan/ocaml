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


#ifndef CAML_MEMPROF_H
#define CAML_MEMPROF_H

#define __OCP_OCAML 1

#include "mlvalues.h"

#ifdef __cplusplus
extern "C" {
#endif

/* Static locids used by the runtime */
#define PROF_DUMMY                 0
#define PROF_TODO                  1
#define PROF_MEMPROF               2
#define PROF_STARTUP               3
#define PROF_STARTUP_INPUT_VAL     4
#define PROF_INTERP                5
#define PROF_INIT                  6
#define PROF_EXCEPTION             7
#define PROF_LAST_EXTERNAL         8

  
#ifdef MEMPROF_INSIDE

CAMLextern int64_t ocp_memprof_seed(void);
CAMLextern void ocp_memprof_scanmult (char *opt, uintnat *var);
CAMLextern void ocp_dump_after_gc(void);
CAMLextern void ocp_memprof_init();
CAMLextern void ocp_memprof_exit();

#else

CAMLextern void caml_memprof_exit();

#endif



CAMLextern
  int caml_memprof_register_loc_table (char* table, mlsize_t len, int elems);

  
/* Needed to prevent the GC from calling caml_memprof_next_thread
  during compaction, since it would access the thread_id which is
  stored in a heap block, not accessible during Compact.invert_at. */
CAMLextern int caml_heapdump_in_dump;

  
#ifdef __cplusplus
}
#endif

#endif // CAML_MEMPROF_H
