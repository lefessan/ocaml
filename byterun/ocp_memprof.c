/**************************************************************************/
/*                                                                        */
/*  Copyright 2014, OCamlPro. All rights reserved.                        */
/*                                                                        */
/* Contributors:                                                          */
/* * Gregoire Henry (OCamlPro)                                            */
/* * Çagdas Bozman  (OCamlPro)                                            */
/* * Fabrice Le Fessant (INRIA/OCamlPro)                                  */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

#include "caml/mlvalues.h"

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
#include "caml/alloc.h"
#include "caml/intext.h"
#include "caml/memory.h"
#include "caml/sys.h"
#include "caml/fix_code.h"
#include "caml/osdeps.h"

#include "caml/version.h"
#include "caml/commit.h"

#include <stdio.h>
#include <errno.h>


#include "caml/ocp_memprof.h"
#include "caml/ocp_utils.h"

#ifdef WITH_MEMPROF
CAMLexport uintnat caml_memprof_ccall_locid = PROF_INIT;
CAMLexport uintnat caml_memprof_exception_locid = PROF_EXCEPTION;
#else
CAMLexport uintnat caml_memprof_ccall_locid = 0;
CAMLexport uintnat caml_memprof_exception_locid = 0;
#endif


RET_MEMPROF_STUB3(int, caml_memprof_register_loc_table,
                  ocp_memprof_register_loc_table,
                  char*, table,mlsize_t, len, int, elems, 0)


/* Needed to prevent the GC from calling caml_memprof_next_thread
  during compaction, since it would access the thread_id which is
  stored in a heap block, not accessible during Compact.invert_at. */
CAMLexport int caml_heapdump_in_dump = 0;





























