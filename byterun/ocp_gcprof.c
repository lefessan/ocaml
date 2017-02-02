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


#include "caml/ocp_utils.h"
#include "caml/ocp_gcprof.h"

CAMLexport int caml_gcprof_flag = 0;


VOID_MEMPROF_STUB2( caml_gcprof_gc_phase, ocp_gcprof_gc_phase,
                    int, gc_phase, int, gc_subphase)
VOID_MEMPROF_STUB2( caml_gcprof_header, ocp_gcprof_header,value,hd,int,kind)
VOID_MEMPROF_STUB1( caml_gcprof_intern_init, ocp_gcprof_intern_init,
                    mlsize_t, wosize)
VOID_MEMPROF_STUB2( caml_gcprof_intern_alloc, ocp_gcprof_intern_alloc,
                    header_t*, hp, int, kind)
VOID_MEMPROF_STUB0( caml_gcprof_intern_finish, ocp_gcprof_intern_finish)
VOID_MEMPROF_STUB0( caml_gcprof_minor_prepare, ocp_gcprof_minor_prepare)
RET_MEMPROF_STUB0( int, caml_gcprof_record_minor_alloc,
                   ocp_gcprof_record_minor_alloc, 0)
VOID_MEMPROF_STUB2( caml_gcprof_record_major_alloc,
                    ocp_gcprof_record_major_alloc,
                    mlsize_t, wosize, uintnat, locid)
VOID_MEMPROF_STUB1( caml_gcprof_major_scan, ocp_gcprof_major_scan,
                    int, kind)
VOID_MEMPROF_STUB0( caml_gcprof_minor_scan, ocp_gcprof_minor_scan)
































