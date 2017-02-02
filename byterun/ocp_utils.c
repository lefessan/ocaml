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

#include "caml/osdeps.h"

#include <stdio.h>
#include <errno.h>


#include "caml/ocp_utils.h"

int caml_hooked_signal = 0;
int (*caml_execute_signal_hook)(int) = NULL;






/* The handle to the dynlinked library. Note that, since now, we use
  cplugins, we might want to use another way to define our stubs.
  Let's see that later ! */
CAMLexport void * caml_memprof_dll_handle = NULL;


#ifdef NATIVE_CODE
#define MEMPROF_DLL "ocp-memprof-native.so"
#else
#define MEMPROF_DLL "ocp-memprof-bytecode.so"
#endif

CAMLexport void* caml_memprof_load_symbol(char *name)
{
  if( caml_memprof_dll_handle == NULL ){
    fprintf(stderr, "Error: trying to load %s while memprof DLL not loaded\n",
            name);
    exit(1);
  }
  void *ret =
    caml_dlsym(caml_memprof_dll_handle, name);
  if( ret == NULL ){
    fprintf(stderr, "Error: could not access symbol %s from ocp-memprof DLL\n",
            name);
    fprintf(stderr, "Reason: %s\n", caml_dlerror());
    exit(1);
  }
  return ret;
}


/* we first try to load 'ocp_memprof_init2' to provide more 
information to the plugin. If it fails, we call 'ocp_memprof_init'
providing no information :-( */

static void memprof_do_init()
{
  void (*ocp_memprof_init2)(struct memprof_init*) = NULL;  

   ocp_memprof_init2 = caml_dlsym(caml_memprof_dll_handle, 
                                  "ocp_memprof_init2");
   if( ocp_memprof_init2 == NULL ){
     void (*ocp_memprof_init)() = NULL;
     //        fprintf(stderr, "library %s loaded\n", library);
     ocp_memprof_init =
       caml_memprof_load_symbol("ocp_memprof_init");
     ocp_memprof_init();
   } else {
     struct memprof_init m;
     m.memprof_version = 0;
     m.ocaml_version = OCAML_VERSION_STRING;
     m.ocaml_commit = OCAML_COMMIT;
     m.ocaml_date = OCAML_DATE;
     ocp_memprof_init2( &m );
   }
}
  
CAMLexport void caml_ocp_utils_init()
{
  if( caml_memprof_dll_handle == NULL ){
    char *dir = getenv("OCP_MEMPROF_DLL_DIR");
    //    fprintf(stderr, "caml_memprof_init: begin\n");

    if(dir != NULL ){
      int path_len = strlen(dir);
      char *library = caml_stat_alloc(path_len + 30);
      // fprintf(stderr, "caml_memprof_init: enabled\n");
      sprintf(library, "%s/%s", dir, MEMPROF_DLL);
      if( getenv("OCP_MEMPROF_DLL_DEBUG") != NULL ){
        fprintf(stderr, "caml_memprof_init: loading %s\n", library);
      }
      caml_memprof_dll_handle = caml_dlopen(library, 1, 0);
      if( caml_memprof_dll_handle != NULL) {
        memprof_do_init();
        //        fprintf(stderr, "ocp_memprof_init executed.\n");
      } else {
        fprintf(stderr, "Error: could not load library %s\n", library);
        fprintf(stderr, "Reason: %s\n", caml_dlerror());
        exit(1);
      }
    } else {
      //      fprintf(stderr, "caml_memprof_init: disabled\n");
    }
  }
}



VOID_MEMPROF_STUB0(caml_memprof_exit2, ocp_memprof_exit)

void caml_memprof_exit()
{
  //  caml_memprof_exit_shared_mem();  
  caml_memprof_exit2();
}

#ifndef NATIVE_CODE
/* Actually, this should not be in the cplugin, but always in. */
VOID_MEMPROF_STUB1(caml_memprof_bytecode_init,
                   ocp_memprof_bytecode_init,
                   char *, memprof_section)
#endif
