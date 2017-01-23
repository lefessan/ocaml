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


#ifndef CAML_OCP_UTILS_H
#define CAML_OCP_UTILS_H

#include "mlvalues.h"

#ifdef __cplusplus
extern "C" {
#endif

CAMLextern void * caml_memprof_dll_handle;
CAMLextern void* caml_memprof_load_symbol(char *name);
  
#define VOID_MEMPROF_STUB0(caml_name, ocp_name)                         \
  CAMLexport void caml_name(){                                          \
    static void (*ocp_name)() = NULL;                                   \
    if( caml_memprof_dll_handle != NULL ){                              \
      if( ocp_name == NULL ) {                                          \
        ocp_name =                                                      \
          caml_memprof_load_symbol(#ocp_name);                           \
      }                                                                 \
      ocp_name();                                                       \
    }                                                                   \
  }

#define VOID_MEMPROF_STUB1(caml_name, ocp_name, ty1,arg1)               \
  CAMLexport void caml_name(ty1 arg1){                                  \
    static void (*ocp_name) (ty1 arg1) = NULL;                          \
    if( caml_memprof_dll_handle != NULL ){                              \
      if( ocp_name == NULL ) {                                          \
        ocp_name =                                                      \
          caml_memprof_load_symbol(#ocp_name);                           \
      }                                                                 \
      ocp_name(arg1);                                                   \
    }                                                                   \
  }

#define VOID_MEMPROF_STUB2(caml_name, ocp_name, ty1,arg1, ty2, arg2)    \
  CAMLexport void caml_name(ty1 arg1, ty2 arg2){                        \
    static void (*ocp_name) (ty1 arg1, ty2 arg2) = NULL;                \
    if( caml_memprof_dll_handle != NULL ){                              \
      if( ocp_name == NULL ) {                                          \
        ocp_name =                                                      \
          caml_memprof_load_symbol(#ocp_name);                           \
      }                                                                 \
      ocp_name(arg1,arg2);                                              \
    }                                                                   \
  }

#define VOID_MEMPROF_STUB3(caml_name, ocp_name, ty1,arg1, ty2, arg2, ty3, arg3) \
  CAMLexport void caml_name(ty1 arg1, ty2 arg2, ty3 arg3){              \
    static void (*ocp_name) (ty1 arg1, ty2 arg2, ty3 arg3) = NULL;      \
    if( caml_memprof_dll_handle != NULL ){                              \
      if( ocp_name == NULL ) {                                          \
        ocp_name =                                                      \
          caml_memprof_load_symbol(#ocp_name);                           \
      }                                                                 \
      ocp_name(arg1,arg2,arg3);                                         \
    }                                                                   \
  }


#define RET_MEMPROF_STUB0(rety, caml_name, ocp_name, retv)              \
  CAMLexport rety caml_name(){                                          \
    static rety (*ocp_name)() = NULL;                                   \
    if( caml_memprof_dll_handle != NULL ){                              \
      if( ocp_name == NULL ) {                                          \
        ocp_name =                                                      \
          caml_memprof_load_symbol(#ocp_name);                           \
      }                                                                 \
      return ocp_name();                                                \
    } else {                                                            \
      return retv;                                                      \
    }                                                                   \
  }

#define RET_MEMPROF_STUB1(rety, caml_name, ocp_name, ty1,arg1, retv)    \
  CAMLexport rety caml_name(ty1 arg1){                                  \
    static rety (*ocp_name) (ty1 arg1) = NULL;                          \
    if( caml_memprof_dll_handle != NULL ){                              \
      if( ocp_name == NULL ) {                                          \
        ocp_name =                                                      \
          caml_memprof_load_symbol(#ocp_name);                           \
      }                                                                 \
      return ocp_name(arg1);                                            \
    } else {                                                            \
      return retv;                                                      \
    }                                                                   \
  }

#define RET_MEMPROF_STUB2(rety, caml_name, ocp_name, ty1,arg1, ty2, arg2, retv) \
  CAMLexport rety caml_name(ty1 arg1, ty2 arg2){                        \
    static rety (*ocp_name) (ty1 arg1, ty2 arg2) = NULL;                \
    if( caml_memprof_dll_handle != NULL ){                              \
      if( ocp_name == NULL ) {                                          \
        ocp_name =                                                      \
          caml_memprof_load_symbol(#ocp_name);                           \
      }                                                                 \
      return ocp_name(arg1,arg2);                                       \
    } else {                                                            \
      return retv;                                                      \
    }                                                                   \
  }

#define RET_MEMPROF_STUB3(rety, caml_name, ocp_name, ty1,arg1, ty2, arg2, ty3, arg3, retv) \
  CAMLexport rety caml_name(ty1 arg1, ty2 arg2, ty3 arg3){              \
    static rety (*ocp_name) (ty1 arg1, ty2 arg2, ty3 arg3) = NULL;      \
    if( caml_memprof_dll_handle != NULL ){                              \
      if( ocp_name == NULL ) {                                          \
        ocp_name =                                                      \
          caml_memprof_load_symbol(#ocp_name);                           \
      }                                                                 \
      return ocp_name(arg1,arg2,arg3);                                  \
    } else {                                                            \
      return retv;                                                      \
    }                                                                   \
  }

#ifdef __cplusplus
}
#endif

#endif // CAML_OCP_UTILS_H
