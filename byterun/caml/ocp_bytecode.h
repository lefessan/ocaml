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


#ifndef CAML_OCP_BYTECODE_H
#define CAML_OCP_BYTECODE_H

#include "mlvalues.h"

#ifdef __cplusplus
extern "C" {
#endif
  
  //#define OCP_BYTECODE_DISABLED
  
CAMLextern void caml_ocp_bytecode_init(char *memprof_section);
CAMLextern  code_t caml_ocp_bytecode_fix_locids(code_t src_code, asize_t* src_size);

/* This table indicates, for a given instruction in the main code segment,
   the offset with the original instruction (used for backtraces...). */
CAMLextern code_t caml_ocp_bytecode_translation_table;

CAMLextern int caml_ocp_bytecode_has_locid(opcode_t);
  
#ifdef __cplusplus
}
#endif

#endif // CAML_OCP_BYTECODE_H
