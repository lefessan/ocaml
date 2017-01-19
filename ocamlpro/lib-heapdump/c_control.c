/**************************************************************************/
/*                                                                        */
/*   Typerex Tools                                                        */
/*                                                                        */
/*   Copyright 2011-2017 OCamlPro SAS                                     */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU General Public License version 3 described in the file       */
/*   LICENSE.                                                             */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

#include "caml/mlvalues.h"
#include "caml/fail.h"
#include "caml/memory.h"
#include "caml/alloc.h"

#include "caml/ocp_utils.h"
#include "c_heapprof.h"

#define INPUT_OPCODE_DUMP 0

value ocp_memprof_control(value input_v)
{
  CAMLparam1(input_v);
  CAMLlocal1(output_v);

  output_v = Val_unit;
  if(Is_block(input_v)){
    switch( Tag_val(input_v) ){
    case INPUT_OPCODE_DUMP:
      caml_minor_collection();
      ocp_heapdump_do_dump(String_val(Field(input_v, 0)), HEAPDUMP_USER);
      break;
    default:
      caml_failwith("ocp_memprof_control: no such block opcode");
      break;
    }
  } else {
    caml_failwith("ocp_memprof_control: no such int opcode");
  }

  CAMLreturn(output_v);  
}



