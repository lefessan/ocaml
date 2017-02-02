/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* In ocp-memprof, we need to add locids to instructions, while still
   ensuring some compatibility with OCaml bytecode. So, we iter on the
   bytecode, adding locids that are stored in the MEMP section, if 
   available.
 */

#define CAML_INTERNALS

#include "caml/config.h"

#ifdef HAS_UNISTD
#include <unistd.h>
#endif

#include "caml/debugger.h"
#include "caml/fix_code.h"
#include "caml/instruct.h"
#include "caml/intext.h"
#include "caml/md5.h"
#include "caml/memory.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/reverse.h"

#include "caml/ocp_bytecode.h"
#include "caml/ocp_memprof.h"



int caml_memprof_bytecode = 0;

#ifndef NATIVE_CODE

CAMLexport code_t caml_ocp_bytecode_translation_table = NULL;

#include <string.h>
#include <stdlib.h>
#include <stdio.h>

static int debug_print = 0;

#define XXX if(debug_print)

char* memprof_info = NULL;
void caml_ocp_bytecode_init(char *memprof_section){
  XXX fprintf(stderr, "memprof_info %ld\n", (value)memprof_info);
  memprof_info = memprof_section;
}

#ifdef OCP_BYTECODE_DISABLED

code_t caml_ocp_bytecode_fix_locids(code_t src_code, asize_t* src_size)
{
  return src_code;
}

CAMLexport int caml_ocp_bytecode_has_locid(opcode_t opcode)
{
  return 0;
}


#else // !OCP_BYTECODE_DISABLED

#ifndef ARCH_BIG_ENDIAN
#include "caml/reverse.h"
static void fixup_endianness_int(uint32_t * p)
{
  Reverse_32(p, p);
}
#else
#define fixup_endianness_int(p) (p)
#endif

static int getint()
{
  uint32_t p = *(uint32_t*) memprof_info;
  fixup_endianness_int(&p);
  memprof_info = memprof_info + sizeof(uint32_t);
  return p;
}

#include "caml/instruct.h"

CAMLexport int caml_ocp_bytecode_has_locid(opcode_t opcode)
{
  switch(opcode){
  case GRAB:
  case CLOSURE:
  case CLOSUREREC:
  case MAKEBLOCK:
  case MAKEBLOCK1:
  case MAKEBLOCK2:
  case MAKEBLOCK3:
  case MAKEFLOATBLOCK:
  case GETFLOATFIELD:
  case C_CALL1:
  case C_CALL2:
  case C_CALL3:
  case C_CALL4:
  case C_CALL5:
  case C_CALLN:
    return 1;
  default: return 0;
  }
}


/* This function modifies the bytecode instructions to insert the locids
   and the corresponding new instructions. After that, it updates the
   code fragments. Beware, instruction pointers saved with profiling
   are not compatible with instruction pointers saved without profiling.

   It would probably be possible to create a "translation" table.
*/

code_t caml_ocp_bytecode_fix_locids(code_t src_code, asize_t* src_size)
{
  int nlocids;
  int ntables;
  int final_src_index;
  int final_dst_index;
  int final_size;
  code_t dst_code;
  code_t translation_code;
  int dst_index = 0;
  int src_index = -1;
  int next_index = -1;
  int next_offset = -1;
  int base_id = -1;
  int opcode = -1;
  int cmo_offset = -1;
  int i, nargs;
  opcode_t* opcode_nargs;
  
  if( getenv("OCP_MEMP_DEBUG") != NULL ) debug_print = 1;
  if( getenv("OCP_MEMP_DISABLED") != NULL ) memprof_info = NULL;

  XXX fprintf(stderr, "memprof_info %ld\n", (value)memprof_info);
  
  opcode_nargs = caml_init_opcode_nargs();
  final_src_index = (*src_size) / sizeof(opcode_t);

  if( memprof_info == NULL ){
    XXX    fprintf(stderr, "computing nlocids to add\n");
    nlocids = 0;
    for(src_index = 0; src_index < final_src_index; ){
      opcode = src_code[src_index];
      XXX fprintf(stderr, "opcode[%d] = %d\n", src_index, opcode);
      switch( opcode ){
      case SWITCH: {
        uint32_t sizes = src_code[src_index+1];
        uint32_t const_size = sizes & 0xFFFF;
        uint32_t block_size = sizes >> 16;
        nargs = 1 + const_size + block_size;
        break;
      }
      case CLOSUREREC: {
        uint32_t nfuncs = src_code[src_index+1];
        nargs = 1 + 1 + nfuncs;
        break;
      }
      default: {
        nargs = opcode_nargs[opcode];
      }
      }
      nlocids += caml_ocp_bytecode_has_locid(opcode);
      src_index += 1 + nargs;
    }
    final_dst_index = final_src_index + nlocids;
    nlocids = 0;
    ntables = 0;
  } else {
    nlocids = getint();
    ntables = getint();
    XXX fprintf(stderr, "nlocids=%d ntables=%d\n", nlocids, ntables);
    final_dst_index = final_src_index + nlocids;
  }
  final_size = final_dst_index * sizeof(opcode_t);
  dst_code = (code_t) caml_stat_alloc( final_size );
  translation_code = (code_t) caml_stat_alloc( final_size );
  
  XXX fprintf(stderr, "caml_ocp_bytecode_fix_locids\n");
  XXX fprintf(stderr, "   src: %lX %ld\n", (value)src_code, *src_size);
  
  /* Clear the table only if we are going to use it ! */
  if(caml_ocp_bytecode_translation_table == NULL){
    memset (translation_code, 0, final_size);
  }
  
  XXX fprintf(stderr, "ntables = %d, %d patches\n", ntables, nlocids);
  nlocids = 0;
  for(src_index = 0; src_index < final_src_index; ){
    XXX fprintf(stderr, "src_index = %d / dst_index = %d\n", src_index, dst_index);
    int has_locid = 0;
    while(next_index < 0 ){
      if( nlocids == 0)
	if(ntables == 0) {
	  XXX fprintf(stderr, "No more table final_src_index = %d  !\n", final_src_index);
	  nlocids = -1;
	  next_index = final_src_index;
	}
	else {
	  int table_string_size;
	  int table_size;
	  char* table;
	  
	  cmo_offset = getint();
	  XXX fprintf(stderr, "cmo_offset = %d\n", cmo_offset);
	  table_string_size = getint();
	  table = memprof_info;
          memprof_info += table_string_size + 1;
	  table_size = getint();

	  XXX fprintf(stderr, "table_size = %d/%d\n", 
		  table_string_size, table_size);
	  base_id = caml_memprof_register_loc_table(table, 
						    table_string_size,
						    table_size);
	  XXX fprintf(stderr, "base_id = %d\n", base_id);
	  nlocids = getint();
	  XXX fprintf(stderr, "nlocids = %d\n", nlocids);
	  ntables--;
	} 
      else {
	next_index = cmo_offset + getint();
	next_offset = getint();
	XXX fprintf(stderr, "locid %d @ %d\n", next_offset, next_index);
	nlocids--;
      }
    }
    opcode = src_code[src_index];
    XXX fprintf(stderr, "opcode[%d] = %d\n", src_index, opcode);
    switch( opcode ){
    case SWITCH: {
      uint32_t sizes = src_code[src_index+1];
      uint32_t const_size = sizes & 0xFFFF;
      uint32_t block_size = sizes >> 16;
      nargs = 1 + const_size + block_size;
      break;
    }
    case CLOSUREREC: {
      uint32_t nfuncs = src_code[src_index+1];
      nargs = 1 + 1 + nfuncs;
      break;
    }
    default: {
      nargs = opcode_nargs[opcode];
    }
    }
    // XXX fprintf(stderr, "  nargs = %d\n", nargs);
    src_code[src_index] = dst_index;

    if( caml_ocp_bytecode_has_locid(opcode) ){
      if(src_index == next_index ){
        XXX fprintf(stderr, "patch @%d\n", next_index);
        next_index = -1;
        has_locid = 1;
        if ( next_offset == 0 )
          dst_code [ dst_index + 1 ] = 0;
        else
          dst_code [ dst_index + 1 ] = base_id + next_offset - 1;
      } else
        if(next_index == final_src_index){
          has_locid = 1;
          dst_code [ dst_index + 1 ] = 0;
        } else
          {
            fprintf(stderr, "***************\n");
            fprintf(stderr, "***************\n");
            fprintf(stderr, "***************   FORGOTTEN OPCODE\n");
            fprintf(stderr, "***************   opcode: %d\n", opcode);
            fprintf(stderr, "***************   src_index: %d\n", src_index);
            fprintf(stderr, "***************\n");
            fprintf(stderr, "***************\n");
            exit(2);
          }
    }

    if( src_index > next_index && next_index >= 0){
      XXX fprintf(stderr, "src_index (%d) > next_index (%d)\n", src_index, next_index);
      exit(2);
    }

    dst_code[dst_index] = opcode;
    dst_index += has_locid; /* skip locid */
    for(i=1; i <= nargs; i ++)  dst_code[dst_index+i] = src_code[src_index+i];
    dst_index += 1 + nargs;
    src_index += 1 + nargs;
  }

  
  
  for(src_index = 0; src_index < final_src_index; ){
    XXX fprintf(stderr, "src_index = %d\n", src_index);
    int dst_index = src_code[src_index];
    XXX fprintf(stderr, "dst_index = %d\n", dst_index);
    int opcode = dst_code[dst_index];
    XXX fprintf(stderr, "opcode = %d\n", opcode);
    int branch_pos = 0;
    int n_addrs = 0;

    translation_code[dst_index] = dst_index - src_index;

    switch(opcode){
    case SWITCH: {
      uint32_t sizes = src_code[src_index+1];
      uint32_t const_size = sizes & 0xFFFF;
      uint32_t block_size = sizes >> 16;
      nargs = 1 + const_size + block_size;
      break;
    }
    case CLOSUREREC: {
      uint32_t nfuncs = src_code[src_index+1];
      nargs = 1 + 1 + nfuncs;
      break;
    }
    default:
      nargs = opcode_nargs[opcode];
    }
  
    switch(opcode){
      /* branches */
    case BRANCH: 
    case BRANCHIF:
    case BRANCHIFNOT:
    case PUSH_RETADDR:
    case PUSHTRAP:
      branch_pos = 1;
      n_addrs = 1;
      break;

    case CLOSURE:
      dst_index++; /* fallthrough */
    case BEQ:
    case BNEQ:
    case BLTINT:
    case BLEINT:
    case BGTINT:
    case BGEINT:
    case BULTINT:
    case BUGEINT: 
      branch_pos = 2;
      n_addrs = 1;
      break;

    case CLOSUREREC: {
      dst_index++;
      branch_pos = 3;
      n_addrs = src_code[ src_index + 1 ];
      break;
    }
    case SWITCH: {
      uint32_t sizes = src_code[ src_index+1 ];
      uint32_t const_size = sizes & 0xFFFF;
      uint32_t block_size = sizes >> 16;
      n_addrs = const_size + block_size;
      branch_pos = 2;
      break;
    }
      
    }

    if( branch_pos > 0 ){
      for(i = 0; i < n_addrs; i++){
	int src_go_to = (src_index+branch_pos) + src_code[ src_index+branch_pos+i ];
	int dst_go_to = src_code [ src_go_to ];
	int dst_offset = (dst_go_to - (dst_index+branch_pos) );
	dst_code [ dst_index+branch_pos+i ] = dst_offset;	
      }
    }
    
    src_index += 1 + nargs;
  }

  *src_size = final_size;

  /* Update code_fragment table */
  { 
    char *code_start = (char*)dst_code;
    struct code_fragment * cf = 
      caml_code_fragments_table.contents[
					 caml_code_fragments_table.size - 1
					 ];
    if( cf->code_start != (char*)src_code ) {
      exit(66);
    } 
    cf->code_start = code_start;
    cf->code_end = code_start + final_size;
  }
  
  if( caml_ocp_bytecode_translation_table == NULL ){
    caml_ocp_bytecode_translation_table = translation_code;
  } else {
    caml_stat_free(translation_code);
  }

  memprof_info = NULL;

  XXX fprintf(stderr, "   dst: %lX %ld\n", (value)dst_code, *src_size);

  return dst_code;
}

#endif // OCP_BYTECODE_DISABLED

#endif // NATIVE_CODE
 
