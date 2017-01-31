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

/* Handling of blocks of bytecode (endianness switch, threading). */

#define MEMPROF_INSIDE

#include "caml/internals/config.h"

#ifdef HAS_UNISTD
#include <unistd.h>
#endif

#include "caml/internals/debugger.h"
#include "caml/internals/fix_code.h"
#include "caml/internals/instruct.h"
#include "caml/internals/intext.h"
#include "caml/internals/md5.h"
#include "caml/internals/memory.h"
#include "caml/internals/misc.h"
#include "caml/internals/mlvalues.h"
#include "caml/internals/reverse.h"
#include "caml/internals/memprof.h"

int caml_memprof_bytecode = 0;

#ifndef ARCH_SIXTYFOUR
// #ifndef ARCH_SIXTYFOUR

code_t ocp_memprof_bytecode_fix_locids(code_t data, asize_t* code_size)
{ return data; }

#else
// #ifdef ARCH_SIXTYFOUR
#ifndef NATIVE_CODE
// #ifdef ARCH_SIXTYFOUR
// #ifndef NATIVE_CODE

#include <string.h>
#include <stdlib.h>
#include <stdio.h>

char* memprof_info = NULL;
void ocp_memprof_bytecode_init(char *memprof_section){
  memprof_info = memprof_section;
}

#ifndef ARCH_BIG_ENDIAN
// #ifdef ARCH_SIXTYFOUR
// #ifndef NATIVE_CODE
// #ifndef ARCH_BIG_ENDIAN

#include "caml/internals/reverse.h"
static void fixup_endianness_int(uint32 * p)
{
  Reverse_32(p, p);
}
#else
// #ifdef ARCH_SIXTYFOUR
// #ifndef NATIVE_CODE
// #ifdef ARCH_BIG_ENDIAN

#define fixup_endianness_int(p) (p)
#endif

// #ifdef ARCH_SIXTYFOUR
// #ifndef NATIVE_CODE


static int getint()
{
  uint32 p = *(uint32*) memprof_info;
  fixup_endianness_int(&p);
  memprof_info = memprof_info + sizeof(uint32);
  return p;
}

static int debug_print = 0;


int ocp_memprof_register_loc_table (char* table, mlsize_t len, int elems);

#define XXX if(debug_print)

#include "caml/internals/instruct.h"


/* This function modifies the bytecode instructions to insert the locids
   and the corresponding new instructions. After that, it updates the
   code fragments. Beware, instruction pointers saved with profiling
   are not compatible with instruction pointers saved without profiling.

   It would probably be possible to create a "translation" table.
*/

code_t ocp_memprof_bytecode_fix_locids(code_t src_code, asize_t* src_size)
{ 
  if( getenv("MEMP_DEBUG") != NULL ) debug_print = 1;
  if( getenv("MEMP_DISABLED") != NULL ) memprof_info = NULL;

  if(memprof_info == NULL){
    return src_code;
  } else {
  int nlocids = getint();
  int ntables = getint();
  int final_src_index = (*src_size) / sizeof(opcode_t);
  int final_dst_index = final_src_index + nlocids;
  int final_size = final_dst_index * sizeof(opcode_t);
  code_t dst_code = (code_t) caml_stat_alloc( final_size );
  code_t translation_code = (code_t) caml_stat_alloc( final_size );
  int dst_index = 0;
  int src_index = -1;
  int next_index = -1;
  int next_offset = -1;
  int base_id = -1;
  int opcode = -1;
  int cmo_offset = -1;
  int i, nargs;
  opcode_t* opcode_nargs = caml_init_opcode_nargs();

  /* Clear the table only if we are going to use it ! */
  if(caml_memprof_translation_table == NULL){
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
	  table = memprof_info;	  memprof_info += table_string_size + 1;
	  table_size = getint();

	  XXX fprintf(stderr, "table_size = %d/%d\n", 
		  table_string_size, table_size);
	  base_id = ocp_memprof_register_loc_table(table, 
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
      uint32 sizes = src_code[src_index+1];
      uint32 const_size = sizes & 0xFFFF;
      uint32 block_size = sizes >> 16;
      nargs = 1 + const_size + block_size;
      break;
    }
    case CLOSUREREC: {
      uint32 nfuncs = src_code[src_index+1];
      nargs = 1 + 1 + nfuncs;
      break;
    }
    default: {
      nargs = opcode_nargs[opcode];
    }
    }
    // XXX fprintf(stderr, "  nargs = %d\n", nargs);
    src_code[src_index] = dst_index;

    if(src_index == next_index ){
      XXX fprintf(stderr, "patch @%d\n", next_index);
      next_index = -1;
      has_locid = 1;
      if ( next_offset == 0 )
	dst_code [ dst_index + 1 ] = 0;
      else
	dst_code [ dst_index + 1 ] = base_id + next_offset - 1;
      
      switch(opcode){
	/* allocations */
      case GRAB: { opcode = GRAB_WITH_LOCID; break; }
      case CLOSURE: { opcode = CLOSURE_WITH_LOCID; break; } 
      case CLOSUREREC: { opcode = CLOSUREREC_WITH_LOCID; break; }
      case MAKEBLOCK: { opcode = MAKEBLOCK_WITH_LOCID; break; }
      case MAKEBLOCK1: { opcode = MAKEBLOCK1_WITH_LOCID; break; }
      case MAKEBLOCK2: { opcode = MAKEBLOCK2_WITH_LOCID; break; }
      case MAKEBLOCK3: { opcode = MAKEBLOCK3_WITH_LOCID; break; }
      case MAKEFLOATBLOCK: { opcode = MAKEFLOATBLOCK_WITH_LOCID; break; }
      case GETFLOATFIELD: { opcode = GETFLOATFIELD_WITH_LOCID; break; }
      case C_CALL1: { opcode = C_CALL1_WITH_LOCID; break; }
      case C_CALL2: { opcode = C_CALL2_WITH_LOCID; break; }
      case C_CALL3: { opcode = C_CALL3_WITH_LOCID; break; }
      case C_CALL4: { opcode = C_CALL4_WITH_LOCID; break; }
      case C_CALL5: { opcode = C_CALL5_WITH_LOCID; break; }
      case C_CALLN: { opcode = C_CALLN_WITH_LOCID; break; }
      default: {
	fprintf(stderr, "******************************\n");
	fprintf(stderr, "******************************\n");
	fprintf(stderr, "******************************   BAD OPCODE\n");
	fprintf(stderr, "******************************   opcode: %d\n", opcode);
	fprintf(stderr, "******************************   src_index: %d\n", src_index);
	fprintf(stderr, "******************************\n");
	fprintf(stderr, "******************************\n");
	if( debug_print == 0 ) return src_code;
	exit(2);
      }
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
      uint32 sizes = src_code[src_index+1];
      uint32 const_size = sizes & 0xFFFF;
      uint32 block_size = sizes >> 16;
      nargs = 1 + const_size + block_size;
      break;
    }
    case CLOSUREREC_WITH_LOCID:
    case CLOSUREREC: {
      uint32 nfuncs = src_code[src_index+1];
      nargs = 1 + 1 + nfuncs;
      break;
    }
    case GRAB_WITH_LOCID :
    case CLOSURE_WITH_LOCID :
    case MAKEBLOCK_WITH_LOCID :
    case MAKEBLOCK1_WITH_LOCID :
    case MAKEBLOCK2_WITH_LOCID :
    case MAKEBLOCK3_WITH_LOCID :
    case MAKEFLOATBLOCK_WITH_LOCID :
    case GETFLOATFIELD_WITH_LOCID :
    case C_CALL1_WITH_LOCID :
    case C_CALL2_WITH_LOCID :
    case C_CALL3_WITH_LOCID :
    case C_CALL4_WITH_LOCID :
    case C_CALL5_WITH_LOCID :
    case C_CALLN_WITH_LOCID : 
      nargs = opcode_nargs[opcode] - 1; /* remove LOCID */
      break;
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

    case CLOSURE_WITH_LOCID:
      dst_index++; /* fallthrough */
    case CLOSURE:
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

    case CLOSUREREC_WITH_LOCID:
      dst_index++; /* fallthrough */
    case CLOSUREREC: {
      branch_pos = 3;
      n_addrs = src_code[ src_index + 1 ];
      break;
    }
    case SWITCH: {
      uint32 sizes = src_code[ src_index+1 ];
      uint32 const_size = sizes & 0xFFFF;
      uint32 block_size = sizes >> 16;
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

  if( caml_memprof_translation_table == NULL ){
    caml_memprof_translation_table = translation_code;
  } else {
    caml_stat_free(translation_code);
  }

  return dst_code;
  }
}

#endif // NATIVE_CODE
 
#endif // ARCH_SIXTYFOUR
