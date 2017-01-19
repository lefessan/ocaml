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

#define MEMPROF_INSIDE
#define CAML_INTERNALS

#include <string.h>

/* ocpwin: for SIGHUP */
#ifdef _WIN32
#define _POSIX
#endif
#include <signal.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#ifndef _WIN32
#include <libgen.h>
#endif

#include "caml/alloc.h"
#include "caml/callback.h"
#include "caml/fail.h"
#include "caml/finalise.h"
#include "caml/gc_ctrl.h"
#include "caml/globroots.h"
#include "caml/intext.h"
#include "caml/memory.h"
#include "caml/ocp_utils.h"
#include "caml/ocp_memprof.h"
#include "caml/minor_gc.h"
#include "caml/mlvalues.h"
#include "caml/roots.h"
#include "caml/signals.h"
#include "caml/sys.h"
#include "caml/exec.h"
#include "caml/unixsupport.h"
#include "caml/md5.h"
#include "caml/caml_io.h"
#include "caml/weak.h"
#define OCP_NEED_LOCINFO
#include "caml/backtrace.h"
#include "c_heapprof.h"

#ifndef NATIVE_CODE
#include "caml/fix_code.h"
#include "caml/stacks.h"
#define caml_code_area_start ((char *) caml_start_code)
#define caml_code_area_end ((char *) caml_start_code + caml_code_size)
#include "caml/startup.h"
#else
#include "caml/stack.h"
extern value *caml_globals[];
extern char caml_globals_map[];
#endif

#define Heapdump_process_magic_number "OCP-2017P001"
#define Heapdump_process_magic_number_len sizeof(Heapdump_process_magic_number)


// value caml_sys_get_argv(value unit); // From sys.c



static int first_dump = 1;
/* [pi_file] is static because it is needed by a function called
 by [caml_do_roots], which does not let us pass an argument
 to the function. */

/* Process info */

static void store_size(intnat v, FILE * file)
{
  int i;
  for (i = 0; i < 32 ; i += 8) putc(v >> i, file);
}

#include "commit.h"
void save_commit_section(FILE* pi_file)
{
  int commitlen = strlen(OCP_COMMIT);
  int datelen = strlen(OCP_DATE);
  int size = 1 + commitlen + 1 + datelen;

  ocp_heapdump_pi_section(pi_file, "CMIT", size);
  putc(commitlen, pi_file);
  fwrite(OCP_COMMIT, commitlen, 1, pi_file);
  putc(datelen, pi_file);
  fwrite(OCP_DATE, datelen, 1, pi_file);
}

void save_compiler_section(FILE* pi_file)
{
  
  if(ocp_memprof_info.memprof_version >= 0){
    char *version = ocp_memprof_info.ocaml_version;
    char *commit = ocp_memprof_info.ocaml_commit;
    char *date = ocp_memprof_info.ocaml_date;

    int versionlen = strlen(version);
    int commitlen = strlen(commit);
    int datelen = strlen(date);
    int size = 1 + commitlen + 1 + datelen + 1 + versionlen;
    
    ocp_heapdump_pi_section(pi_file, "COMP", size);
    putc(versionlen, pi_file);
    fwrite(version, versionlen, 1, pi_file);
    putc(commitlen, pi_file);
    fwrite(commit, commitlen, 1, pi_file);
    putc(datelen, pi_file);
    fwrite(date, datelen, 1, pi_file);
  }
}

static void dump_process_info(FILE* pi_file) {

  CAMLparam0();
  CAMLlocal3(data, exe_name, runparam);
  char *buf;
  intnat size;
#ifndef NATIVE_CODE
  struct exec_trailer trail;
  int fd;
#endif

  first_dump = 0;

  exe_name = caml_sys_get_argv(Val_unit);
  buf = getenv ("OCAMLRUNPARAM");
  if(!buf) buf = getenv ("CAMLRUNPARAM");
  if(!buf) {
    runparam = Val_long(0);
  } else {
    runparam = caml_alloc_small(1, 0);
    Field(runparam, 0) = caml_copy_string(buf);
  }

  // To be synchronized with 'Heapdump_reader.raw_process_info'
  data = caml_alloc_tuple(5);
  Field(data, 0) = Val_long(HEAPDUMP_GETPID());
  Field(data, 1) = exe_name;
  Field(data, 2) = runparam;
  Field(data, 3) = Val_long (8 * sizeof(value));
#ifdef NATIVE_CODE
  Field(data, 4) = Val_long(1);
#else
  Field(data, 4) = Val_long(0);
#endif

  caml_output_value_to_malloc(data, Val_int(0), &buf, &size);

  /* Write process_info: | MAGIC | INFO | GMAP | LOCS | */
  fwrite(Heapdump_process_magic_number,
	 Heapdump_process_magic_number_len - 1, 1, pi_file);
  ocp_heapdump_pi_section(pi_file, "INFO", size);
  fwrite(buf, size, 1, pi_file);
  caml_stat_free(buf);

  /* The globals map */
#ifdef NATIVE_CODE
  size = caml_string_length((value) caml_globals_map);
  ocp_heapdump_pi_section(pi_file, "GMAP", size);
  fwrite(String_val((value) caml_globals_map), size, 1, pi_file);
#else
  fd = caml_attempt_open(&caml_exe_name, &trail, 1);
  if (fd > 0) {
    caml_read_section_descriptors(fd, &trail);
    size = caml_seek_optional_section(fd, &trail, "SYMB");
    if (size != -1) {
      buf = caml_stat_alloc(size);
      if (read(fd, buf, size) == size) {
        ocp_heapdump_pi_section(pi_file, "GMAP", size);
        fwrite(buf, size, 1, pi_file);
      }
      caml_stat_free(buf);
    }
  }
#endif
  save_commit_section(pi_file);
  save_compiler_section(pi_file);

  CAMLreturn0;

}

CAMLexport void ocp_heapdump_pi_section(FILE* pi_file, char *name, mlsize_t size)
{
  fwrite(name, 4, 1, pi_file);
  store_size(size, pi_file);
 
}

static char* process_info_filename = NULL;
static FILE *pi_file = NULL;

CAMLexport void ocp_heapdump_pi_setname(char *name)
{
  process_info_filename = name;
}


#ifdef NATIVE_CODE

static int dynglobal_counter_done = 0;
static int dynglobal_size_done = 0;

static int dynglobal_counter = 0;
static int dynglobal_size = 0;

static void pi_prepare_dynglobal_name(char *name, value v)
{
  int name_size = strlen(name);
  dynglobal_counter++;
  dynglobal_size += name_size + 1;
}

static void pi_store_dynglobal_name(char *name, value v)
{
  if( dynglobal_counter > dynglobal_counter_done ) {
    dynglobal_counter_done++;
    int name_size = strlen(name);
    fwrite(name, name_size+1, 1, pi_file);
  }
}

#endif

CAMLexport void ocp_heapdump_pi_do_dump(char *filename, value md5)
{
  CAMLparam1(md5);
  int filename_size = strlen(filename);
  int md5_size = caml_string_length(md5);
  /* Create/Update the process_info file */
  
  pi_file = fopen(process_info_filename, "a");
  if (pi_file == 0) {
    perror("fopen");
    fprintf(stderr, "Error while opening \"%s\"\n", process_info_filename);
    /* Fabrice: never abort. What should we do here ? */
    exit(EXIT_FAILURE);
  }

  if (first_dump) dump_process_info(pi_file);

#ifdef NATIVE_CODE
  dynglobal_counter = 0;
  dynglobal_size = 0;
  caml_do_dynglobals(pi_prepare_dynglobal_name);
  if( dynglobal_counter > dynglobal_counter_done ){
    int size = dynglobal_size - dynglobal_size_done + 8;
    /* begin section DYNM */
    ocp_heapdump_pi_section(pi_file, "DYNM", size); 
    /* position */
    store_size(dynglobal_counter_done, pi_file);
    /* number of items (0-terminated) */
    store_size(dynglobal_counter - dynglobal_counter_done, pi_file);
     /* items (0-terminated) */
    caml_do_dynglobals(pi_store_dynglobal_name);
    dynglobal_size_done = dynglobal_size;
  }
#endif

  if (ocp_heapdump_hook && ocp_heapdump_hook_process){
    ocp_heapdump_hook(pi_file);
  }
  ocp_heapdump_pi_section(pi_file, "DUMP", filename_size);
  fwrite(filename, filename_size, 1, pi_file);

  ocp_heapdump_pi_section(pi_file, "HASH", md5_size );
  fwrite(String_val(md5), md5_size, 1, pi_file);
  fclose(pi_file);
  pi_file = NULL;
  CAMLreturn0;
}
