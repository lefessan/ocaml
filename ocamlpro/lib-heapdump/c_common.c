/**************************************************************************/
/*                                                                        */
/*   Copyright 2014-2017 OCamlPro SAS --- typerex-memprof                 */
/*                                                                        */
/*   Do not redistribute, without permission of OCamlPro, source          */
/*   or binary copies of this software.                                   */
/*                                                                        */
/**************************************************************************/


#define MEMPROF_INSIDE
#define CAML_INTERNALS


#include "caml/mlvalues.h"

#include "caml/roots.h"

/* ocpwin: for getpid */
#ifdef HAS_UNISTD
#include <unistd.h>
#endif

#include <string.h>
#include "caml/alloc.h"
#include "caml/intext.h"
#include "caml/memory.h"
#include "caml/sys.h"
/* #include "caml/sys.h" */
/* #include "caml/fix_code.h" */
#include "caml/signals.h"
#include "c_heapprof.h"

#ifdef _WIN32
#include <wtypes.h>
#include <winbase.h>
/* for getpid: */
#include <process.h>
#define _POSIX
#endif
#include <signal.h>

#include "caml/ocp_utils.h"
#include "caml/ocp_memprof.h"
#include "caml/ocp_gcprof.h"

#define MEMPROF_LOCID_LOCATION_TABLE 2

#include <stdio.h>
#include <errno.h>

/* Globals */
int caml_memprof_next_location_id = 0;
char **caml_location_tables = NULL;
mlsize_t *caml_location_tables_sizes = NULL;
int caml_location_tables_next = 0;

/* Locals */
/* This table indicates, for a given instruction in the main code segment,
   the offset with the original instruction (used for backtraces...). */
code_t caml_memprof_translation_table = NULL;

#ifndef ARCH_SIXTYFOUR


value ocp_memprof_register_locid(value name_v)
{ return Val_int(0); }

value ocp_memprof_set_locid (value arg, value new_locid)
{ return Val_int(-1); }

value ocp_memprof_get_locid(value arg)
{
  return Val_int(-1);
}

#else

static int location_tables_last_dump = 0;
static int location_tables_max = 0;
static void (*heapdump_hook_saved)(FILE *);

void memprof_dump_location_tables(FILE* pi_file)
{
  int i;
  static int inited = 0;
  if (heapdump_hook_saved) heapdump_hook_saved(pi_file);

  if( !inited ){
    int64_t seed = ocp_memprof_seed();
    ocp_heapdump_pi_section(pi_file, "SEED", 8);
    fwrite(&seed, 8, 1, pi_file);
    inited = 1;
  }

  for(i = location_tables_last_dump; i < caml_location_tables_next; i++) {
    ocp_heapdump_pi_section(pi_file, "LOCS", caml_location_tables_sizes[i]);
    fwrite(caml_location_tables[i], 1, caml_location_tables_sizes[i], pi_file);
  }
  location_tables_last_dump = i;
}

int ocp_memprof_register_loc_table (char* table, mlsize_t len, int elems)
{
  int offset;

  if(caml_location_tables_next >= location_tables_max) {
    location_tables_max += 1;
    location_tables_max *= 2;
    caml_location_tables =
      caml_stat_resize(caml_location_tables,
                       location_tables_max * sizeof(value));
    caml_location_tables_sizes =
      caml_stat_resize(caml_location_tables_sizes,
                       location_tables_max * sizeof(mlsize_t));
  }

  // FIXME: test next_location_id < (2^22)...
  offset = caml_memprof_next_location_id;
  caml_memprof_next_location_id += elems;

  caml_location_tables[caml_location_tables_next] = table;
  caml_location_tables_sizes[caml_location_tables_next] = len;
  caml_location_tables_next++;
  ocp_heapdump_hook_process = 1;

  return offset;
}

/* For each name, we create a faked location table. Normally, these
location tables are marshaled, so unmarshaling is going to fail, and we
will discover that it is not a real location table ! */
CAMLprim value ocp_memprof_register_locid(value name_v)
{
  char *name = String_val(name_v);
  int len = strlen(name)+5;
  char *new_name = caml_stat_alloc(len);
  int locid;

  /* Add a short magic in front of the string to recognize it is not
     an error in the unmarshaller format. */
  new_name[0] = 0xFE;
  new_name[1] = 0x81;
  new_name[2] = 0x82;
  new_name[3] = 0xCF;
  strcpy(new_name+4, name);
  locid = ocp_memprof_register_loc_table (new_name, len, 1);

  return Val_int(locid);
}

CAMLprim value ocp_memprof_set_locid (value arg, value new_locid)
{
  value hd = Hd_val(arg);
  profinfo_t old_locid = Profinfo_hd(hd);
  Hd_val(arg) = Hd_no_profinfo(hd)
    | Make_header_with_profinfo(0,0,0,new_locid);
  return Val_int(old_locid);
}

CAMLprim value ocp_memprof_get_locid(value arg)
{
  if (!Is_in_value_area (arg)) return Val_int(-1);
  return Val_int(Profinfo_val(arg));
}

CAMLprim value ocp_memprof_register_table (value table, value elems, value ui_name)
{
  int offset;

  offset = 
    ocp_memprof_register_loc_table(String_val(table),
                                    caml_string_length(table),
                                    Int_val(elems));
  return (Val_int(offset));
}

uintnat caml_memprof_locid_location_table = MEMPROF_LOCID_LOCATION_TABLE << 10;

static const memprof_id_desc caml_memprof_allids[PROF_LAST_EXTERNAL+1] = {
  { "_memprof_", -1, PROF_MEMPROF },
  { "_startup_", 374, PROF_STARTUP },
  { "_startup_input_", -1, PROF_STARTUP_INPUT_VAL },
  { "_interp_", -1, PROF_INTERP },
  { "_init_", -1, PROF_INIT },
  { "_exception_", -1, PROF_EXCEPTION },
  
  { NULL, 0, PROF_LAST_EXTERNAL },
  { NULL, 0, PROF_LAST_EXTERNAL },
  { NULL, 0, PROF_LAST_EXTERNAL }
};

static void register_externals(void) {

  CAMLparam0();
  CAMLlocal5(dummy_id, dummy_mty, dummy_subst, position, location);
  CAMLlocal5(external_name, info, info_array, loc, table);
  CAMLlocal1(cmg_info);

  int i;
  intnat len;
  char *buf;

  external_name = caml_copy_string("__externals");

  dummy_id = caml_alloc_tuple(3); // Ident.t
  Store_field(dummy_id, 0, Val_int(0));
  Store_field(dummy_id, 1, caml_copy_string(":dummy:"));
  Store_field(dummy_id, 2, Val_int(0));

  dummy_mty = caml_alloc_small(1, 1); // Types.Mty_signature
  Store_field(dummy_mty, 0, Val_int(0)); // []

  dummy_subst = caml_alloc_small(4, 0); // Subst.t
  Store_field(dummy_subst, 0, Val_int(0)); // Tbl.Empty
  Store_field(dummy_subst, 1, Val_int(0)); // Tbl.Empty
  Store_field(dummy_subst, 2, Val_int(0)); // Tbl.Empty
  Store_field(dummy_subst, 3, Val_int(0)); // false

  info_array =
    caml_alloc_small(PROF_LAST_EXTERNAL, 0); // Types.block_info array

  position = caml_alloc_small(4, 0); // Lexing.position
  Store_field(position, 0, caml_copy_string("_none_"));
  Store_field(position, 1, Val_int(1));
  Store_field(position, 2, Val_int(0));
  Store_field(position, 3, Val_int(-1));
  location = caml_alloc_small(3, 0); // Location.t
  Store_field(location, 0, position);
  Store_field(location, 1, position);
  Store_field(location, 2, Val_int(1)); // true
  loc = caml_alloc_small(2, 0); // Types.block_location
  Store_field(loc, 0, location);
  Store_field(loc, 1, Val_long(0));
  info = caml_alloc_small(2, 0); // Types.block_info
  Store_field(info, 0, loc);
  Store_field(info, 1, Val_long(0));  // Types.Dummy
  Store_field(info_array, 0, info);
  Store_field(info_array, 1, info);

  for(i = 0; i < PROF_LAST_EXTERNAL-2; i++) {
    position = caml_alloc_small(4, 0); // Lexing.position
    Store_field(position, 0,
                caml_copy_string(caml_memprof_allids[i].filename));
    Store_field(position, 1, Val_int(caml_memprof_allids[i].line));
    Store_field(position, 2, Val_int(0));
    Store_field(position, 3, Val_int(0));
    location = caml_alloc_small(3, 0); // Location.t
    Store_field(location, 0, position);
    Store_field(location, 1, position);
    Store_field(location, 2, Val_int(0)); // false
    loc = caml_alloc_small(2, 0); // Types.block_location
    Store_field(loc, 0, location);
    Store_field(loc, 1, Val_long(0));
    info = caml_alloc_small(2, 0); // Types.block_info
    Store_field(info, 0, loc);
    Store_field(info, 1, Val_long(1));  // Types.External
    Store_field(info_array, caml_memprof_allids[i].locid, info);
  };

  cmg_info = caml_alloc_tuple(7); // Memprof.cmg_infos
  Store_field(cmg_info, 0, external_name);
  Store_field(cmg_info, 1, dummy_id);
  Store_field(cmg_info, 2, dummy_mty);
  Store_field(cmg_info, 3, dummy_subst);
  Store_field(cmg_info, 4, info_array);
  Store_field(cmg_info, 5, Val_unit);
  Store_field(cmg_info, 6, Atom(0)); 

  info = caml_alloc_small(1, 1); // Memprof.Direct
  Store_field(info, 0, cmg_info);

  caml_output_value_to_malloc(info, Val_int(0), &buf, &len);
  ocp_memprof_register_loc_table(buf, len, PROF_LAST_EXTERNAL);

  CAMLreturn0;
}

static void memprof_init(void)
{
  ocp_heapdump_init();

  /* Initialise the location table */
  location_tables_max = 128;
  caml_location_tables = caml_stat_alloc(location_tables_max * sizeof(value));
  caml_location_tables_sizes =
    caml_stat_alloc(location_tables_max * sizeof(mlsize_t));

  register_externals();

  heapdump_hook_saved = ocp_heapdump_hook;
  ocp_heapdump_hook = memprof_dump_location_tables;

}

#endif // ARCH_SIXTYFOUR

/*
CAMLprim value ocp_memprof_watermark(value s_v)
{ 
  GCPROF_WATERMARK(String_val(s_v));
  return Val_unit; 
}
*/

/********************************************************************/
/*                                                                  */
/*  Common init/exit functions for memprof/gcprof/allocprof         */
/*                                                                  */
/********************************************************************/

CAMLexport void ocp_memprof_init()
{
#ifdef ARCH_SIXTYFOUR
  memprof_init();
#endif
/* Initialize gcprof before allocprof, because gcprof can trigger allocprof */
#ifndef DISABLE_GCPROF
  ocp_gcprof_init();
#endif
#ifndef DISABLE_ALLOCPROF
  ocp_allocprof_init(0);
#endif
}

CAMLexport struct memprof_init ocp_memprof_info = {
  -1, NULL, NULL, NULL
};

CAMLexport void ocp_memprof_init2(struct memprof_init* m)
{
  ocp_memprof_info.memprof_version = m->memprof_version;
  if(m->ocaml_version != NULL)
    ocp_memprof_info.ocaml_version = strdup(m->ocaml_version);
  if(m->ocaml_commit != NULL)
    ocp_memprof_info.ocaml_commit = strdup(m->ocaml_commit);
  if(m->ocaml_date != NULL)
    ocp_memprof_info.ocaml_date = strdup(m->ocaml_date);

  ocp_memprof_init();
}


CAMLexport void ocp_memprof_exit()
{
#ifndef DISABLE_ALLOCPROF
  ocp_allocprof_exit();
#endif
#ifndef DISABLE_GCPROF
  ocp_gcprof_exit();
#endif
}

void ocp_memprof_gdb()
{

}


/********************************************************************/
/*                                                                  */
/*                           Compaction                             */
/*                                                                  */
/********************************************************************/

/* Format of "memprof.%PID.%GC.%COMPACTION.compact":
   - string[12]: magic number
   - uintz : %PID
   - uintz : %GC
   - uintz : %COMPACTION
   - items:
   ---- uintz : wosize[item] (always <> 0)
   ---- int64 : old pointer
   ---- int64 : new pointer
   - uintz : 0
 */

#define Compact_magic_number "OCP-2014C001"
#define Compact_magic_number_len sizeof(Compact_magic_number)

static int save_compact_moves = 0;
static int compaction_count = 0;
static FILE *compact_oc = NULL;

static void fput_word(value v, FILE*oc)
{
  int i;
  for (i = 0; i < sizeof(value) * 8 ; i += 8) putc(v >> i, oc);
}

static void fput_uint(uint64_t c, FILE*oc)
{
  uint64_t m = c >> 7;
  while( m != 0 ){
    putc( c & 0x7f, oc);
    c = m;
    m = c >> 7;
  }
  putc( c|0x80 , oc);
}

CAMLexport void ocp_memprof_compact_begin()
{
  static int inited = 0;

  if(  !inited ){
    inited = 1;
    if( (ocp_heapdump_after_gc == 1) &&
        (getenv("OCP_MEMPROF_AGE") != NULL) ) save_compact_moves = 1;
  }
  if( save_compact_moves ){
    int heap_number;
    int pid;
    char *destdir = ocp_heapdump_get_destdir(&pid, &heap_number);  
    char *filename = caml_stat_alloc( strlen(destdir) + 50 );
    compaction_count++;
    sprintf(filename, "%s/memprof.%d.%d.%d.compact",
            destdir, pid, heap_number, compaction_count);
    compact_oc = fopen(filename, "w+b");
    caml_stat_free( filename );
    fwrite(Compact_magic_number, Compact_magic_number_len - 1, 1, compact_oc);
    fput_uint(pid, compact_oc);
    fput_uint(heap_number, compact_oc);
    fput_uint(compaction_count, compact_oc);
  }
}

CAMLexport void ocp_memprof_compact_move(void* p, void* newaddr, size_t sz)
{
  if( save_compact_moves ){
    fput_uint(sz, compact_oc);
    fput_word( (value)p, compact_oc);
    fput_word( (value)newaddr, compact_oc);
  }
}
CAMLexport void ocp_memprof_compact_end()
{
  if( save_compact_moves ){
    fput_uint(0, compact_oc);
    fclose(compact_oc);
  }
}

/* Use gettimeofday to extract a usec-counter [seed], that is going to
be saved in all our logs. Sharing the same pid and the same seed
should ensure that these logs are indeed associated with the same
file. */

#include <sys/types.h>
#include <sys/time.h>

CAMLexport int64_t ocp_memprof_seed()
{
  static int inited = 0;
  static int64_t seed = 0;
  if( !inited ){
    struct timeval tp;
    union { double d; int64_t i; int32_t h[2]; } u;
    
    gettimeofday(&tp, NULL);

    u.d = (double) tp.tv_sec + (double) tp.tv_usec / 1e6;
#if defined(__arm__) && !defined(__ARM_EABI__)
    { int32 t = u.h[0]; u.h[0] = u.h[1]; u.h[1] = t; }
#endif
    seed = u.i;
    inited = 1;
  }
  return seed;
}


CAMLexport void ocp_memprof_scanmult (char *opt, uintnat *var)
{
  char mult = ' ';
  unsigned int val;
  sscanf (opt, "=%u%c", &val, &mult);
  sscanf (opt, "=0x%x%c", &val, &mult);
  switch (mult) {
  case 'k':   *var = (uintnat) val * 1024; break;
  case 'M':   *var = (uintnat) val * 1024 * 1024; break;
  case 'G':   *var = (uintnat) val * 1024 * 1024 * 1024; break;
  default:    *var = (uintnat) val; break;
  }
}
