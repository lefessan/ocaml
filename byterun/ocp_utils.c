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































#if 0
  
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
        caml_memprof_do_init();
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


CAMLexport char *caml_gcprof_young_space = NULL;
CAMLexport uintnat caml_gcprof_profile_period = 0;
CAMLexport uintnat caml_gcprof_profile_major = 0;


/* Needed to prevent the GC from calling caml_memprof_next_thread
  during compaction, since it would access the thread_id which is
  stored in a heap block, not accessible during Compact.invert_at. */
CAMLexport int caml_heapdump_in_dump;

/* we first try to load 'ocp_memprof_init2' to provide more 
information to the plugin. If it fails, we call 'ocp_memprof_init'
providing no information :-( */

static void caml_memprof_do_init()
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

static void caml_memprof_init_shared_mem();
static void caml_memprof_exit_shared_mem();
  
CAMLexport void caml_memprof_init()
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
        caml_memprof_do_init();
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

  caml_memprof_init_shared_mem();
}



char* ocp_debug_init=0;

void memprof_debug_init(value ui_name)
{
  static int debug = -1;
  if( debug < 0){
    if( getenv("OCP_DEBUG_INIT") == NULL ) debug = 0;
    else debug = 1;
  }
  if(debug) {
    fprintf(stderr, "module:%s\n", String_val(ui_name)); fflush(stderr);
  }
}

#ifdef ARCH_SIXTYFOUR


/* These globals are here because we want to access it from the debugger. 
They are also in the shared library, because we want some compatibility
with older version of ocp-memprof. */
int caml_ocp_location_tables_max = 0;
int caml_ocp_next_location_id = 0;
char **caml_ocp_location_tables = NULL;
mlsize_t *caml_ocp_location_tables_sizes = NULL;
int caml_ocp_location_tables_next = 0;



static int caml_ocp_register_loc_table (char* table, mlsize_t len, int elems)
{
  int offset;
  
  if(caml_ocp_location_tables_next >= caml_ocp_location_tables_max) {
    caml_ocp_location_tables_max += 1;
    caml_ocp_location_tables_max *= 2;
    caml_ocp_location_tables =
      caml_stat_resize(caml_ocp_location_tables,
                       caml_ocp_location_tables_max * sizeof(value));
    caml_ocp_location_tables_sizes =
      caml_stat_resize(caml_ocp_location_tables_sizes,
                       caml_ocp_location_tables_max * sizeof(mlsize_t));
  }

  // FIXME: test next_location_id < (2^22)...
  offset = caml_ocp_next_location_id;
  caml_ocp_next_location_id += elems;

  caml_ocp_location_tables[caml_ocp_location_tables_next] = table;
  caml_ocp_location_tables_sizes[caml_ocp_location_tables_next] = len;
  caml_ocp_location_tables_next++;

  if( (caml_ocp_next_location_id & 0x3fffff) != caml_ocp_next_location_id){
    fprintf(stderr,"Warning: locid limit reached = 0x%x\n",
            caml_ocp_next_location_id);
    return 0;
  }

  return offset;
}



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

static void caml_ocp_register_externals(void) {

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
  caml_ocp_register_loc_table(buf, len, PROF_LAST_EXTERNAL);

  CAMLreturn0;
}

value caml_memprof_register_table (value table, value elems, value ui_name)
{
  static value (*ocp_memprof_register_table) (value table, value elems, value ui_name) = NULL;

  memprof_debug_init(ui_name);

  
  if(caml_ocp_location_tables_max == 0){

    /* Initialise the location table */
    caml_ocp_location_tables_max = 128;
    caml_ocp_location_tables =
      caml_stat_alloc(caml_ocp_location_tables_max * sizeof(value));
    caml_ocp_location_tables_sizes =
      caml_stat_alloc(caml_ocp_location_tables_max * sizeof(mlsize_t));
    caml_ocp_register_externals();
  }

  {  
    int first_offset = caml_ocp_register_loc_table(String_val(table),
                                                caml_string_length(table),
                                                Int_val(elems));
  
    if( caml_memprof_dll_handle != NULL ){
      if( ocp_memprof_register_table == NULL ) {
        ocp_memprof_register_table =
          caml_memprof_load_symbol("ocp_memprof_register_table");
      }
      value second_offset_v = ocp_memprof_register_table(table, elems, ui_name);
      if( Val_int(first_offset) != second_offset_v){
        fprintf(stderr,"Warning: inconsistent location tables %d<>%d\n",
                first_offset, Int_val(second_offset_v));
      }
    }
    return Val_int(first_offset);
  }
}

#else

value caml_memprof_register_table (value table, value elems, value ui_name)
{
  memprof_debug_init(ui_name);
  return Val_int(0);
}

#endif





VOID_MEMPROF_STUB0(caml_memprof_exit2, ocp_memprof_exit)

void caml_memprof_exit()
{
  caml_memprof_exit_shared_mem();  
  caml_memprof_exit2();
}


VOID_MEMPROF_STUB0(caml_memprof_compact_begin, ocp_memprof_compact_begin)
VOID_MEMPROF_STUB3(caml_memprof_compact_move, ocp_memprof_compact_move,
                   void*, p, void*, newaddr, size_t, sz)
VOID_MEMPROF_STUB0(caml_memprof_compact_end, ocp_memprof_compact_end)

RET_MEMPROF_STUB3(int, caml_memprof_register_loc_table,
                  ocp_memprof_register_loc_table,
                  char*, table,mlsize_t, len, int, elems, 0)

#ifndef NATIVE_CODE

/* This table indicates, for a given instruction in the main code segment,
   the offset with the original instruction (used for backtraces...). */
CAMLexport code_t caml_memprof_translation_table;

RET_MEMPROF_STUB2(code_t, caml_memprof_bytecode_fix_locids,
                  ocp_memprof_bytecode_fix_locids,
                  code_t, src_code, asize_t*, src_size, src_code);
VOID_MEMPROF_STUB1(caml_memprof_bytecode_init,
                   ocp_memprof_bytecode_init,
                   char *, memprof_section)

#endif

VOID_MEMPROF_STUB3( caml_memprof_next_thread, ocp_memprof_next_thread,
                    value, thread_id, char*, bottom_of_stack,
                    uintnat, retaddr)
VOID_MEMPROF_STUB2( caml_memprof_change_thread, ocp_memprof_change_thread,
                    value, thread_id, int, change)

CAMLexport char *caml_allocprof_young_ptr;
VOID_MEMPROF_STUB2( caml_allocprof_future_minor_alloc,
                     ocp_allocprof_future_minor_alloc,
                    char*, young_ptr,
                    mlsize_t, size_plus_header)
VOID_MEMPROF_STUB0( caml_allocprof_minor_collection,
                     ocp_allocprof_minor_collection)
VOID_MEMPROF_STUB3( caml_allocprof_alloc_major,
                     ocp_allocprof_alloc_major,
                    mlsize_t, sz, tag_t, tag,profiling_t, locid)
VOID_MEMPROF_STUB1( caml_gcprof_gctime,
                    ocp_gcprof_gctime,
                    int, gcevent)

CAMLexport void caml_memprof_dependencies()
{
  caml_major_gc_hook = NULL;
}


CAMLprim value caml_memprof_control(value arg_v)
{
  CAMLparam1(arg_v);
  CAMLlocal1(ret_v);

  static value (*ocp_name) (value) = NULL;
  if( caml_memprof_dll_handle != NULL ){
    if( ocp_name == NULL ) {
      ocp_name = caml_memprof_load_symbol("ocp_memprof_control");
    }
    ret_v = ocp_name(arg_v);
  } else {
    ret_v = Val_unit;
  }

  CAMLreturn(ret_v);
}


/**************************************************************************/
/*                                                                        */
/*   Monitoring system: create a file in /tmp/ocperfdata_$UID/ for every  */
/* running OCaml program, containing shared information.                  */
/*                                                                        */
/*                                                                        */
/**************************************************************************/











#if defined(_WIN32) || defined(_WIN64)
#define OCP_SHARED_MEM_FOR_WINDOWS
#endif

#ifdef OCP_SHARED_MEM_FOR_WINDOWS

#include <windows.h>
#include <conio.h>
#include <tchar.h>

#define SHARED_FD HANDLE
#define CLOSE_SHARED_FD CloseHandle
#define STORE_FD(x) (value)(x)
#define RESTORE_FD(x) (SHARED_FD)(x)

#else /* FOR LINUX */

#include <stdint.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

#include <sys/mman.h> /* for mmap */

#define SHARED_FD int
#define STORE_FD(x) Val_int(x)
#define RESTORE_FD(x) Int_val(x)
#define CLOSE_SHARED_FD close

#endif

typedef struct monitored_value_declaration {
  uint16_t id;
  char* name;
  uint8_t type;
} monitored_value_declaration;

#define OCP_MONITOR_SIZE 32768
#define OCP_MONITOR_MAGIC "OCP-2015S001"
#define OCP_MONITOR_MAGIC_LENGTH 12
#define OCP_MONITOR_VERSION   1

#define OCP_MONITOR_TYPE_CONST_UINT32  1
#define OCP_MONITOR_TYPE_CONST_STRING  2
#define OCP_MONITOR_TYPE_CONST_STRINGS 3
#define OCP_MONITOR_TYPE_CONST_DOUBLE  4
#define OCP_MONITOR_TYPE_MUTAB_UINT32  5
#define OCP_MONITOR_TYPE_MUTAB_DOUBLE  6

#define OCP_MONITOR_VALUE_TIME_BEGIN      1
#define OCP_MONITOR_VALUE_TIME_END        2
#define OCP_MONITOR_VALUE_COMMAND         3
#define OCP_MONITOR_VALUE_ARGV            4
#define OCP_MONITOR_VALUE_MINOR_WORDS     5
#define OCP_MONITOR_VALUE_PROMOTED_WORDS  6
#define OCP_MONITOR_VALUE_MAJOR_WORDS     7
#define OCP_MONITOR_VALUE_MINOR_GC        8
#define OCP_MONITOR_VALUE_MAJOR_GC        9
#define OCP_MONITOR_VALUE_HEAP_WORDS         10
#define OCP_MONITOR_VALUE_TOP_HEAP_WORDS     11
#define OCP_MONITOR_VALUE_COMPACTIONS        12
#define OCP_MONITOR_VALUE_FREE_WORDS         13 
#define OCP_MONITOR_VALUE_MINOR_SIZE         14
#define OCP_MONITOR_VALUE_MINOR_FREE         15
#define OCP_MONITOR_VALUE_STACK_USAGE        16
#define OCP_MONITOR_VALUE_IS_BYTECODE        17
#define OCP_MONITOR_VALUE_TRIGGER_GC         18
#define OCP_MONITOR_VALUE_TRIGGER_DUMP       19
#define OCP_MONITOR_VALUE_ARGS               20
/* OCP_MONITOR_VALUES: last_id+1 */
#define OCP_MONITOR_NVALUES  21

static monitored_value_declaration values[] = {
  { 0, NULL, 0 },
  { 1, "ProcTimeBegin", OCP_MONITOR_TYPE_CONST_DOUBLE },
  { 2, "ProcTimeEnd", OCP_MONITOR_TYPE_MUTAB_DOUBLE },
  { 3, "ProcCmd", OCP_MONITOR_TYPE_CONST_STRING },
  { 4, "ProcArgv", OCP_MONITOR_TYPE_CONST_STRINGS },
  { 5, "MinorWords", OCP_MONITOR_TYPE_MUTAB_DOUBLE },
  { 6, "PromotedWords", OCP_MONITOR_TYPE_MUTAB_DOUBLE },
  { 7, "MajorWords", OCP_MONITOR_TYPE_MUTAB_DOUBLE },
  { 8, "MinorGC", OCP_MONITOR_TYPE_MUTAB_UINT32 },
  { 9, "MajorGC", OCP_MONITOR_TYPE_MUTAB_UINT32 },
  { 10, "HeapWords", OCP_MONITOR_TYPE_MUTAB_DOUBLE },
  { 11, "TopHeapWords", OCP_MONITOR_TYPE_MUTAB_DOUBLE },
  { 12, "Compactions", OCP_MONITOR_TYPE_MUTAB_UINT32 },
  { 13, "FreeWords", OCP_MONITOR_TYPE_MUTAB_DOUBLE },
  { 14, "MinorSize", OCP_MONITOR_TYPE_MUTAB_DOUBLE },
  { 15, "MinorFree", OCP_MONITOR_TYPE_MUTAB_DOUBLE },
  { 16, "StackUsage", OCP_MONITOR_TYPE_MUTAB_DOUBLE },
  { 17, "IsBytecode", OCP_MONITOR_TYPE_CONST_UINT32 },
  { 18, "TriggerGC",  OCP_MONITOR_TYPE_MUTAB_UINT32 },
  { 19, "TriggerDump", OCP_MONITOR_TYPE_MUTAB_UINT32 },
  { 20, "ProcArgs", OCP_MONITOR_TYPE_CONST_UINT32 },
  { 0, NULL, 0 },
};

typedef struct monitor_symbol {
  uint16_t pos;
  uint16_t len;
} monitor_symbol;

typedef struct monitored_value {
  uint16_t id;
  monitor_symbol name;
  uint16_t type;
  char data[8];
} monitored_value;

typedef struct monitor_file {
  char  magic[OCP_MONITOR_MAGIC_LENGTH];
  uint16_t version;
  uint16_t nvalues;
  /* values are aligned on 16 bytes */
  monitored_value values[OCP_MONITOR_NVALUES];
  uint16_t textlen;
  char strings[8];
} monitor_file;

#define MONITOR_UINT32(m,pos) (*(uint32_t*)(&(m)->values[pos].data))
#define MONITOR_DOUBLE(m,pos) (*(double*)(&(m)->values[pos].data))
#define MONITOR_SET_STRING(m,pos,s,len)                                 \
  add_string(m, (monitor_symbol*)(&(m)->values[pos].data), s, len)


CAMLexport monitor_file* ocp_memprof_shared_mem = NULL;
static SHARED_FD ocp_memprof_shared_mem_fd;
static char* ocp_memprof_shared_mem_filename = NULL;
static int ocp_memprof_shared_mem_pid = -1;

static void
add_string(monitor_file* m, monitor_symbol* dst, char* src, int srclen)
{
  int pos = m->textlen;
  dst->len = srclen;
  dst->pos = sizeof(monitor_file) - 8 + pos;
  m->textlen = pos + srclen+1;
  memcpy(m->strings + pos, src, srclen);
  m->strings[pos+srclen] = 0;
}
extern char ** caml_main_argv;

static void define_value(monitor_file* m, int value, int id,
                         int type, char* name)
{
  m->values[value].id = id;
  m->values[value].type = type;
  if( name != NULL) 
    add_string(m, & m->values[value].name, name,strlen(name));
}

/* for "gettimeofday" */
#include <sys/types.h>
#include <sys/time.h>
/* for "kill" */
#include <signal.h>
/* for "waitpid" */
#include <sys/wait.h>

static double get_current_time()
{
  struct timeval tp;
  if (gettimeofday(&tp, NULL) == -1) return 0.0;
  return (double)((double) tp.tv_sec + (double) tp.tv_usec / 1e6);
}

static void ocp_memprof_init_monitor()
{
  monitor_file* m = ocp_memprof_shared_mem;
  int i;

  memset(m, 0, sizeof(monitor_file));
  memcpy(&m->magic, OCP_MONITOR_MAGIC, OCP_MONITOR_MAGIC_LENGTH);
  m->version=OCP_MONITOR_VERSION;
  m->nvalues=OCP_MONITOR_NVALUES;

  for(i = 0; i<OCP_MONITOR_NVALUES; i++){
    define_value(m, i, values[i].id, values[i].type, values[i].name);
  }  

  MONITOR_DOUBLE(m, OCP_MONITOR_VALUE_TIME_BEGIN) = get_current_time();
  MONITOR_DOUBLE(m, OCP_MONITOR_VALUE_TIME_END) = 0;
  MONITOR_SET_STRING(m, OCP_MONITOR_VALUE_COMMAND,
                 caml_exe_name, strlen(caml_exe_name));
#ifdef NATIVE_CODE
  MONITOR_UINT32(m, OCP_MONITOR_VALUE_IS_BYTECODE) = 0;
#else
  MONITOR_UINT32(m, OCP_MONITOR_VALUE_IS_BYTECODE) = 1;
#endif
  {  /* Limit the size of arguments to less than 1024 chars */
    int i, nargs = 0;
    monitor_symbol* args;
    int arglen = 0;
    int maxnargs = 0; /* maximal number of arguments to store, within
                         the 1024 chars limitation */
    for(; caml_main_argv[nargs] != NULL; nargs++){
      char* arg = caml_main_argv[nargs];
      int len = strlen( arg );
      if( maxnargs == 0 ){
        if( arglen + len > 1024 ){
          maxnargs = nargs;
        } else {
          arglen += len;
        }
      }
    }
    if( maxnargs == 0 ){
      if( arglen > 0 ){
        maxnargs = nargs; /* store all args */
      } else {
        maxnargs = 1; /* first arg is too big, try to store its prefix */
      }
    }
    MONITOR_UINT32(m, OCP_MONITOR_VALUE_ARGS) = nargs;
    args = (monitor_symbol*) malloc((maxnargs) * sizeof(monitor_symbol));
    for(i = 0; i<maxnargs; i++){
      int len = strlen(caml_main_argv[i]);
      if( len > 1024 ) len = 1024; /* can only happen for first arg */
      add_string(m, args+i, caml_main_argv[i], len);
    }
    MONITOR_SET_STRING(m, OCP_MONITOR_VALUE_ARGV,
                   (char*)args, (maxnargs) * sizeof(monitor_symbol));
    free(args);
  }
  
  msync( ocp_memprof_shared_mem, OCP_MONITOR_SIZE, MS_ASYNC);
}

#define EXIT_FREE(buf)  { free(buf); return; }

static void caml_memprof_exit_shared_mem()
{
  if( ocp_memprof_shared_mem != NULL ){
    monitor_file* m = ocp_memprof_shared_mem;
    MONITOR_DOUBLE(m, OCP_MONITOR_VALUE_TIME_END) = get_current_time();
    ocp_monitor_update_gc_stats();
    msync(ocp_memprof_shared_mem, OCP_MONITOR_SIZE, MS_SYNC);
    munmap(ocp_memprof_shared_mem, OCP_MONITOR_SIZE);
    ocp_memprof_shared_mem = NULL;
  }
  if( ocp_memprof_shared_mem_filename != NULL ){
    close( ocp_memprof_shared_mem_fd );
    remove(ocp_memprof_shared_mem_filename);
    /*    rmdir(dirname(ocp_memprof_shared_mem_filename)); */
    free( ocp_memprof_shared_mem_filename );
    ocp_memprof_shared_mem_filename = NULL;
  }
}

static void caml_memprof_init_shared_mem()
{
  if( getenv("OCPERFDATA_DISABLED") == NULL ){
    uid_t uid = getuid();
    char *tmpdir = getenv("OCPERFDATA_TMPDIR");
    struct stat st;
    int ret;
    char* addr;
    char* buf;
    int pid;
    
    if( sizeof(monitored_value) != 16){
      fprintf(stderr, "sizeof(monitored_value) = %d\n",
              (int)sizeof(monitored_value));
      exit(3);
    }
    if(tmpdir == NULL) tmpdir = "/tmp";
    buf = (char*)malloc(strlen(tmpdir) + 30);
    if(buf == NULL) return;
    sprintf(buf, "%s/ocperfdata_%d", tmpdir, uid);
    ret = stat(buf, &st);
    if( ret != 0 ){
      if( errno == ENOENT ){ mkdir(buf, 0700); }
      ret = stat(buf, &st);
    }
    ret = access( buf, R_OK | W_OK | X_OK );
    if( ret != 0 ) EXIT_FREE(buf);
    pid = getpid();
    sprintf(buf, "%s/ocperfdata_%d/%d", tmpdir, uid, pid);
#ifdef OCP_SHARED_MEM_FOR_WINDOWS
    {
      SHARED_FD fmap;
      SECURITY_ATTRIBUTES attr;
      attr.nLength = sizeof(attr);
      attr.lpSecurityDescriptor = NULL;
      attr.bInheritHandle = FALSE;

      ocp_memprof_shared_mem_fd = CreateFile(buf, O_TRUNC | O_CREAT,
                                             FILE_SHARE_READ | FILE_SHARE_WRITE, &attr,
                                             CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);
      if(ocp_memprof_shared_mem_fd == NULL) EXIT_FREE(buf);
      ocp_memprof_shared_mem_filename = buf;
      fmap = CreateFileMapping(ocp_memprof_shared_mem_fd,
                               NULL, PAGE_READWRITE, 0, 32768, NULL);
      addr =
        MapViewOfFile(fmap, FILE_MAP_WRITE, 0, 0, 32768);
    }
#else
    ocp_memprof_shared_mem_fd = open(buf, O_RDWR | O_TRUNC | O_CREAT, 0600);
    if(ocp_memprof_shared_mem_fd < 0) EXIT_FREE(buf);
    fcntl(ocp_memprof_shared_mem_fd, F_SETFD, FD_CLOEXEC);
    ocp_memprof_shared_mem_filename = buf;
    ftruncate(ocp_memprof_shared_mem_fd, 32768);
    addr = mmap(NULL, 32768, PROT_READ|PROT_WRITE, MAP_SHARED,
                ocp_memprof_shared_mem_fd, 0);
    {
      int pid;
      pid = fork();
      if( pid == 0 ){
        int ppid = getppid();
        /* close all FDs immediatly: some of them might contain unflushed
           data, that would be printed twice otherwise. */
        int maxfd=sysconf(_SC_OPEN_MAX);
        int fd;
        for(fd=0; fd<maxfd; fd++) close(fd);
        pid = fork();
        if( pid == 0 ){
          char arg[20];
          sprintf(arg, "%d", ppid);
          setsid();
          if( execlp("ocp-monitor-pid", "ocp-monitor-pid", arg, "-rm",
                     ocp_memprof_shared_mem_filename, NULL) < 0 ){
            while(1){
            sleep(1);
            if( kill(ppid, 0) < 0 ){ /* monitored process is dead */
              remove(ocp_memprof_shared_mem_filename);
              exit(0);
            }
          }
        }
      } else {
        exit(0);
      }
    } else {
      waitpid(pid, NULL, 0);
    }
  }
#endif
  if( addr == NULL ) return;
  ocp_memprof_shared_mem_pid = pid;
  ocp_memprof_shared_mem = (monitor_file*) addr;
  ocp_memprof_init_monitor();
  return;
}
}

#ifdef NATIVE_CODE
#include "stack.h"
#else
#include "stacks.h"
#endif

#define CAML_MORE_ALLOC_H
#define CAML_MORE_CALLBACK_H
#include "callback.h"



extern double caml_stat_minor_words,
       caml_stat_promoted_words,
       caml_stat_major_words;

extern intnat caml_stat_minor_collections,
       caml_stat_major_collections,
       caml_stat_heap_size,              /* bytes */
       caml_stat_top_heap_size,          /* bytes */
       caml_stat_compactions,
       caml_stat_heap_chunks;

extern uintnat caml_major_heap_increment;  /* bytes; see major_gc.c */
extern uintnat caml_percent_free;          /*        see major_gc.c */
extern uintnat caml_percent_max;           /*        see compact.c */
extern uintnat caml_allocation_policy;     /*        see freelist.c */

int ocp_monitor_update_gc_stats()
{
  if( ocp_memprof_shared_mem != NULL &&
      ocp_memprof_shared_mem_pid == getpid() 
      ){
    monitor_file* m = ocp_memprof_shared_mem;
  double minwords = caml_stat_minor_words
                    + (double) Wsize_bsize (caml_young_end - caml_young_ptr);
  double prowords = caml_stat_promoted_words;
  double majwords = caml_stat_major_words + (double) caml_allocated_words;
  intnat mincoll = caml_stat_minor_collections;
  intnat majcoll = caml_stat_major_collections;
  intnat heap_words = caml_stat_heap_size / sizeof (value);
  intnat top_heap_words = caml_stat_top_heap_size / sizeof (value);
  intnat cpct = caml_stat_compactions;
  /* get a copy of these before allocating anything... 
  intnat heap_chunks = caml_stat_heap_chunks;
  */
  int trigger_gc = MONITOR_UINT32(m, OCP_MONITOR_VALUE_TRIGGER_GC );

  if(trigger_gc){
    MONITOR_UINT32(m, OCP_MONITOR_VALUE_TRIGGER_GC ) = 0;
  }
    MONITOR_DOUBLE(m, OCP_MONITOR_VALUE_MINOR_WORDS) = minwords;
    MONITOR_DOUBLE(m, OCP_MONITOR_VALUE_PROMOTED_WORDS ) = prowords;
    MONITOR_DOUBLE(m, OCP_MONITOR_VALUE_MAJOR_WORDS ) = majwords;
    MONITOR_UINT32(m, OCP_MONITOR_VALUE_MINOR_GC ) = mincoll;
    MONITOR_UINT32(m, OCP_MONITOR_VALUE_MAJOR_GC ) = majcoll;
    MONITOR_DOUBLE(m, OCP_MONITOR_VALUE_HEAP_WORDS ) = heap_words;
    MONITOR_DOUBLE(m, OCP_MONITOR_VALUE_TOP_HEAP_WORDS ) = top_heap_words;
    MONITOR_UINT32(m, OCP_MONITOR_VALUE_COMPACTIONS ) = cpct;
    MONITOR_DOUBLE(m, OCP_MONITOR_VALUE_FREE_WORDS ) = caml_fl_cur_size;
    MONITOR_DOUBLE(m, OCP_MONITOR_VALUE_MINOR_SIZE ) =
      caml_minor_heap_size / sizeof(value);
    MONITOR_DOUBLE(m, OCP_MONITOR_VALUE_MINOR_FREE ) =
      (caml_young_ptr - caml_young_start) / sizeof(value);
    MONITOR_DOUBLE(m, OCP_MONITOR_VALUE_STACK_USAGE ) =
      caml_stack_usage();
    
    msync( ocp_memprof_shared_mem, OCP_MONITOR_SIZE, MS_ASYNC);

    return trigger_gc;
  }
  return 0;
}

CAMLexport value caml_md5_from_channel(struct channel * chan, value len)
{
  return caml_md5_channel(chan, Long_val(len));
}

CAMLexport void caml_iter_named_value(caml_named_action f)
{
  caml_iterate_named_values(f);
}

#ifdef NATIVE_CODE

/* Linked-list */
typedef struct caml_named_link {
  char* name;
  void *data;
  struct caml_named_link *next;
} caml_named_link;

static caml_named_link *cons(char* name, void *data, caml_named_link *tl) {
  caml_named_link *lnk = caml_stat_alloc(sizeof(caml_named_link));
  lnk->name = name;
  lnk->data = data;
  lnk->next = tl;
  return lnk;
}

#define iter_list(list,lnk)                             \
  for (lnk = list; lnk != NULL; lnk = lnk->next)


static caml_named_link * caml_dyn_globals_map = NULL;

CAMLexport void caml_register_dynlink_global(char *val_name, void* sym)
{
  int name_size = strlen(val_name);
  char *name = caml_stat_alloc(name_size + 1);

  memcpy(name, val_name, name_size + 1);

  caml_dyn_globals_map = cons(name, sym, caml_dyn_globals_map);
  caml_register_dyn_global(sym);
}

void caml_do_dynglobals (dyn_scanning_action f)
{
  caml_named_link *lnk;

  iter_list(caml_dyn_globals_map, lnk) {
    value glob = (value) lnk->data;
    char* name = lnk->name;
    int size = Wosize_val(glob);
    int j;
    for (j = 0; j < size; j++){
      f (name, Field (glob, j));
    }
  }
}

alloc_stats * caml_frame_stats = NULL;

CAMLexport value caml_make_header(mlsize_t wosize, tag_t tag, int color)
{
  return  Make_header(wosize, tag, color);
}

#endif



#endif // #if 0
