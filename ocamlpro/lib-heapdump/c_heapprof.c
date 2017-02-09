/**************************************************************************/
/*                                                                        */
/*   Copyright 2014-2017 OCamlPro SAS --- typerex-memprof                 */
/*                                                                        */
/*   Do not redistribute, without permission of OCamlPro, source          */
/*   or binary copies of this software.                                   */
/*                                                                        */
/**************************************************************************/

#define _OCP_OCAML

#define MEMPROF_INSIDE
#define CAML_INTERNALS

#define CAML_MORE_MD5_H

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
#include "caml/backtrace_prim.h"

#define CAML_MORE_CALLBACK_H
#include "caml/ocp_utils.h"
#include "caml/ocp_memprof.h"

#ifdef _WIN32
extern int32 win32_getppid_c(void);
#endif

#define ATOM MAKE_ATOM
#include "caml/instruct.h"
#undef ATOM

#ifndef NATIVE_CODE
#include "caml/fix_code.h"
#include "caml/stacks.h"
#define caml_code_area_start ((char *) caml_start_code)
#define caml_code_area_end ((char *) caml_start_code + caml_code_size)
#include "caml/startup.h"
#else
#include "caml/stack.h"
extern value* caml_globals[];
extern char caml_globals_map[];
#endif
extern value caml_make_vect(value len, value init);
extern char *caml_exe_name;

#define MIN(a,b) (((a)<(b))?(a):(b))

#ifndef PATH_MAX
#ifdef MAXPATHLEN
#define PATH_MAX MAXPATHLEN
#else
#define PATH_MAX 512
#endif
#endif

#include "c_heapprof.h"

#define FREEBLOCKS_SIZE 64

#define MAX_STR         65535
#define STR_SIZE        8
static char* str_tbl = NULL; /* max size MAX_STR * STR_SIZE */
static int str_id = 0;

#define Heapdump_magic_number "OCP-2014D001"
#define Heapdump_magic_number_len sizeof(Heapdump_magic_number)

/* Should be in 240..247 to interleave with 'store_offset'. */
/* 0b11110... */
#define CODE_INT         240
#define CODE_MAXINT      4
#define CODE_STATIC      244
#define CODE_NOT_A_VALUE 245
#define CODE_FUNCTION    246
#define CODE_RESTART     247

static int locid_size = 22;

static int block_count = 0;

static int heap_number = 0;
static int heapdump_inited = 0;
static char* heapdump_destdir = NULL;
static int heapdump_destdir_created = 0;


/* These functions are defined at the end of the file, because they
   work by side-effects on [file_oc]. They need to be so because
   they are called from iterators that do not provide another
   argument.
*/
static void dump_store_pointer_root(value v, value *fp);
static void dump_store_root(value v, value *useless);
static void dump_store_named(value *v, char* name);
#ifdef NATIVE_CODE
static void dump_store_dynglobal_root(char *name, value v);
#endif

void (*ocp_heapdump_hook)(FILE *) = NULL;
int ocp_heapdump_hook_process = 0;

/* The GC dumping mode. This is used when a hook has been installed
after every GC to decide whether a dump should be generated. Set by
reading OCP_MEMPROF_FLAGS. 0 means auto-adapt dump, 1 a dump at every
major GC, > 1 a period between GC.
*/
int ocp_heapdump_after_gc = 0;

/* Whether a dump should contain the pointer graph */
int ocp_heapdump_has_pointer_graph = 0;


/* In which format we save the dump file */
int ocp_format_version = 1;

#ifdef DEBUG

static int small_block_count = 0;
static int medium_block_count = 0;
static int huge_block_count = 0;
static int freeblock_count = 0;
static int small_freeblock_count = 0;
static int huge_freeblock_count = 0;
static int offset_count[8];
static int offset_total = 0;

#define COUNT_OFFSET(i) { offset_count[(i)]++; offset_total++; }
#define COUNT_SMALL_BLOCK() { small_block_count++; }
#define COUNT_MEDIUM_BLOCK() { medium_block_count++; }
#define COUNT_HUGE_BLOCK() { huge_block_count++; }
#define COUNT_SMALL_FREEBLOCK() { small_freeblock_count++; freeblock_count++; }
#define COUNT_HUGE_FREEBLOCK() { huge_freeblock_count++; freeblock_count++; }

#else

#define COUNT_OFFSET(i) {}
#define COUNT_SMALL_BLOCK() {}
#define COUNT_MEDIUM_BLOCK() {}
#define COUNT_HUGE_BLOCK() {}
#define COUNT_SMALL_FREEBLOCK() {}
#define COUNT_HUGE_FREEBLOCK() {}

#endif

/* return pid, heap_number and destdir */
CAMLexport char* ocp_heapdump_get_destdir(int *pid, int *heap_num)
{ 
  if( pid != NULL){ *pid = HEAPDUMP_GETPID(); }
  if( heap_num != NULL){ *heap_num = heap_number; }
  return heapdump_destdir; 
}

static char *absolute_path(char *path) {
#ifdef _WIN32
#define BUFSIZE 4096
  static char pathbuf[BUFSIZE];
  DWORD  retval = GetFullPathNameA(path, BUFSIZE, pathbuf, NULL);
  if (retval == 0) return NULL;
  else {
    char *realpath = (char*)malloc(retval+1);
    memcpy(realpath, pathbuf, retval+1);
    return realpath;
  }
#else
  if (path[0] != '/') {
    char *res = caml_stat_alloc(PATH_MAX);
    char basedir[PATH_MAX];
    if (getcwd(basedir, sizeof(basedir)) == 0) {
      perror("getcwd");
      return NULL;
    }
    sprintf(res, "%s/%s", basedir, path);
    return res;
  } else {
    return path;
  }
#endif
}

#ifdef _WIN32
static char *ocp_strndup(char const *s, size_t n) {
   char *d = (char *) malloc(n + 1);
   if (NULL != d) {
     strncpy(d, s, n);
   }
   return d;
 }
#endif

#if 0
/* [get_dirname(path)] returns a malloced string containing the 
   dirname of [path] */
static char *get_dirname(char *path)
{
#ifdef _WIN32
  char path_buffer[_MAX_PATH];
  char drive[_MAX_DRIVE];
  char dir[_MAX_DIR];

  _splitpath( path, drive, dir, NULL, NULL );
  _makepath( path_buffer, drive, dir, NULL, NULL);
  return strdup(path_buffer);
#else
 /* Since dirname() may modify its argument, we must first make a
     copy.  Using strndup(path, MAX_PATH) does not make sense to avoid
     a buffer overflow unless MAX_PATH is the real size of path, no
     ? */
  char *newpath = strdup(path);
  char *dir = dirname(newpath);
  if( dir == newpath ){ return dir; }
  free (newpath);
  return strdup(dir);
#endif
}
#endif

static int heapdump_mkdir(char *path) {

  struct stat sb;

  if (HEAPDUMP_LSTAT(path, &sb) == -1) {
    if (errno == ENOENT) {
      char *subpath = HEAPDUMP_STRNDUP(path, PATH_MAX);
      subpath = HEAPDUMP_DIRNAME(subpath);
      if (!heapdump_mkdir(subpath)) return 0;
      caml_stat_free(subpath);
      if (HEAPDUMP_MKDIR(path) < 0) {
        perror("mkdir");
        fprintf(stderr,
                "Heapdump: error while creating the destination directory: \"%s\"\n",
                path);
        return 0;
      }
      return 1;
    } else {
      perror("lstat");
      fprintf(stderr,
              "Heapdump: error while creating the destination directory: \"%s\"\n",
              path);
      return 0;
    }
  } else {
    if (sb.st_mode | S_IFDIR) {
      return 1;
    } else {
      fprintf(stderr,
              "Heapdump: the destination directory is not a directory: \"%s\".\n",
              path);
      return 0;
    }
  }

}

 /* set using 'p' in OCP_MEMPROF_FLAGS, used when 'm' is 1 */
static int ocp_heapdump_period = 1;

/* set using 't' in OCP_MEMPROF_FLAGS, used when 'm' is 2 */
static uint64_t ocp_heapdump_threshold = 0;
static uint64_t ocp_heapdump_new_threshold = 0;

static int ocp_heapdump_last_dump = 0;

static void do_dump_after_gc(void)
{
  int do_dump = 0;
  switch( ocp_heapdump_after_gc ){
  case 1 : /* one dump every 'p' major gc */
    do_dump = ((caml_stat_major_collections % ocp_heapdump_period) == 0 );
    break;
  case 2 : /* one dump everytime is passes a 't' memory threshold */
    if( caml_stat_heap_size >= ocp_heapdump_new_threshold ){
      do_dump = 1;
      if( ocp_heapdump_threshold == 0 ){
        ocp_heapdump_threshold = ocp_heapdump_new_threshold;
      }
      ocp_heapdump_new_threshold = caml_stat_heap_size + ocp_heapdump_threshold;
    }
    break;
  case 3 :
    if ( caml_stat_major_collections < 20 ){
      do_dump = 1;
    } else
      if ( caml_stat_major_collections < 50 ){
        do_dump = ( (caml_stat_major_collections) % 2 == 0);
      } else
      if ( caml_stat_major_collections < 100 ){
        do_dump = ( (caml_stat_major_collections) % 5 == 0);
      }
 else
      if ( caml_stat_major_collections < 500 ){
        do_dump = ( (caml_stat_major_collections) % 10 == 0);
      } else {
        do_dump = ( (caml_stat_major_collections) % 50 == 0);
      }
    break;
  }
  
  if( do_dump ){
    ocp_heapdump_last_dump = caml_stat_major_collections;
    ocp_heapdump_do_dump("after_major_collection", HEAPDUMP_MAJORGC);
  }
}

static int may_dump_on_signal(int signal_number)
{
  if (signal_number != caml_hooked_signal) return 1;
  caml_minor_collection(); /* to avoid revert pointers to minor heap */
  ocp_heapdump_do_dump("signal", HEAPDUMP_SIGNAL);
  return 0;
}

static void flush_option(char flag, uint64_t val)
{
  switch (flag){
  case 'm': /* dump after every GC */
    ocp_heapdump_after_gc = val;
    caml_major_gc_hook = do_dump_after_gc;
    break;
  case 'p': ocp_heapdump_period = val; break;
  case 't': ocp_heapdump_new_threshold = val; break;
  case 'P': /* choose signal to use */
    caml_hooked_signal = val * (-1); /* the signal need to be */
    caml_hooked_signal = caml_convert_signal_number(caml_hooked_signal);
    break;
  case 'g': /* enable pointer graph */
    ocp_heapdump_has_pointer_graph = 1;
    break;
  }
}

void ocp_heapdump_init() {
  
  char *process_info_filename;
  if (heapdump_inited) return;

  heapdump_destdir = getenv(HEAPDUMP_DESTDIR_VAR);
  if (heapdump_destdir == NULL) {
    heapdump_destdir = caml_stat_alloc(32);
    sprintf(heapdump_destdir, "memprof.%d", HEAPDUMP_GETPID());
  }

  heapdump_destdir = absolute_path(heapdump_destdir);
  if (heapdump_destdir == NULL) goto failure;

  // Compute the process_info_filename
  process_info_filename = caml_stat_alloc(strlen(heapdump_destdir)+32);
  sprintf(process_info_filename, "%s/memprof.%d.process_info",
          heapdump_destdir, HEAPDUMP_GETPID());

  if (unlink(process_info_filename) == -1 && errno != ENOENT) {
    perror("unlink");
    goto failure;
  }
  ocp_heapdump_pi_setname(process_info_filename);
  
  if (ocp_heapdump_is_applicable(caml_exe_name)){
    char *flags = getenv("OCP_MEMPROF_FLAGS");
    if( flags != NULL ){
      uint64_t val = 0;
      char flag = ' ';
      while (*flags != '\0'){
        char c = *flags++;
        if( c >= '0' && c <= '9' ){
          val = val*10 + (c - '0');
        } else {
        switch (c){
        case 'm': /* dump after every GC */
          flush_option(flag, val);
          flag = c; val = 1; /* auto-adapt */
          break;
        case 'P': /* choose signal to use */
        case 'g': /* enable pointer graph */
        case 'p': /* dump period */
        case 't': /* dump threshold */
          flush_option(flag, val);
          flag = c; val = 0;
          break;
        case '-':
          break;
        case '=':
          val = 0;
          break;
        case 'K': /* Kilobyte option */
          val = val * 1024;
          break;
        case 'M': /* Megabyte option */
          val = val * 1024 * 1024;
          break;
        case 'G': /* Gigabyte option */
          val = val * 1024 * 1024 * 1024;
          break;
        default:
          flush_option(flag, val);
          flag = ' '; val = 0;
          break;
        }
        }
      }
      flush_option(flag, val);
    }
  }

#ifndef _WIN32
  caml_execute_signal_hook = may_dump_on_signal;
  caml_set_signal_action(caml_hooked_signal, 0);
#endif

  heapdump_inited = 1;
  return;

 failure:
  heapdump_inited = -1;
  return;

}

/* Basic 'store' and 'read' functions */

static void store_word(value v, FILE *file_oc)
{
  int i;
  for (i = 0; i < sizeof(value) * 8 ; i += 8) putc(v >> i, file_oc);
}


/* store in compressed form, 7bits per char */
void ocp_heapdump_store_uint(uint64_t c, FILE* oc)
{
  uint64_t m = c >> 7;
  while( m != 0 ){
    putc( c & 0x7f, oc);
    c = m;
    m = c >> 7;
  }
  putc( c|0x80 , oc);
}
#define store_uint ocp_heapdump_store_uint

static void store_int8(value v, FILE *file_oc) { putc(v, file_oc); }

/* Content table: addresses and size for chunk and native globals. */
/* CHKS section */
static void store_chunk_table(FILE *file_oc)
{
  int count;
  char *chunk;

  store_word(block_count, file_oc);
  store_int8(ocp_heapdump_has_pointer_graph, file_oc);

  // Dump the list of chunks

  count = 0;
  for (chunk = caml_heap_start; chunk != NULL; chunk = Chunk_next(chunk))
    count++;
  store_word(count, file_oc);
  for (chunk = caml_heap_start; chunk != NULL; chunk = Chunk_next(chunk)) {
    store_word((intnat)chunk, file_oc);
    store_word(Chunk_size(chunk), file_oc);
  }

}



#define HP_OPCODE_STACK_UPWARDS 0
#define HP_OPCODE_FRAME_BEGIN   1
#define HP_OPCODE_STACK_END     2

#ifdef NATIVE_CODE

/* also in ROOT section */
static void store_backtrace(FILE * file_oc,
                            char* bottom_of_stack,
                            uintnat last_retaddr)
{
  char * sp;
  uintnat retaddr;
  frame_descr * d;

  if (caml_frame_descriptors == NULL) caml_init_frame_descriptors();

#ifdef Stack_grows_upwards
  store_int8(HP_OPCODE_STACK_UPWARDS, file_oc);
#endif
  sp = bottom_of_stack;
  retaddr = last_retaddr;
  if (sp != NULL) {
    while (1) {
      /* Find the descriptor corresponding to the return address */
      uintnat h = Hash_retaddr(retaddr);
      while(1) {
        d = caml_frame_descriptors[h];
        if (d->retaddr == retaddr) break;
        h = (h+1) & caml_frame_descriptors_mask;
      }
      if (d->frame_size != 0xFFFF) {
        store_int8(HP_OPCODE_FRAME_BEGIN, file_oc);
        store_word((value)sp, file_oc);
        store_uint(d->frame_size & 0xFFFC, file_oc);
        {
          struct caml_loc_info li;
          caml_extract_location_info(d, &li);
          if(li.loc_valid){
            int len = strlen(li.loc_filename);
            store_int8(1, file_oc);
            store_uint(len, file_oc);
            fwrite(li.loc_filename, len, 1, file_oc);
            store_uint(li.loc_lnum, file_oc);
            store_uint(li.loc_startchr, file_oc);
            store_uint(li.loc_endchr, file_oc);
          } else {
            store_int8(0, file_oc);
          }
        }
        /* Move to next frame */
#ifndef Stack_grows_upwards
        sp += (d->frame_size & 0xFFFC);
#else
        sp -= (d->frame_size & 0xFFFC);
#endif
        retaddr = Saved_return_address(sp);
      } else {
        /* This marks the top of a stack chunk for an ML callback.
           Skip C portion of stack and continue with next ML stack chunk. */
        struct caml_context * next_context = Callback_link(sp);
        sp = next_context->bottom_of_stack;
        retaddr = next_context->last_retaddr;
        /* A null sp means no more ML stack chunks; stop here. */
        if (sp == NULL) break;
      }
    }
  }
  store_int8(HP_OPCODE_STACK_END, file_oc);
}

#else

static void store_backtrace(FILE *file_oc, 
                            char* bottom_of_stack, uintnat retaddr)
{
  store_int8(HP_OPCODE_STACK_END, file_oc);
}
#endif


/* [root_count] is used only when dumping roots. It helps to know
   how many roots of a particular kind have been saved. */
static int root_count;


struct thread_mem {
  value thread_id;
#ifdef NATIVE_CODE
  char* bottom_of_stack;
  uintnat retaddr;
#endif
  int begin_count;
  int end_count;
  
  struct thread_mem* next;
};

static struct thread_mem* threads = NULL;

CAMLexport void ocp_memprof_next_thread(value thread_id,
                                        char* bottom_of_stack,
                                        uintnat retaddr)
{
  if( threads != NULL ){
    threads->end_count = root_count;
  }

  if(thread_id != 0){
    struct thread_mem* th = caml_stat_alloc(sizeof(struct thread_mem));

    th->thread_id = thread_id;
#ifdef NATIVE_CODE
    th->bottom_of_stack = bottom_of_stack;
    th->retaddr = retaddr;
#endif
    th->begin_count = root_count;
    th->end_count = root_count;
    
    th->next = threads;
    threads = th;
  }
}

static void store_threads(FILE* file_oc)
{
  while( threads != NULL ){
    struct thread_mem* next = threads->next;
    ocp_heapdump_store_uint(Int_val(threads->thread_id)+1, file_oc); /* always > 0 */
    ocp_heapdump_store_uint(threads->begin_count, file_oc);
    ocp_heapdump_store_uint(threads->end_count, file_oc);
#ifdef NATIVE_CODE
    store_backtrace(file_oc, threads->bottom_of_stack, threads->retaddr);
#else
    store_backtrace(file_oc, NULL, 0);
#endif

    caml_stat_free(threads);
    threads = next;
  }
  ocp_heapdump_store_uint(0, file_oc); /* no more threads */
}

/* ROOT section */
static void store_roots(FILE *file_oc)
{

  int globals_count, dyn_globals_count, stack_count;
  int c_count, finalised_count, hook_count;
#ifdef NATIVE_CODE
  int i, j;
  value* glob;
#endif

  // Adapted from [caml_do_root] in '{byterun,asmrun}/roots.c'

  /* The global roots */
#ifdef NATIVE_CODE
  for (i = 0; caml_globals[i] != 0; i++) {
    glob = caml_globals[i];
    store_word(Wosize_val(glob), file_oc);
    for (j = 0; j < Wosize_val(glob); j++) {
      value v = Field(glob, j);
      if (Is_block(v) && Is_in_heap(v) && Tag_val(v) == Infix_tag)
        store_word(v - Infix_offset_val(v), file_oc);
      else
        store_word(v, file_oc);
    }
  }
  globals_count = i;
#else
  store_word(caml_global_data, file_oc);
  globals_count = 0;
#endif

  /* Dynamic global roots */
  root_count = 0;
#ifdef NATIVE_CODE
  caml_do_dynglobals(dump_store_dynglobal_root);
#else
  /* Non-existent in bytecode: the module tuple may even be deallocated. */
#endif
  dyn_globals_count = root_count;

  /* The stack and local roots */
  root_count = 0;
#ifdef NATIVE_CODE
  if (caml_frame_descriptors == NULL) caml_init_frame_descriptors();
  caml_do_local_roots(dump_store_pointer_root, caml_bottom_of_stack,
                      caml_last_return_address,
                      caml_gc_regs, caml_local_roots);
#else
  caml_do_local_roots(dump_store_pointer_root, caml_extern_sp,
                      caml_stack_high, caml_local_roots);
#endif
  stack_count = root_count;

  /* Global C roots */
  root_count = 0;
  caml_scan_global_roots(dump_store_pointer_root);
  c_count = root_count;

  /* Finalised values */
  root_count = 0;
  caml_final_do_roots(dump_store_root);
  finalised_count = root_count;

  /* Hook */
  root_count = 0;
  if (caml_scan_roots_hook != NULL){
    (*caml_scan_roots_hook)(dump_store_pointer_root);
  }
  hook_count = root_count;

#ifdef NATIVE_CODE
  store_backtrace(file_oc, caml_bottom_of_stack, caml_last_return_address);
#else
  store_backtrace(file_oc, NULL, 0);
#endif

  store_threads(file_oc);
  caml_iterate_named_values(dump_store_named);
  ocp_heapdump_store_uint(0, file_oc); /* end of named roots */

  /* Store counts: LAST THING to do in this section ! 
     heapdump_reader.ml assumes these 6 values are stored
     at (section_size - 6 * sizeof(value))   */
  store_word(globals_count, file_oc);
  store_word(dyn_globals_count, file_oc);
  store_word(stack_count, file_oc);
  store_word(c_count, file_oc);
  store_word(finalised_count, file_oc);
  store_word(hook_count, file_oc);

  return;

}


/* Helpers for dumping pointer in block fields. */

/* We compute the number of significant bits, we keep one more bit for
   the sign. */

static void store_offset(intnat ofs, FILE* file_oc)
{
  int i;

  intnat abs_ofs = ofs > 0 ? ofs : (-ofs-1);
  if (!(abs_ofs >> 6)) {                 /* 6 significant bits */
    COUNT_OFFSET(0);
    putc( 0x00 | ((ofs      ) & 0x7f), file_oc);
  } else if (!(abs_ofs >> (5 + 1*8))) {  /* 13 significant bits */
    COUNT_OFFSET(1);
    putc( 0x80 | ((ofs >>  8) & 0x3f), file_oc);
    for (i = 0; i >= 0; i -= 8) putc(ofs >> i, file_oc);
  } else if (!(abs_ofs >> (4 + 2*8))) { /* 20 significant bits */
    COUNT_OFFSET(2);
    putc( 0xc0 | ((ofs >> 16) & 0x1f), file_oc);
    for (i = 8; i >= 0; i -= 8) putc(ofs >> i, file_oc);
  } else if (!(abs_ofs >> (3 + 3*8))) { /* 27 significant bits */
    COUNT_OFFSET(3);
    putc( 0xe0 | ((ofs >> 24) & 0x0f), file_oc);
    for (i = 16; i >= 0; i -= 8) putc(ofs >> i, file_oc);
#ifdef ARCH_SIXTYFOUR
  } else if (!(abs_ofs >> (1 + 4*8))) {/* 33 significant bits */
    COUNT_OFFSET(4);
    putc( 0xf8 | ((ofs >> 32) & 0x03), file_oc);
    for (i = 24; i >= 0; i -= 8) putc(ofs >> i, file_oc);
  } else if (!(abs_ofs >> (0 + 5*8))) { /* 40 significant bits */
    COUNT_OFFSET(5);
    putc( 0xfc | ((ofs >> 40) & 0x01), file_oc);
    for (i = 32; i >= 0; i -= 8) putc(ofs >> i, file_oc);
  } else if (!(abs_ofs >> (7 + 5*8))) { /* 47 significant bits */
    COUNT_OFFSET(6);
    putc( 0xfe , file_oc);
    for (i = 40; i >= 0; i -= 8) putc(ofs >> i, file_oc);
#endif
  } else {
    COUNT_OFFSET(7);
    putc(0xff, file_oc);
    for (i = (sizeof(intnat)*8)-8 ; i >= 0; i -= 8) putc(ofs >> i, file_oc);
  }
}

static void store_block_fields(mlsize_t sz, value v, char *base, FILE *file_oc)
{

  int long_values = 0;
  mlsize_t i;

  for (i = 0; i < sz; i++) {
#define store_long(a) { putc(CODE_INT + (a) - 1, file_oc); (a) = 0; }
    value f = Field (v, i);
#ifndef NATIVE_CODE
    if ((char *) f >= caml_code_area_start && (char *) f < caml_code_area_end) {
      if
#ifdef THREADED_CODE
        (*(code_t)f == (opcode_t)(caml_instr_table[RESTART]-caml_instr_base))
#else
        (*(code_t)f == RESTART)
#endif
      {
        if (long_values) store_long(long_values);
        putc(CODE_RESTART, file_oc);
      } else {
        if (long_values) store_long(long_values);
        putc(CODE_FUNCTION, file_oc);
      }
    } else
#endif
    {
      if (Is_long(f)) {
        long_values++;
        if (long_values == CODE_MAXINT) store_long(long_values);
      } else {
        if (long_values) store_long(long_values);
        if (Is_in_value_area(f)) {
          if (Is_in_heap_or_young(f)) {
            if (Tag_val(f) == Infix_tag) f -= Infix_offset_val(f);
#ifdef ARCH_SIXTYFOUR
            store_offset( ((intnat)Hp_val(f) - (intnat)base) >> 3, file_oc);
#else
            store_offset((intnat)(Hp_val(f) - base) >> 2, file_oc);
#endif
          } else {
            putc(CODE_STATIC, file_oc);
          }
        } else {
          putc(CODE_NOT_A_VALUE, file_oc);
        }
      }
    }
  }
  if (long_values) store_long(long_values);

#undef store_long

}

#if HAS_WEAK_POINTERS

/* WEAK section */
static void store_weaks(FILE *file_oc)
{
  value *pp = &caml_weak_list_head;
  value p;
  value q;
  size_t sz, i;
  size_t live;

  while (1){
    p = *pp; /* pointer to the weak block */
    store_word(p, file_oc);  /* 1. store the pointer to the weak block, NULL if end */
    if (p == (value) NULL) break;
    q = Hd_val (p);
    sz = Wosize_hd (q);
    ocp_heapdump_store_uint(sz, file_oc); /* 2. store the size of the block */
    live = 0;
    for (i = 1; i < sz; i++){
      value v = Field (p,i);
      if (v != caml_weak_none && Is_block(v) && Is_in_heap(v)){
        live++;
      }
    }
    ocp_heapdump_store_uint( live, file_oc ); /* 3. store the number of live pointers */
    for (i = 1; i < sz; i++){
      value v = Field (p,i);
      if (v != caml_weak_none && Is_block(v) && Is_in_heap(v)){
        store_word( v, file_oc ); /* 4+. store the live pointers */
      }
    }
    pp = &Field (p, 0);
  }
}

#else

/* Not implemented yet for trunk */

/* WEAK section */
static void store_weaks(FILE *file_oc)
{
  store_word(0, file_oc);
}

#endif

/* STRS section */
static void store_strings(FILE *file_oc)
{
  store_word(STR_SIZE, file_oc);
  if( str_id > 0 ){
    fwrite(str_tbl, STR_SIZE, str_id, file_oc);
  }
}

static char* str_tbl_slot()
{
  int i = str_id;
  if(str_tbl == NULL){
    str_tbl = (char*)caml_stat_alloc(STR_SIZE * MAX_STR);
  }
  str_id++;
  return str_tbl + i * STR_SIZE;
}

static void store_block(char *hp, FILE* file_oc)
{

  int i;
  value v = Val_hp(hp);
  header_t hd = Hd_val(v);
  tag_t tag = Tag_hd(hd);
  mlsize_t sz = Wosize_hd(hd);
  uintnat locid = Profinfo_hd(hd);

  block_count ++;

  /* Store the header. First bit is 0 (1 is freeblock). */

  if (tag < 16 && sz < (1 << (25 - locid_size))) {
    // 010 . size (25-locid_size) . locid(locid_size) . tag (4bits)
    uint32_t hd = (0x40 << 24) | (sz << (locid_size + 4)) | (locid << 4) | tag;
    COUNT_SMALL_BLOCK();
    for (i = 24; i >= 0; i -= 8) putc(hd >> i, file_oc);
  } else if (sz < (1 << (29 - locid_size))) {
    // 011 . size (29-locid_size) . locid(locid_size) . tag (8bits)
    uint64_t opcode = 0x60;
    uint64_t hd = (opcode << 32) | (sz << (locid_size + 8)) | (locid << 8) | tag;
    COUNT_MEDIUM_BLOCK();
    for (i = 32; i >= 0; i -= 8) putc(hd >> i, file_oc);
  } else {
    // 00 . size (32bits) . locid(22bits) . tag (8bits)
    uint64_t sz64 = sz;
    uint64_t hd = (sz64 << 30) | (locid << 8) | tag;
    COUNT_HUGE_BLOCK();
    for (i = 56; i >= 0; i -= 8) putc(hd >> i, file_oc);
  }

  /* if tag < No_scan_tag only, the contents of the block */

  if(ocp_heapdump_has_pointer_graph){
    if (tag < No_scan_tag) {
      store_block_fields(sz, v, hp, file_oc);
    } else if (tag == String_tag && str_id < MAX_STR) {
      int len = caml_string_length(v);
      int i;
      char *p = str_tbl_slot();
      char *s = String_val(v);
      for(i=0; i < STR_SIZE; i++){ 
        if( i < len ) { p[i] = s[i]; } else { p[i] = 0; }
      }
    }
  }
}

static void store_freeblock(char *hp, FILE* file_oc)
{
  asize_t sz = Wosize_hp(hp);

  /* First bit is 1 (0 is block header). */
  if (sz < 127) {
    COUNT_SMALL_FREEBLOCK();
    putc( 0x80 | sz, file_oc);
  } else {
    COUNT_HUGE_FREEBLOCK();
    putc( 0xff, file_oc);
    store_word(sz, file_oc);
  }
}

/* BLKS section */
static void store_blocks(FILE *file_oc)
{

  char *chunk;

  // Dump the chunks content: blocks and freeblocks.
  for (chunk = caml_heap_start; chunk != NULL; chunk = Chunk_next(chunk)) {

    char *hp;

    for (hp = chunk; hp < chunk + Chunk_size(chunk);
         hp = hp + Bhsize_hp(hp)) {

      switch (Color_hp(hp)) {
      case Caml_white:
        if ((Wosize_hp(hp) == 0) || (caml_gc_phase == Phase_sweep && hp >= caml_gc_sweep_hp)) {
          store_freeblock(hp, file_oc);
        } else {
          store_block(hp, file_oc);
        }
        break;
      case Caml_gray: case Caml_black:
        store_block(hp, file_oc);
        break;
      case Caml_blue:
        store_freeblock(hp, file_oc);
        break;
      }

    }

  }

}

/* Section */

#define MAX_SECTION 16

static int section_count;
static char section_names[MAX_SECTION][4];
static long section_starts[MAX_SECTION];

void init_section_index(FILE* file_oc) {
  fwrite(Heapdump_magic_number, Heapdump_magic_number_len - 1, 1, file_oc);
  section_count = 0;
}

void start_section(char name[4], FILE* file_oc) {
  int i = section_count++;
  if(i >= MAX_SECTION){
    caml_failwith("Heapdump: too many section!");
  }
  memcpy(section_names[i], name, sizeof(section_names[i]));
  section_starts[i] = ftell(file_oc);
}

void store_section_index(FILE *file_oc) {
  int i;
  for (i = 0; i < section_count; i++) {
    fwrite(section_names[i], sizeof(section_names[i]), 1, file_oc);
    store_word(section_starts[i], file_oc);
  }
  store_word(section_count, file_oc);
}



static char *heapdump_basename(char *s)
{
  char *basename = s;
  while( *s != 0 ){
    if( *s == '/' || *s == '\\' || *s == ':' ){
      basename = s+1;
    }
    s++;
  }
  return basename;
}


int ocp_heapdump_is_applicable(char *exe_name) {
  char *opt = getenv (HEAPDUMP_FILTER_VAR);
  char *exe_basename = heapdump_basename(exe_name);
  if (!opt) return 1;
  if (strlen(opt) == 0) return 1;

  while (*opt != '\0') {
    if (strncmp(opt, "pid=", 4) == 0) {
      int expected_pid;
      opt += 4;
      if (sscanf(opt, "%d", &expected_pid) != 1)
        fprintf(stderr,
		  "Can't parse the value of 'pid' in %s.\n",
		  HEAPDUMP_FILTER_VAR);
      if (expected_pid == HEAPDUMP_GETPID()) return 1;
    } else if (strncmp(opt, "ppid=", 5) == 0) {
      int expected_ppid;
      opt += 5;
      if (sscanf(opt, "%d", &expected_ppid) != 1)
        fprintf(stderr,
		  "Can't parse the value of 'ppid' in %s.\n",
		  HEAPDUMP_FILTER_VAR);
      if (expected_ppid == HEAPDUMP_GETPPID ()) return 1;
    } else if (strncmp(opt, "exe_name=", 9) == 0) {
      char expected_name[51];
      opt += 9;

      if (sscanf(opt, "%50s,", expected_name) == 1
          || sscanf(opt, "%50s", expected_name) == 1) {
        if (strcmp(expected_name, exe_name) == 0 ||
            strcmp(expected_name, exe_basename) == 0)  {
          return 1;
        }
      } else {
        fprintf(stderr,
		  "Can't parse the value of 'exe_name' in %s.\n",
		  HEAPDUMP_FILTER_VAR);
      }
    } else {
      opt++;
    }
  }
  return 0;
}


static FILE *file_oc = NULL;

static void dump_store_root(value v, value *useless)
{
  if(Is_block(v) && Is_in_heap(v)) {
    if(Tag_val(v) == Infix_tag) {
      store_word(v - Infix_offset_val(v), file_oc);
    } else {
      store_word(v, file_oc);
    }
    root_count++;
  }
}

static void dump_store_pointer_root(value v, value *fp)
{
  if(Is_block(v) && Is_in_heap(v)) {
    store_word((value)fp, file_oc);
    if(Tag_val(v) == Infix_tag) {
      store_word(v - Infix_offset_val(v), file_oc);
    } else {
      store_word(v, file_oc);
    }
    root_count++;
  }
}

/* NAME section */
static void dump_store_named(value *fp, char* name)
{
  int len = 0;
  if(name != NULL) {
    len = strlen(name);
    ocp_heapdump_store_uint(1+len, file_oc); /* > 0 */
    if( len > 0 ){
      fwrite(name, 1, len, file_oc);
    }
    store_word((value)fp, file_oc);
  }
}

#ifdef NATIVE_CODE
static void dump_store_dynglobal_root(char *name, value v)
{
  /* if(Is_block(v) && Is_in_heap(v)) */ { 
    store_word(v, file_oc);
    root_count++;
  }
}
#endif

static int final_set;
static int final_counter[5];
static void final_count(value f, value* fp){
  if( fp == NULL ){
    store_uint( final_counter[final_set], file_oc );
    final_set++;
    final_counter[final_set] = 0;
  } else {
    final_counter[final_set]++;
  }
}

static void store_final(FILE* file_oc)
{
  final_set = 0;
  caml_final_do( final_count );
  caml_final_do( dump_store_root );
}

void store_string(char*s, FILE* file_oc){
  int len = strlen(s);
  store_uint(len, file_oc);
  fwrite(s, len, 1, file_oc);
}

/* Main function */

void ocp_heapdump_do_dump(char *dumpname, value kind)
{
  CAMLparam0();
  CAMLlocal4(data, start_time, end_time, gc_stat);
  char *buf;
  char filename[FILENAME_MAX];
  value md5;
  struct channel * md5_chan;
  
  intnat heap_info_size;
#if (__OCP_OCAML || DEBUG)
  int i;
#endif

#ifdef DEBUG
  int file_end;
#endif

  if (heapdump_inited < 0) goto exit_do_dump; /* error */
  if (!heapdump_destdir_created) {
    if (!heapdump_mkdir(heapdump_destdir)) {
      heapdump_inited = -1;
      goto exit_do_dump;
    }
    heapdump_destdir_created = 1;
  }

#ifdef DEBUG
  small_block_count = 0;
  medium_block_count = 0;
  huge_block_count = 0;
  freeblock_count = 0;
  small_freeblock_count = 0;
  huge_freeblock_count = 0;
  for (i = 0; i < 8; i++) offset_count[i] = 0;
  offset_total = 0;
#endif

  block_count = 0;
  str_id = 0;

  locid_size = 0;
#ifdef __OCP_OCAML
  for (i = caml_memprof_next_location_id - 1; i > 0; i >>= 1) locid_size++;
#endif

  start_time = caml_sys_time(Val_unit);

  /* Open the dump file */
  if (strcmp(dumpname, "") != 0) {
      sprintf(filename, "%s/memprof.%d.%d.%s.dump",
              heapdump_destdir, HEAPDUMP_GETPID(),
              heap_number, dumpname);
  } else {
      sprintf(filename, "%s/memprof.%d.%d.dump",
              heapdump_destdir, HEAPDUMP_GETPID(), heap_number);
  }
  file_oc = fopen(filename, "w+b");
  if (file_oc == 0) {
    perror("fopen");
    fprintf(stderr, "Error while opening \"%s\"\n", filename);
    fflush(stderr);
    exit(EXIT_FAILURE);
  }

  /* Write header (magic, wordsize(s), native/bytecode...) */
  init_section_index(file_oc);
  putc(8 * sizeof(value), file_oc);
  putc(locid_size, file_oc);
#ifdef NATIVE_CODE
  putc(1, file_oc);
#else
  putc(0, file_oc);
#endif
  putc(ocp_format_version, file_oc);
  if( ocp_format_version > 0){
    store_string(dumpname, file_oc);
  }

  /* Dump the contents of allocated blocks. */
  start_section("BLKS", file_oc);
  store_blocks(file_oc);

  /* Dump a table of max 65535 pointers to strings in heap.
     What's the purpose of this ? */
  start_section("STRS", file_oc);
  store_strings(file_oc);

  /* Dump information on the the heap:
     - the number of blocks
     - the address and size of each chunk
  */
  start_section("CHKS", file_oc);
  store_chunk_table(file_oc); // Should be after 'store_blocks'

  /* List the GC roots (globals, stack, ...) */
  threads = NULL;
  start_section("ROOT", file_oc);
  caml_heapdump_in_dump = 1; /* to trigger caml_memprof_next_thread */
  store_roots(file_oc);
  caml_heapdump_in_dump = 0;

  /* Store weak pointers */
  start_section("WEAK", file_oc);
  store_weaks(file_oc);
  
  /* Store finalise information */
  start_section("FINA", file_oc);
  store_final(file_oc);

  /* Dump the statistics (should be after 'store chunks' in order not
     to dump the following allocations twice...) */
  start_section("INFO", file_oc);
  gc_stat = caml_gc_stat (Val_unit); // Should be called first
  end_time = caml_sys_time(Val_unit);
  data = caml_alloc_tuple(6);
  Field(data, 0) = Val_long(HEAPDUMP_GETPID());
  Field(data, 1) = Val_long(heap_number++);
  Field(data, 2) = kind;
  Field(data, 3) = gc_stat;
  Field(data, 4) = start_time;
  Field(data, 5) = end_time;
  caml_output_value_to_malloc(data, Val_int(0), &buf, &heap_info_size);
  fwrite(buf, heap_info_size, 1, file_oc);

  fflush(file_oc);

#ifdef DEBUG
  file_end = ftell(file_oc);
  fprintf(stderr, "### %s (%.2fMB) \n", filename, file_end / 1024. / 1024.);
  fprintf(stderr, "Locid_size: %d bits\n", locid_size);
  fprintf(stderr, "Block: %d (Header sizes: %2.1f/%2.1f/%2.1f)\n",
          block_count,
          small_block_count * 100. / block_count,
          medium_block_count * 100. / block_count,
          huge_block_count * 100. / block_count);
  fprintf(stderr, "Freeblock: %d (Sizes: %2.1f/%2.1f))\n",
          freeblock_count,
          small_freeblock_count * 100. / freeblock_count,
          huge_freeblock_count * 100. / freeblock_count);
  fprintf(stderr, "Offset: %d (Sizes: %2.1f/%2.1f/%2.1f/%2.1f/%2.1f/%2.1f/%2.1f/%2.1f)\n", offset_total,
          offset_count[0] * 100. / offset_total,
          offset_count[1] * 100. / offset_total,
          offset_count[2] * 100. / offset_total,
          offset_count[3] * 100. / offset_total,
          offset_count[4] * 100. / offset_total,
          offset_count[5] * 100. / offset_total,
          offset_count[6] * 100. / offset_total,
          offset_count[7] * 100. / offset_total);
  fflush(stderr);
#endif


  /* And we're done for the dump! */

  store_section_index(file_oc);

  /* We are done ! */
  fflush(file_oc); /* before computing the md5 */

  /* Computing md5 of dumps */
  fseek(file_oc, 0, SEEK_SET);
  md5_chan = caml_open_descriptor_in(fileno(file_oc));
  md5 = caml_md5_channel(md5_chan, Long_val(-1));
  caml_close_channel(md5_chan);
  
  fclose(file_oc);
  file_oc = NULL;

  ocp_heapdump_pi_do_dump(filename, md5);

 exit_do_dump:
  CAMLreturn0;
}
