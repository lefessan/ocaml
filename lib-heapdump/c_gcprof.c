/**************************************************************************/
/*                                                                        */
/*   Copyright 2014-2017 OCamlPro SAS --- typerex-memprof                 */
/*                                                                        */
/*   Do not redistribute, without permission of OCamlPro, source          */
/*   or binary copies of this software.                                   */
/*                                                                        */
/**************************************************************************/

/* gcprof:
   All this machinery is triggered by setting:
   * OCP_GCPROF_FILE=filename.gcprof
   * OCP_GCPROF_FLAGS, where flags are:
     - 'm': trace minor GC too
     - 'l': log all events
     - 'c': counters (a file is generated only if a watermark is done)
     - 'f': counters, saved at end of program
     - 'g': counters, saved after every GC
 */

/* TODO:
 * Some values during a compaction appear to change size: they have
    a freed count of 0, but a negative freed volume.
 * How to find which values are allocated in minor heap with locid = 0 ?

 * Update OPCODE_COUNTERS to contain the event that triggered the 
   dump (end_of_execution, signal, etc.)
 */
/* 
#define GCPROF_OPCODE_EXIT        0
  Always written at the end of the file when ocp_gcprof_exit() is called,
  in both LOGGING and COUNTERS mode.

#define GCPROF_OPCODE_FLUSH       1
  In LOGGING mode, save the current state of modified counters. The tables 
  contain the new updates since the previous GCPROF_OPCODE_FLUSH.

#define GCPROF_OPCODE_GC_PHASE    2
  In LOGGING mode, a change in the GC phase or sub-phase.

#define GCPROF_OPCODE_LOCATIONS   3
  In LOGGING and COUNTERS modes, the location tables are saved before
  GCPROF_OPCODE_FLUSH and GCPROF_OPCODE_COUNTERS.

#define GCPROF_OPCODE_WATERMARK   4
  In both LOGGING and COUNTERS modes, flush the tables and counters, and
  put the name of the mark.

#define GCPROF_OPCODE_COUNTERS    5
  In COUNTERS mode, save the tables.

#define GCPROF_OPCODE_THREAD      6
  In LOGGING and COUNTERS mode, a thread has been changed (created, scheduled,
    yielded, stopped, registered, unregistered)
*/

#define MEMPROF_INSIDE

#ifdef _WIN32

#define GCPROF_GETPID _getpid

#else

#define GCPROF_GETPID getpid

#endif

#include "caml/internals/mlvalues.h"

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
#include "caml/internals/alloc.h"
#include "caml/internals/intext.h"
#include "caml/internals/memory.h"
#include "caml/internals/memprof.h"
#include "caml/internals/sys.h"
#include "caml/internals/fix_code.h"

#include <stdio.h>
#include <errno.h>
#include "c_gcprof.h"

#ifndef PATH_MAX
#ifdef MAXPATHLEN
#define PATH_MAX MAXPATHLEN
#else
#define PATH_MAX 512
#endif
#endif

extern char ** caml_main_argv;

/********************************************************************/
/*                                                                  */
/*                     Continuous Profiling                         */
/*                                                                  */
/********************************************************************/

#ifndef DISABLE_GCPROF

/* Warning: check in mlvalues.h that this tag is not already used ! */
#define TAG_NOT_CONSTANT    245

#define GCPROF_OPCODE_EXIT        0
#define GCPROF_OPCODE_FLUSH       1
#define GCPROF_OPCODE_GC_PHASE    2
#define GCPROF_OPCODE_LOCATIONS   3
#define GCPROF_OPCODE_WATERMARK   4
#define GCPROF_OPCODE_COUNTERS    5
#define GCPROF_OPCODE_THREAD      6

#define GCPROF_MAGIC_NUMBER "OCP-2014G001"
#define GCPROF_MAGIC_NUMBER_LEN 12

#define GCPROF_INITIAL_MAX_LOCID      (1 << 18)


typedef unsigned short gcprof_count_type;
typedef uint64 gcprof_volume_type;

static gcprof_count_type  gcprof_minor_count = 0;
static FILE*              gcprof_output = NULL;
static unsigned int       gcprof_next_location_table_to_save = 0;


/********************************************************************/
/*                                                                  */
/*                     Continuous Logging                           */
/*                                                                  */
/********************************************************************/
#define GCPROF_LOGGING
#define GCPROF_COUNTERS


#ifdef GCPROF_LOGGING
static void gcprof_log_header(int locid, int tag, int sz, int kind);
static void gcprof_log_init();
static void gcprof_log_exit();
static void gcprof_log_watermark(char* name,int);
static void gcprof_log_gc_phase(int gc_phase, int gc_subphase);
#endif

#ifdef GCPROF_COUNTERS
static void gcprof_cnt_header(int locid, int tag, int sz, int kind);
static void gcprof_cnt_init();
static void gcprof_cnt_exit();
static void gcprof_cnt_watermark(char* name,int);
static void gcprof_cnt_gc_phase(int gc_phase, int gc_subphase);
#endif


/* intern.c allocates a big block and splits it in smaller pieces.
  So, we skip the first allocation, and then wait for the end to
  work through the new blocks. */
static int gcprof_intern_skip_next = 0;
static int gcprof_intern_wosize = 0;
static header_t* gcprof_intern_hp = NULL;
static int gcprof_intern_kind = 0; /* MINOR_ALLOC, MAJOR_ALLOC ? */



static char* gcprof_filename = NULL;

static void gcprof_store_uint(gcprof_volume_type c)
{
  gcprof_volume_type m = c >> 7;
  while( m != 0 ){
    putc( c & 0x7f, gcprof_output);
    c = m;
    m = c >> 7;
  }
  putc( c|0x80 , gcprof_output);
}

static void gcprof_store_string(char *name)
{
  int name_sz = strlen(name);
  int i;

  if(name_sz > 255) name_sz = 255;
  fputc(name_sz, gcprof_output);
  for(i=0; i<name_sz; i++) fputc(name[i], gcprof_output);
}

static void gcprof_realloc_volume_table(gcprof_volume_type **table_ptr,
			      int old_size,
			      int new_size)
{
  gcprof_volume_type *tmp = (gcprof_volume_type *)
    calloc(new_size, sizeof(gcprof_volume_type));
  if( *table_ptr != NULL ){
    memcpy(tmp, *table_ptr, old_size * sizeof(gcprof_volume_type));
    if( *table_ptr != NULL ) free(*table_ptr);
  }
  *table_ptr = tmp;
}


static void gcprof_open_file()
{
  int argn = 0;
  int pid = GCPROF_GETPID();
  char *path = caml_stat_alloc(PATH_MAX);
  int64 seed;
  sprintf(path, "%s-%d.gcprof", gcprof_filename, pid);
  gcprof_output = fopen(path, "w");
  
  fwrite(GCPROF_MAGIC_NUMBER, GCPROF_MAGIC_NUMBER_LEN, 1, gcprof_output);
  fputc(1, gcprof_output);  /* Current Version */
  fputc(caml_gcprof_flag, gcprof_output);   /* kind ? */
  fputc(1, gcprof_output);  /* Compressed ? */
  fputc(caml_gcprof_flag & GCPROF_MODE_MINOR_GC ? 1 : 0, gcprof_output);
  
  gcprof_store_uint(pid);
  gcprof_store_string(caml_exe_name);
  while(caml_main_argv[argn] != NULL) argn++;
  gcprof_store_uint(argn); 
  argn=0; while(caml_main_argv[argn] != NULL) {
    gcprof_store_string(caml_main_argv[argn]);
    argn++;
  }
  seed = ocp_memprof_seed();
  fwrite(&seed, 8, 1, gcprof_output);
}

static void gcprof_flush_location_tables()
{
  if( gcprof_next_location_table_to_save < caml_location_tables_next){
    for(;
	gcprof_next_location_table_to_save < caml_location_tables_next;
	gcprof_next_location_table_to_save++){
      //      fprintf(stderr, "GCPROF_OPCODE_LOCATIONS\n");
      fputc(GCPROF_OPCODE_LOCATIONS, gcprof_output);
      gcprof_store_uint
        (caml_location_tables_sizes[gcprof_next_location_table_to_save]);
      fwrite(caml_location_tables[gcprof_next_location_table_to_save], 1,
	     caml_location_tables_sizes[gcprof_next_location_table_to_save],
	     gcprof_output);
    }
  }
}

void ocp_gcprof_init()
{
  char *filename = getenv("OCP_GCPROF_FILE");

  if(filename == NULL)
    filename = getenv("OCAML_GCPROF");
  //  fprintf(stderr, "gcprof_init...\n");

  if( filename != NULL ){
    char *flags = getenv("OCP_GCPROF_FLAGS");
    //    fprintf(stderr, "gcprof_init INIIT...\n");

    gcprof_filename = filename;
    caml_gcprof_flag = GCPROF_MODE_ACTIVE;

    if(flags != NULL){
      while(*flags != 0){
        switch(*flags++){
        case 'm': caml_gcprof_flag |= GCPROF_MODE_MINOR_GC; break;
#ifdef GCPROF_LOGGING
        case 'l': caml_gcprof_flag |= GCPROF_MODE_LOGGING; break;
#endif
#ifdef GCPROF_COUNTERS
        case 'c': caml_gcprof_flag |= GCPROF_MODE_COUNTERS; break;
        case 'g': caml_gcprof_flag |= 
            GCPROF_MODE_COUNTERS | GCPROF_MODE_COUNTERS_GC; 
          break;
#endif
#ifdef NATIVE_CODE
        case 't': caml_gcprof_flag |= GCPROF_MODE_THREADS; break;
        case 'b': 
          caml_gcprof_flag |= GCPROF_MODE_BACKTRACES; 
          break;
#endif
        }
      }
    }

    gcprof_open_file();

#ifdef GCPROF_LOGGING
    gcprof_log_init();
#endif
#ifdef GCPROF_COUNTERS 
    gcprof_cnt_init();
#endif
  }
}

void ocp_gcprof_exit()
{

 if(gcprof_output != NULL){
   gcprof_flush_location_tables();
#ifdef GCPROF_LOGGING
   gcprof_log_exit();
#endif
#ifdef GCPROF_COUNTERS 
   gcprof_cnt_exit();
#endif

   fputc(GCPROF_OPCODE_EXIT, gcprof_output);
   fclose(gcprof_output);
   gcprof_output = NULL;
 }
}

void ocp_gcprof_header(value hd, int kind)
{
  profiling_t locid = Locid_hd(hd);
  mlsize_t sz =  Wosize_hd(hd);
  int tag =  Tag_hd(hd);

  if( locid == 0 && kind == GCPROF_MAJOR_ALLOC && !caml_in_minor_collection) 
    ocp_memprof_gdb();

  if( gcprof_intern_skip_next ) {
    gcprof_intern_skip_next = 0;
    return;
  }

  /* TODO: we should handle this case, although it should never happen */
  if(sz == 0) return;


#ifdef GCPROF_LOGGING
     if(caml_gcprof_flag & GCPROF_MODE_LOGGING ) 
       gcprof_log_header(locid, tag, sz, kind);
#endif
#ifdef GCPROF_COUNTERS 
     if(caml_gcprof_flag & GCPROF_MODE_COUNTERS)
       gcprof_cnt_header(locid, tag, sz, kind);
#endif

}

void ocp_gcprof_gc_phase(int gc_phase, int gc_subphase)
{
#ifdef GCPROF_LOGGING
  if( caml_gcprof_flag & GCPROF_MODE_LOGGING) 
    gcprof_log_gc_phase(gc_phase, gc_subphase);
#endif
#ifdef GCPROF_COUNTERS
  if( caml_gcprof_flag & GCPROF_MODE_COUNTERS)
    gcprof_cnt_gc_phase(gc_phase, gc_subphase);
#endif
}

void ocp_gcprof_major_scan(int kind)
{
   char *ch, *chend;

   if( caml_gcprof_flag & (GCPROF_MODE_LOGGING|GCPROF_MODE_COUNTERS) ){
     ch = caml_heap_start;
     while (ch != NULL){
       header_t *p = (header_t *) ch;
       
       chend = ch + Chunk_size (ch);
       while ((char *) p < chend){
         header_t hd = Hd_hp (p);
         mlsize_t sz = Wosize_hd (hd);
         if ( ! Is_blue_hd (hd)){
           ocp_gcprof_header( hd, kind );
         }
         p += Whsize_wosize (sz);
       }
       ch = Chunk_next (ch);
     }
   }
}

void ocp_gcprof_intern_init(mlsize_t wosize)
{
  gcprof_intern_hp = NULL;
  gcprof_intern_skip_next = 1;
  gcprof_intern_wosize = wosize;
}
void ocp_gcprof_intern_alloc(header_t* hp, int kind)
{
  gcprof_intern_hp = hp;
  gcprof_intern_kind = kind;
  gcprof_intern_skip_next = 0;
}
void ocp_gcprof_intern_finish()
{
  if( gcprof_intern_hp != NULL ){
    header_t *end_hp = gcprof_intern_hp + gcprof_intern_wosize;
    while( gcprof_intern_hp < end_hp ){
      header_t hd = Hd_hp( gcprof_intern_hp );
      mlsize_t wz = Wosize_hd(hd);
      ocp_gcprof_header( hd, gcprof_intern_kind );
      gcprof_intern_hp += wz + 1;
    }

  }
  gcprof_intern_hp = NULL;
}

void ocp_gcprof_minor_prepare()
{
  if( caml_gcprof_flag & GCPROF_MODE_MINOR_GC ){
    memset(caml_young_start, 0, caml_young_end - caml_young_start);
  }
}

void ocp_gcprof_minor_scan()
{
  if( caml_gcprof_flag & GCPROF_MODE_MINOR_GC ){
    value *start_hp;

    gcprof_minor_count++;
    ocp_gcprof_gc_phase(Phase_minor, Subphase_main);
    if( caml_young_ptr < caml_young_start ){
      start_hp = (value*)caml_young_start;
      while(start_hp < (value*)caml_young_end && *start_hp == 0) start_hp++;
    } else {
      start_hp = (value*)caml_young_ptr;
    }
    while( start_hp < (value*) caml_young_end ){
      header_t hd = Hd_hp(start_hp);
      mlsize_t sz = Wosize_hd(hd);

      ocp_gcprof_header(hd, GCPROF_MINOR_ALLOC);
      start_hp += sz + 1;
    }
  }
}

#define GCPROF_WATERMARK_EVENT           1
#define GCPROF_WATERMARK_LOGGING_BEGIN   2
#define GCPROF_WATERMARK_LOGGING_END     4
#define GCPROF_WATERMARK_COUNTERS_BEGIN  8
#define GCPROF_WATERMARK_COUNTERS_END   16

void ocp_gcprof_watermark(char *name, int watermark_kind)
{
  /* Should the watermark be used to flush counters for
     GCPROF_COUNTERS too ? */
#ifdef GCPROF_LOGGING
  if( caml_gcprof_flag & GCPROF_MODE_LOGGING )
    gcprof_log_watermark(name, watermark_kind); 
#endif
#ifdef GCPROF_LOGGING
  if( caml_gcprof_flag & GCPROF_MODE_COUNTERS )
    gcprof_cnt_watermark(name, watermark_kind); 
#endif
  if(gcprof_output != NULL){
    fputc(GCPROF_OPCODE_WATERMARK, gcprof_output);
    gcprof_store_uint(watermark_kind);
    gcprof_store_string(name);
  }
}





#ifdef GCPROF_LOGGING

#define GCPROF_MAX_INDEX_INCREMENT    (1 << 10)
#define GCPROF_MAX_MODIFIED           (65535-1024)
#define GCPROF_MAX_COUNT              65535
#define GCPROF_OVER_SMALLSIZE         256

static unsigned int gcprof_max_locid = 0;
static unsigned int gcprof_next_index = GCPROF_OVER_SMALLSIZE;
static unsigned int gcprof_max_index = GCPROF_OVER_SMALLSIZE;
static unsigned int gcprof_next_modified = 0;

static gcprof_count_type *gcprof_tag_of_locid = NULL;
static gcprof_count_type *gcprof_size_of_locid = NULL;
static gcprof_count_type *gcprof_modified_of_locid = NULL;
static gcprof_count_type *gcprof_count_of_locid[GCPROF_NTABLES];

static profiling_t   *gcprof_modified = NULL;
static uint64        *gcprof_volume_of_index[GCPROF_NTABLES];


static void gcprof_realloc_count_table(gcprof_count_type **table_ptr,
			      int old_size,
			      int new_size)
{
  gcprof_count_type *tmp = (gcprof_count_type *)
    calloc(new_size, sizeof(gcprof_count_type));
  if( *table_ptr != NULL ){
    memcpy(tmp, *table_ptr, old_size * sizeof(gcprof_count_type));
    if( *table_ptr != NULL ) free(*table_ptr);
  }
  *table_ptr = tmp;
}

static void gcprof_log_flush_tables()
{
  int i;

  if( gcprof_output == NULL) gcprof_open_file();
  gcprof_flush_location_tables();

  fputc(GCPROF_OPCODE_FLUSH, gcprof_output);
  gcprof_store_uint(gcprof_next_modified);

  //  fprintf(stderr, "NMODIFIED %d\n", gcprof_next_modified);
  for(i = 0; i < gcprof_next_modified; i++){
    int j;
    profiling_t locid = gcprof_modified[i];
    mlsize_t old_sz = gcprof_size_of_locid[locid];

    //    if(locid == 0) ocp_memprof_gdb();
    //    fprintf(stderr, "LOC %d\n", locid);
    gcprof_store_uint( locid );
    fputc(gcprof_tag_of_locid[locid], gcprof_output);
    for(j=0; j< GCPROF_NTABLES; j++){
      gcprof_store_uint(gcprof_count_of_locid[j][locid]);
      gcprof_count_of_locid[j][locid] = 0;
    }
    if(old_sz < GCPROF_OVER_SMALLSIZE){
      gcprof_store_uint(old_sz);
    } else {
      gcprof_store_uint(0);
      for(j=0; j< GCPROF_NTABLES; j++){
	gcprof_store_uint(gcprof_volume_of_index[j][old_sz-GCPROF_OVER_SMALLSIZE]);
      }
    }
    gcprof_size_of_locid[locid] = 0;
    gcprof_modified_of_locid[locid] = 0;
  }
  gcprof_next_modified = 0;
  gcprof_next_index = GCPROF_OVER_SMALLSIZE;
  fflush(gcprof_output);
}

static void gcprof_log_header(int locid, int tag, int sz, int kind){
  mlsize_t old_sz;
  gcprof_count_type* table;

  if(kind == GCPROF_COMPACT_ALLOC) {
    kind = GCPROF_MAJOR_ALLOC;
  } else
  if(kind == GCPROF_COMPACT_FREE) {
    kind = GCPROF_MAJOR_FREE;
  } 

  if(locid >= gcprof_max_locid){
    /* We need to increase the size of the locid table */
    int i;
    profiling_t next_max_locid = gcprof_max_locid;
    if( next_max_locid == 0 ) next_max_locid = GCPROF_INITIAL_MAX_LOCID;
    while( locid >= next_max_locid ) next_max_locid = next_max_locid << 1;

    for(i=0; i< GCPROF_NTABLES; i++){
      gcprof_realloc_count_table( & gcprof_count_of_locid[i],
			      gcprof_max_locid, next_max_locid);
    }
    gcprof_realloc_count_table( & gcprof_tag_of_locid,
			    gcprof_max_locid, next_max_locid);
    gcprof_realloc_count_table( & gcprof_size_of_locid,
			    gcprof_max_locid, next_max_locid);
    gcprof_realloc_count_table( & gcprof_modified_of_locid,
			    gcprof_max_locid, next_max_locid);
    gcprof_max_locid = next_max_locid;
  }

  table = gcprof_count_of_locid[kind];
  if( table[locid] == GCPROF_MAX_COUNT ||
      ( gcprof_modified_of_locid[locid] == 0 &&
	gcprof_next_modified == GCPROF_MAX_MODIFIED )
      ) gcprof_log_flush_tables();
  table[locid] += 1;
  if( gcprof_modified_of_locid[locid] == 0 ){
    gcprof_modified_of_locid[locid] = gcprof_next_modified+1;
    if( gcprof_next_modified == 0 ){
      gcprof_modified = (profiling_t*)
	calloc(GCPROF_MAX_MODIFIED, sizeof(profiling_t));
    }
    gcprof_modified[ gcprof_next_modified ] = locid;
    gcprof_next_modified++;

    gcprof_tag_of_locid[locid] = tag;
    gcprof_size_of_locid[locid] = 0;
  }
  if( gcprof_tag_of_locid[locid] != tag) 
    gcprof_tag_of_locid[locid] = TAG_NOT_CONSTANT;

  old_sz = gcprof_size_of_locid[locid];
  if( sz < GCPROF_OVER_SMALLSIZE ){
    if ( old_sz == sz ) return;
    if ( old_sz == 0 ){
      gcprof_size_of_locid[locid] = sz;
      return;
    }
  }

  /* We must also record the volume */
  sz++; /* size must take header into account */
  if( old_sz < GCPROF_OVER_SMALLSIZE ){ /* we need a new index */
    int i;

    sz += (1 + old_sz) * (table[locid]-1);

    old_sz = gcprof_next_index;
    if( gcprof_next_index == gcprof_max_index ){
      int next_max_index = gcprof_max_index + GCPROF_MAX_INDEX_INCREMENT;
      int i;

      for(i=0; i< GCPROF_NTABLES; i++){
	gcprof_realloc_volume_table( &gcprof_volume_of_index[i],
				 gcprof_max_index - GCPROF_OVER_SMALLSIZE,
				 next_max_index - GCPROF_OVER_SMALLSIZE);
      }
      gcprof_max_index = next_max_index;
    }
    gcprof_next_index++;
    gcprof_size_of_locid[locid] = old_sz;
    for(i=0; i< GCPROF_NTABLES; i++){
      gcprof_volume_of_index[i][old_sz - GCPROF_OVER_SMALLSIZE] = 0;
    }
    gcprof_volume_of_index[kind][old_sz - GCPROF_OVER_SMALLSIZE] = sz;
  } else {
    gcprof_volume_of_index[kind][old_sz - GCPROF_OVER_SMALLSIZE] += sz;
  }
}

static void gcprof_log_gc_phase(int gc_phase, int gc_subphase){
  if( gcprof_output != NULL ){
    gcprof_log_flush_tables();
    //    fprintf(stderr, "GCPROF_OPCODE_GC_PHASE\n");
    fputc(GCPROF_OPCODE_GC_PHASE, gcprof_output);
    fputc(gc_phase, gcprof_output);
    fputc(gc_subphase, gcprof_output);
    gcprof_store_uint( gcprof_minor_count );
  }
}

static void gcprof_log_init(){
  int i;

  for(i=0; i< GCPROF_NTABLES; i++){
    gcprof_count_of_locid[i] = NULL;
    gcprof_volume_of_index[i] = NULL;
  }
}

static void gcprof_log_watermark(char* name, int watermark_kind){
  if( gcprof_output != NULL){
    gcprof_log_flush_tables();
  }
}

static void gcprof_log_exit(){

  if( gcprof_output != NULL ){
    gcprof_log_flush_tables();
  }
}

#endif // GCPROF_LOGGING

/* if LOGGING and COUNTERS are two contradictory modes of profiling,
   we could probably share the same data structures, no ? */

#ifdef GCPROF_COUNTERS
static int gcprof_cnt_last_locid = -1;
static unsigned int gcprof_cnt_max_locid = 0;
static gcprof_volume_type *gcprof_cnt_count_of_locid[GCPROF_NTABLES];
static gcprof_volume_type *gcprof_cnt_volume_of_locid[GCPROF_NTABLES];

static void gcprof_cnt_header(int locid, int tag, int sz, int kind)
{
  if(locid > gcprof_cnt_last_locid){
    gcprof_cnt_last_locid = locid;
    if(locid >= gcprof_cnt_max_locid){
    /* We need to increase the size of the locid table */
    int i;
    profiling_t next_max_locid = gcprof_cnt_max_locid;
    if( next_max_locid == 0 ) next_max_locid = GCPROF_INITIAL_MAX_LOCID;
    while( locid >= next_max_locid ) next_max_locid = next_max_locid << 1;

    for(i=0; i< GCPROF_NTABLES; i++){
      gcprof_realloc_volume_table( & gcprof_cnt_count_of_locid[i],
                                   gcprof_cnt_max_locid, next_max_locid);
      gcprof_realloc_volume_table( & gcprof_cnt_volume_of_locid[i],
                                   gcprof_cnt_max_locid, next_max_locid);
    }
    gcprof_cnt_max_locid = next_max_locid;
  }
  }
  if(kind == GCPROF_COMPACT_ALLOC){
     /* Alloc during compaction means that the previous free should be
        removed, since the block is still alive. */
     kind = GCPROF_MAJOR_FREE;
     gcprof_cnt_count_of_locid[kind][locid]--;
     gcprof_cnt_volume_of_locid[kind][locid]-= (sz+1);
  } else {
     if(kind == GCPROF_COMPACT_FREE) kind = GCPROF_MAJOR_FREE;
     gcprof_cnt_count_of_locid[kind][locid] ++;
     gcprof_cnt_volume_of_locid[kind][locid] += (sz+1);
  } 
}

static void gcprof_cnt_init()
{
  int i;

  for(i=0; i< GCPROF_NTABLES; i++){
    gcprof_cnt_count_of_locid[i] = NULL;
    gcprof_cnt_volume_of_locid[i] = NULL;
  }
}

static void gcprof_cnt_save_table()
{
  int locid,kind;
  
  if( gcprof_output == NULL) gcprof_open_file();
  gcprof_flush_location_tables();
  fputc(GCPROF_OPCODE_COUNTERS, gcprof_output);
  //  fprintf(stderr, "GCPROF_OPCODE_COUNTERS\n");
  //  fprintf(stderr, "gcprof_cnt_last_locid=%d\n", gcprof_cnt_last_locid);
  gcprof_store_uint(gcprof_cnt_last_locid);
  for(locid = 0; locid <= gcprof_cnt_last_locid; locid++){
    for(kind=0; kind< GCPROF_NTABLES; kind++)
        gcprof_store_uint(gcprof_cnt_count_of_locid[kind][locid]);
    for(kind=0; kind< GCPROF_NTABLES; kind++)
      gcprof_store_uint(gcprof_cnt_volume_of_locid[kind][locid]);
    
  } 
  fflush(gcprof_output);
}

static void gcprof_cnt_exit()
{
  if( caml_gcprof_flag | GCPROF_MODE_COUNTERS ){
    gcprof_cnt_save_table();
  }
}

static void gcprof_cnt_watermark(char* name, int watermark_kind)
{
  gcprof_cnt_save_table();
}

static void gcprof_cnt_gc_phase(int gc_phase, int gc_subphase)
{
  if( (caml_gcprof_flag & GCPROF_MODE_COUNTERS_GC) && 
     gc_phase == Phase_idle){
    //    fprintf(stderr, "GC finished\n");
    gcprof_cnt_save_table();
  }
}

#endif

void ocp_gcprof_gctime(int gcevent)
{

}

#ifdef NATIVE_CODE

#include "caml/internals/stack.h"
#define OCP_NEED_LOCINFO
#include "caml/internals/backtrace.h"

struct prof_backtrace {
  mlsize_t bt_size;
  mlsize_t bt_maxsize;
  frame_descr *bt_trace[1];
};

//static struct prof_backtrace* previous_backtrace = NULL;
//static struct prof_backtrace* current_backtrace = NULL;

static void gcprof_save_backtrace(mlsize_t sz, profiling_t locid, 
                                    int minor_alloc)
{
}

CAMLexport void ocp_gcprof_alloc_major(mlsize_t sz, tag_t tag,
                                       profiling_t locid)
{}
CAMLexport void ocp_gcprof_minor_collection()
{}
CAMLexport void ocp_gcprof_future_minor_alloc(char* young_ptr, 
                                              mlsize_t size_plus_header)
{}

#else

int ocp_gcprof_record_minor_alloc()
{ return 0; }

void ocp_gcprof_record_major_alloc(mlsize_t sz, profiling_t locid)
{}

#endif // NATIVE_CODE

void ocp_memprof_change_thread(value thread_id, int change)
{
  if( caml_gcprof_flag & GCPROF_MODE_THREADS ){
    if( caml_gcprof_flag & GCPROF_MODE_LOGGING ){
      gcprof_log_flush_tables();
    } else
    if( caml_gcprof_flag & GCPROF_MODE_COUNTERS ){
      gcprof_cnt_save_table();
    }
    if( gcprof_output != NULL ){
      fputc(GCPROF_OPCODE_THREAD, gcprof_output);
      gcprof_store_uint(thread_id);
      fputc(change, gcprof_output);
    }
  }
}


#else // defined(DISABLE_GCPROF)

#endif // DISABLE_GCPROF
