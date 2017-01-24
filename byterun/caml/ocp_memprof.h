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


#ifndef CAML_MEMPROF_H
#define CAML_MEMPROF_H

#define __OCP_OCAML 1

#include "mlvalues.h"

#ifdef __cplusplus
extern "C" {
#endif

/* Static locids used by the runtime */
#define PROF_DUMMY                 0
#define PROF_TODO                  1
#define PROF_MEMPROF               2
#define PROF_STARTUP               3
#define PROF_STARTUP_INPUT_VAL     4
#define PROF_INTERP                5
#define PROF_INIT                  6
#define PROF_EXCEPTION             7
#define PROF_LAST_EXTERNAL         8

#ifdef MEMPROF_INSIDE

CAMLextern int64 ocp_memprof_seed(void);
CAMLextern void ocp_memprof_scanmult (char *opt, uintnat *var);
CAMLextern void ocp_dump_after_gc(void);
CAMLextern void ocp_memprof_init();
CAMLextern void ocp_memprof_exit();

#else /* !MEMPROF_INSIDE */

CAMLextern void caml_memprof_scanmult (char *opt, uintnat *var);
CAMLextern void caml_dump_after_gc(void);

CAMLextern void caml_memprof_init();
CAMLextern void caml_memprof_exit();

#endif


#ifdef MEMPROF_INSIDE
 
CAMLextern void ocp_memprof_bytecode_init(char *memprof_section);
CAMLextern code_t ocp_memprof_bytecode_fix_locids(code_t data, 
                                                  asize_t* code_size);

#else /* !MEMPROF_SECTION */

CAMLextern void caml_memprof_bytecode_init(char *memprof_section);
CAMLextern code_t caml_memprof_bytecode_fix_locids(code_t data, 
                                                   asize_t* code_size);

#endif  /* MEMPROF_SECTION */

CAMLextern
  int caml_memprof_register_loc_table (char* table, mlsize_t len, int elems);

  
#ifdef __cplusplus
}
#endif

#endif // CAML_MEMPROF_H


















/* comment everything for now... */
#if 0

/* patch better epuration */
CAMLextern uintnat caml_grayvals_ratio;

#ifdef NATIVE_CODE

#else
CAMLextern int* caml_init_opcode_nargs(); /* fix_code.c */
#endif


CAMLextern value caml_gc_stat(value v); /* gc_ctrl.c */
CAMLextern void (*caml_major_gc_hook)(void); /* major_gc.c */
struct channel;
CAMLextern value caml_md5_channel(struct channel * chan, intnat len); /* md5.c */
CAMLextern value caml_sys_time (value); /* sys.c */
#ifndef CAML_ROOTS_H
typedef void (*dyn_scanning_action) (char *, value);
#endif
CAMLextern void caml_do_dynglobals (dyn_scanning_action); 
CAMLextern void caml_register_dynlink_global(char*, void *);


/* Can be used by applications to know whether they should use
   caml_make_header() */
#define HAS_OCP_MEMPROF
/* Can be used to implement Make_header() when HAS_OCP_MEMPROF is defined. */
CAMLextern value caml_make_header(mlsize_t wosize, tag_t tag, int color);

/* These definitions are only available when compiling OCaml. */
#ifdef CAML_NAME_SPACE

/* for signals.c */
#ifndef NSIG
#define NSIG 64
#endif

#ifdef _WIN32
#define CAML_NSIG     (NSIG)+1
#define SIGALRM       NSIG
#else
#define CAML_NSIG NSIG
#define CAML_FAKED_SIGNALS NSIG
#endif

/* for floats.c */
#ifdef _WIN32
#undef strtod
#endif

#endif

/* On Windows, gcc-mingw produces calls to __chkstk_ms for big buffers
allocated in the stack, producing code that cannot be linked with msvc
runtime. To avoid this problem, all buffers are malloced under
Windows. */
#ifdef _WIN32
#define OCP_DECLARE_BUFFER(buf,sz)  char *buf = (char*)caml_stat_alloc(sz)
#define OCP_FREE_BUFFER(buf) caml_stat_free(buf)
#else
#define OCP_DECLARE_BUFFER(buf,sz) char buf[sz]
#define OCP_FREE_BUFFER(buffer)
#endif

/********************************************************************/
/*                                                                  */
/*                     Snapshot Profiling                           */
/*                                                                  */
/********************************************************************/

CAMLextern int caml_memprof_next_location_id;

CAMLextern int caml_location_tables_next;
CAMLextern mlsize_t* caml_location_tables_sizes;
CAMLextern char** caml_location_tables;

/* [MEMPROF_THREAD_NEXT(thread_id,bottom_of_stack, retaddr)] is called
   when dumping roots, before scanning the thread roots. If thread_id=0
   (i.e. not an Int_val()), then it was the last thread. */

/* [MEMPROF_THREAD_CHANGE(thread_id, change)] is called when a thread
   operation was performed (creation, yield, schedule, stop). */

#define MEMPROF_THREAD_INIT       0
#define MEMPROF_THREAD_YIELD      1
#define MEMPROF_THREAD_SCHEDULE   2
#define MEMPROF_THREAD_STOP       3
#define MEMPROF_THREAD_REINIT     4
#define MEMPROF_THREAD_REGISTER   5
#define MEMPROF_THREAD_UNREGISTER 6
#define MEMPROF_THREAD_CREATE     7


/*
 [ MEMPROF_THREAD_INIT ]: corresponding thread is the first thread of the
    system. All preceeding allocations were done by it !
 [ MEMPROF_THREAD_YIELD ]: corresponding thread is not going to allocate
    anymore until scheduled again.
 [ MEMPROF_THREAD_SCHEDULE ]: corresponding thread is being scheduled.
    Further allocations are done by it !
 [ MEMPROF_THREAD_STOP ]: corresponding thread is being terminated. It will
    NEVER allocate again.
 [ MEMPROF_THREAD_REINIT ]: corresponding thread is the first thread after
    a fork.
 [ MEMPROF_THREAD_REGISTER ]: corresponding thread is declared as a C thread.
    It should be scheduled just after.
 [ MEMPROF_THREAD_UNREGISTER ]: corresponding thread is unregistered. It will
    not perform new allocations, unless it is registered again, but under a
    different identifier.
 [ MEMPROF_THREAD_CREATE ]: corresponding thread is being created. It is not
    scheduled yet, i.e. next allocation might be performed by another thread.
 */

#ifdef MEMPROF_INSIDE

CAMLextern void ocp_memprof_next_thread(value thread_id, char* bottom,
                                        uintnat retaddr);
CAMLextern void ocp_memprof_change_thread(value thread_id, int change);

#else

#define MEMPROF_THREAD_NEXT(thread_id,bsp,retaddr) \
             caml_memprof_next_thread(thread_id, bsp, retaddr)
#define MEMPROF_THREAD_CHANGE(thread_id,change) \
             caml_memprof_change_thread(thread_id, change)

CAMLextern void caml_memprof_next_thread(value thread_id, char* bottom,
                                         uintnat retaddr);
CAMLextern void caml_memprof_change_thread(value thread_id, int change);

#endif

#define GCPROF_GCTIME_MINOR_BEGIN    0
#define GCPROF_GCTIME_MINOR_END      1
#define GCPROF_GCTIME_MARK_BEGIN     2
#define GCPROF_GCTIME_MARK_END       3
#define GCPROF_GCTIME_SWEEP_BEGIN    4
#define GCPROF_GCTIME_SWEEP_END      5
#define GCPROF_GCTIME_COMPACT_BEGIN  6
#define GCPROF_GCTIME_COMPACT_END    7

#ifdef MEMPROF_INSIDE

CAMLextern void ocp_memprof_gdb();

/* Returns the first locid to be used by the module */
CAMLextern value ocp_memprof_register_table (value table, value elems,
                                             value ui_name);

CAMLextern void ocp_memprof_compact_begin();
CAMLextern void ocp_memprof_compact_move(void*, void*, size_t);
CAMLextern void ocp_memprof_compact_end();
CAMLextern void ocp_gcprof_gctime(int gcevent);

#else /* !MEMPROF_INSIDE */

/* Returns the first locid to be used by the module */
CAMLextern value caml_memprof_register_table (value table, value elems,
                                              value ui_name);

CAMLextern void caml_memprof_compact_begin();
CAMLextern void caml_memprof_compact_move(void*, void*, size_t);
CAMLextern void caml_memprof_compact_end();
CAMLextern void caml_gcprof_gctime(int gcevent);

#define GCPROF_GCTIME(event) \
  if(caml_gcprof_flag & GCPROF_MODE_GCTIME) caml_gcprof_gctime(event)

#define MEMPROF_COMPACT_BEGIN() caml_memprof_compact_begin()
#define MEMPROF_COMPACT_MOVE(p, newaddr, sz) \
                    caml_memprof_compact_move(p,newaddr, sz)
#define MEMPROF_COMPACT_END()     caml_memprof_compact_end()

#ifdef NATIVE_CODE

#define MEMPROF_BYTECODE_INIT(memprof_section)
#define MEMPROF_BYTECODE_FIX_LOCIDS(code, code_size)

#else /* !NATIVE_CODE */


#define MEMPROF_BYTECODE_INIT(memprof_section) \
  caml_memprof_bytecode_init(memprof_section)

#define MEMPROF_BYTECODE_FIX_LOCIDS(code, code_size) \
  code = caml_memprof_bytecode_fix_locids(code, &code_size)

#endif  /* NATIVE_CODE */

#endif  /* MEMPROF_INSIDE */


/********************************************************************/
/*                                                                  */
/*                     Continuous Profiling                         */
/*                                                                  */
/********************************************************************/

#if 0
/* This MACRO can be used to completely remove the GcProf patch */
#define DISABLE_GCPROF
#endif

#ifdef DISABLE_GCPROF

#define GCPROF_HEADER(hd,kind)
#define GCPROF_GC_PHASE(gc_phase, gc_subphase)
#define GCPROF_PREPARE_MINOR()
#define GCPROF_MINOR_SCAN()
#define GCPROF_MAJOR_SCAN(kind)
#define GCPROF_INTERN_INIT(wosize)
#define GCPROF_INTERN_ALLOC(hp, kind)
#define GCPROF_INTERN_FINISH()
#define GCPROF_WATERMARK(name,kind)

#else /* DISABLE_GCPROF */

#define GCPROF_MINOR_ALLOC     0
#define GCPROF_MINOR_PROMOTE   1
#define GCPROF_MAJOR_ALLOC     2
#define GCPROF_MAJOR_FREE      3
#define GCPROF_MAJOR_SCANNED   4
#define GCPROF_MAJOR_DARKEN    5
#define GCPROF_NTABLES         6
#define GCPROF_COMPACT_ALLOC   7
#define GCPROF_COMPACT_FREE    8

#define Phase_minor            4

/* We still have some work to do with compaction: we should scan all
blocks before compaction, mark them in MAJOR_FREE, and then scan
them again after compaction, marking them as MAJOR_ALLOC. */

#define Phase_compact          5
#define Subphase_retry        15

/* This variable is non-nul if GC profiling has been activated. */
extern int caml_gcprof_flag;

#define GCPROF_HEADER(hd,kind)                              \
  if ( caml_gcprof_flag )				    \
    caml_gcprof_header( hd, GCPROF_##kind);

#define GCPROF_GC_PHASE(gc_phase, gc_subphase)   	    \
  if ( caml_gcprof_flag )      			            \
    caml_gcprof_gc_phase(gc_phase,gc_subphase);

#define GCPROF_PREPARE_MINOR()                              \
  if ( caml_gcprof_flag )                                   \
    caml_gcprof_minor_prepare();

#define GCPROF_MINOR_SCAN()                                 \
  if ( caml_gcprof_flag )                                   \
    caml_gcprof_minor_scan();

#define GCPROF_MAJOR_SCAN(kind)                             \
  if ( caml_gcprof_flag )                                   \
    caml_gcprof_major_scan( GCPROF_##kind);

/* store the wosize of the block being created, and ask to
   skip recording the next allocation.  */
#define GCPROF_INTERN_INIT(wosize)                          \
  if ( caml_gcprof_flag )				    \
    caml_gcprof_intern_init(wosize)
/* store the block allocated for interning the values, and
   the kind of tables in which records should be stored. */
#define GCPROF_INTERN_ALLOC(hp, kind)                       \
  if ( caml_gcprof_flag )				    \
    caml_gcprof_intern_alloc(hp, GCPROF_##kind)
/* iter in the block on the values newly created, and
   call GCPROF_HEADER on each of them. */
#define GCPROF_INTERN_FINISH()                              \
  if ( caml_gcprof_flag )				    \
    caml_gcprof_intern_finish()

/* #define GCPROF_WATERMARK(s) caml_gcprof_watermark(s) */

#ifdef MEMPROF_INSIDE

CAMLextern void ocp_gcprof_header(value hp, int kind);
CAMLextern void ocp_gcprof_gc_phase(int gc_phase, int gc_subphase);
CAMLextern void ocp_gcprof_minor_prepare();
CAMLextern void ocp_gcprof_minor_scan();
CAMLextern void ocp_gcprof_major_scan(int kind);
CAMLextern void ocp_gcprof_init();
CAMLextern void ocp_gcprof_exit();

/* These macros are used in the specific case of intern.c, where
   allocations are performed inline. */
CAMLextern void ocp_gcprof_intern_init(mlsize_t wosize);
CAMLextern void ocp_gcprof_intern_alloc(header_t* hp, int kind);
CAMLextern void ocp_gcprof_intern_finish();
CAMLextern void ocp_gcprof_watermark(char*,int);


#else /* !MEMPROF_INSIDE */

CAMLextern void caml_gcprof_header(value hp, int kind);
CAMLextern void caml_gcprof_gc_phase(int gc_phase, int gc_subphase);
CAMLextern void caml_gcprof_minor_prepare();
CAMLextern void caml_gcprof_minor_scan();
CAMLextern void caml_gcprof_major_scan(int kind);
CAMLextern void caml_gcprof_init();
CAMLextern void caml_gcprof_exit();

/* These macros are used in the specific case of intern.c, where
   allocations are performed inline. */
CAMLextern void caml_gcprof_intern_init(mlsize_t wosize);
CAMLextern void caml_gcprof_intern_alloc(header_t* hp, int kind);
CAMLextern void caml_gcprof_intern_finish();
CAMLextern void caml_gcprof_watermark(char*,int);

#endif /* MEMPROF_INSIDE */


#endif /* DISABLE_GCPROF */

/********************************************************************/
/*                                                                  */
/*                     Allocation Profiling                         */
/*                                                                  */
/********************************************************************/

#define GCPROF_MODE_ACTIVE        1
#define GCPROF_MODE_LOGGING       2
#define GCPROF_MODE_COUNTERS      4
#define GCPROF_MODE_COUNTERS_GC   8
#define GCPROF_MODE_MINOR_GC     16
#define GCPROF_MODE_THREADS      32
#define GCPROF_MODE_BACKTRACES   64
#define GCPROF_MODE_GCTIME      128

#ifdef NATIVE_CODE

/* This variable is non-null if alloc-profiling is activated */
CAMLextern char *caml_allocprof_young_ptr;

/* in signals_asm.c:caml_garbage_collection()  */
#define ALLOCPROF_BEGIN_GARBAGE_COLLECTION()                          \
  int size_plus_header = 0;                                           \
  char *old_caml_young_ptr = caml_young_ptr;                          \
  if( caml_gcprof_flag &  GCPROF_MODE_BACKTRACES){                    \
    size_plus_header = caml_allocprof_young_ptr - caml_young_ptr;     \
  } else {                                                            \
    caml_young_limit = caml_young_start;                              \
  }

/* in signals_asm.c:caml_garbage_collection()  
   We need to set caml_young_limit so that it will not trigger
   a new call to caml_garbage_collection, unless some signals
   did allocate.
*/
#define ALLOCPROF_END_GARBAGE_COLLECTION()                              \
  if( (caml_gcprof_flag &  GCPROF_MODE_BACKTRACES) &&                   \
      caml_young_ptr == old_caml_young_ptr ){                           \
    caml_young_ptr = caml_allocprof_young_ptr;                          \
    caml_allocprof_young_ptr = caml_young_ptr - size_plus_header;       \
    if( caml_allocprof_young_ptr >= caml_young_start ){                 \
      caml_young_limit = caml_allocprof_young_ptr - 1;                  \
      caml_allocprof_future_minor_alloc(caml_allocprof_young_ptr,       \
                                        size_plus_header);              \
    } else {                                                            \
      caml_young_limit = caml_young_end;                                \
    }                                                                   \
  }

#define ALLOCPROF_SMALL_ALLOC(wosize)                                   \
  if( caml_gcprof_flag &  GCPROF_MODE_BACKTRACES ){                     \
    caml_allocprof_future_minor_alloc(caml_young_ptr,Bhsize_wosize (wosize)); \
    caml_allocprof_young_ptr = caml_young_ptr;                          \
    caml_young_limit = caml_allocprof_young_ptr - 1;                    \
  }

/* in minor_gc.c:caml_minor_collection() */
#define ALLOCPROF_MINOR_COLLECTION()                          \
  if( caml_gcprof_flag &  GCPROF_MODE_BACKTRACES )            \
    caml_allocprof_minor_collection();

/* in minor_gc.c:caml_set_minor_heap_size() and caml_empty_minor_heap()

  If alloc-profiling is on, set [caml_young_limit] so that it will
  trigger a call to [caml_garbage_collection] immediatly at the next
  minor allocation. Also save current value of caml_young_ptr in
  caml_allocprof_young_ptr.
*/
#define ALLOCPROF_SET_YOUNG_LIMIT()                                   \
  if( caml_gcprof_flag &  GCPROF_MODE_BACKTRACES ){                   \
      caml_young_limit = caml_young_end-1;                            \
      caml_allocprof_young_ptr = caml_young_end;                      \
  } else {                                                            \
    caml_young_limit = caml_young_start;                              \
  }

/* in memory.c: caml_alloc_shr_loc() */
#define ALLOCPROF_ALLOC_MAJOR(wosize, tag, locid)                     \
  if( (caml_gcprof_flag &  GCPROF_MODE_BACKTRACES)                    \
      && !caml_in_minor_collection )                                  \
    caml_allocprof_alloc_major(wosize, tag, locid)

#else

#define ALLOCPROF_SMALL_ALLOC(wosize)
#define ALLOCPROF_BEGIN_GARBAGE_COLLECTION() \
  caml_young_limit = caml_young_start;
#define ALLOCPROF_END_GARBAGE_COLLECTION()
#define ALLOCPROF_MINOR_COLLECTION()
#define ALLOCPROF_SET_YOUNG_LIMIT()             \
  caml_young_limit = caml_young_start;
#define ALLOCPROF_ALLOC_MAJOR(wosize, tag, locid)

#endif

#ifdef MEMPROF_INSIDE

CAMLextern void ocp_allocprof_init();
CAMLextern void ocp_allocprof_exit();
CAMLextern void ocp_allocprof_minor_collection();
CAMLextern void ocp_allocprof_future_minor_alloc(char* young_ptr, 
                                                  mlsize_t size_plus_header);
CAMLextern void ocp_allocprof_alloc_major(mlsize_t sz, tag_t tag, profiling_t locid);

#else /* !MEMPROF_INSIDE */

CAMLextern void caml_allocprof_init();
CAMLextern void caml_allocprof_exit();
CAMLextern void caml_allocprof_minor_collection();
CAMLextern void caml_allocprof_future_minor_alloc(char* young_ptr, 
                                                  mlsize_t size_plus_header);
CAMLextern void caml_allocprof_alloc_major(mlsize_t sz, tag_t tag, profiling_t locid);

#endif /* MEMPROF_INSIDE */


/********************************************************************/
/*                                                                  */
/*                     Common init/exit functions                   */
/*                                                                  */
/********************************************************************/

CAMLextern int caml_heapdump_in_dump;

/* in startup.c:parse_camlrunparam */
/*
 #define MEMPROF_PARSE_OCAMLRUNPARAM()                        \
  case 'g': scanmult (opt, &caml_grayvals_ratio); break;
*/


typedef struct {
  const char *filename;
  const int line;
  const int locid;
} memprof_id_desc;

CAMLextern int ocp_monitor_update_gc_stats();


#endif



#ifdef CAML_MORE_CALLBACK_H
  CAMLextern void caml_iter_named_value(caml_named_action f);
#endif

#ifdef CAML_MORE_ROOTS_H
  CAMLextern void caml_do_dynglobals (dyn_scanning_action);
#endif

#ifdef CAML_MORE_MD5_H
  CAMLexport value caml_md5_from_channel(struct channel * chan, value len);
#endif

#ifdef CAML_MORE_ALLOC_H
#ifdef NATIVE_CODE

#define MAX_STATS_LEVELS 20
#define MAX_STATS_FRIENDS 50

typedef struct alloc_stats {
  frame_descr *d;
  uintnat top_pos;
  uintnat any_pos;
  uintnat last_sample;
  uintnat levels[MAX_STATS_LEVELS];
  uintnat friends[MAX_STATS_FRIENDS+1];
  uintnat friends_stats[MAX_STATS_FRIENDS];
} alloc_stats;

CAMLextern alloc_stats * caml_frame_stats;

#endif
#endif
  
#ifdef __cplusplus
}
#endif





