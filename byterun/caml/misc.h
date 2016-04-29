/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*         Xavier Leroy and Damien Doligez, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* Miscellaneous macros and variables. */

#ifndef CAML_MISC_H
#define CAML_MISC_H

#ifndef CAML_NAME_SPACE
#include "compatibility.h"
#endif
#include "config.h"

/* Standard definitions */

#include <stddef.h>
#include <stdlib.h>

/* Basic types and constants */

typedef size_t asize_t;

#ifndef NULL
#define NULL 0
#endif

/* <private> */
typedef char * addr;
/* </private> */

#ifdef __GNUC__
  /* Works only in GCC 2.5 and later */
  #define Noreturn __attribute__ ((noreturn))
#else
  #define Noreturn
#endif

/* Export control (to mark primitives and to handle Windows DLL) */

#define CAMLexport
#define CAMLprim
#define CAMLextern extern

/* Weak function definitions that can be overriden by external libs */
/* Conservatively restricted to ELF and MacOSX platforms */
#if defined(__GNUC__) && (defined (__ELF__) || defined(__APPLE__))
#define CAMLweakdef __attribute__((weak))
#else
#define CAMLweakdef
#endif

#ifdef __cplusplus
extern "C" {
#endif

/* GC timing hooks. These can be assigned by the user.
   [caml_minor_gc_begin_hook] must not allocate nor change any heap value.
   The others can allocate and even call back to OCaml code.
*/
typedef void (*caml_timing_hook) (void);
extern caml_timing_hook caml_major_slice_begin_hook, caml_major_slice_end_hook;
extern caml_timing_hook caml_minor_gc_begin_hook, caml_minor_gc_end_hook;
extern caml_timing_hook caml_finalise_begin_hook, caml_finalise_end_hook;

/* Assertions */

#ifdef DEBUG
#define CAMLassert(x) \
  ((x) ? (void) 0 : caml_failed_assert ( #x , __FILE__, __LINE__))
CAMLextern int caml_failed_assert (char *, char *, int);
#else
#define CAMLassert(x) ((void) 0)
#endif

CAMLextern void caml_fatal_error (char *msg) Noreturn;
CAMLextern void caml_fatal_error_arg (char *fmt, char *arg) Noreturn;
CAMLextern void caml_fatal_error_arg2 (char *fmt1, char *arg1,
                                       char *fmt2, char *arg2) Noreturn;

/* Safe string operations */

CAMLextern char * caml_strdup(const char * s);
CAMLextern char * caml_strconcat(int n, ...); /* n args of const char * type */

/* Use macros for some system calls being called from OCaml itself.
  These calls can be either traced for security reasons, or changed to
  virtualize the program. */

#ifndef CAML_WITH_CPLUGINS

#define CAML_SYS_EXIT(retcode) exit(retcode)
#define CAML_SYS_OPEN(filename,flags,perm) open(filename,flags,perm)
#define CAML_SYS_CLOSE(fd) close(fd)
#define CAML_SYS_STAT(filename,st) stat(filename,st)
#define CAML_SYS_UNLINK(filename) unlink(filename)
#define CAML_SYS_RENAME(old_name,new_name) rename(old_name, new_name)
#define CAML_SYS_CHDIR(dirname) chdir(dirname)
#define CAML_SYS_GETENV(varname) getenv(varname)
#define CAML_SYS_SYSTEM(command) system(command)
#define CAML_SYS_READ_DIRECTORY(dirname,tbl) caml_read_directory(dirname,tbl)

#else

#define CAML_CPLUGINS_EXIT 0
#define CAML_CPLUGINS_OPEN 1
#define CAML_CPLUGINS_CLOSE 2
#define CAML_CPLUGINS_STAT 3
#define CAML_CPLUGINS_UNLINK 4
#define CAML_CPLUGINS_RENAME 5
#define CAML_CPLUGINS_CHDIR 6
#define CAML_CPLUGINS_GETENV 7
#define CAML_CPLUGINS_SYSTEM 8
#define CAML_CPLUGINS_READ_DIRECTORY 9

extern intnat (*caml_cplugins_prim)(int,intnat,intnat,intnat);

#define CAML_SYS_PRIM_1(code,prim,arg1)               \
  (caml_cplugins_prim == NULL) ? prim(arg1) :    \
  caml_cplugins_prim(code,(intnat) (arg1),0,0)
#define CAML_SYS_STRING_PRIM_1(code,prim,arg1)               \
  (caml_cplugins_prim == NULL) ? prim(arg1) :    \
  (char*)caml_cplugins_prim(code,(intnat) (arg1),0,0)
#define CAML_SYS_PRIM_2(code,prim,arg1,arg2)                         \
  (caml_cplugins_prim == NULL) ? prim(arg1,arg2) :              \
  caml_cplugins_prim(code,(intnat) (arg1), (intnat) (arg2),0)
#define CAML_SYS_PRIM_3(code,prim,arg1,arg2,arg3)                            \
  (caml_cplugins_prim == NULL) ? prim(arg1,arg2,arg3) :                 \
  caml_cplugins_prim(code,(intnat) (arg1), (intnat) (arg2),(intnat) (arg3))

#define CAML_SYS_EXIT(retcode) \
  CAML_SYS_PRIM_1(CAML_CPLUGINS_EXIT,exit,retcode)
#define CAML_SYS_OPEN(filename,flags,perm)                      \
  CAML_SYS_PRIM_3(CAML_CPLUGINS_OPEN,open,filename,flags,perm)
#define CAML_SYS_CLOSE(fd)                      \
  CAML_SYS_PRIM_1(CAML_CPLUGINS_CLOSE,close,fd)
#define CAML_SYS_STAT(filename,st)                      \
  CAML_SYS_PRIM_2(CAML_CPLUGINS_STAT,stat,filename,st)
#define CAML_SYS_UNLINK(filename)                       \
  CAML_SYS_PRIM_1(CAML_CPLUGINS_UNLINK,unlink,filename)
#define CAML_SYS_RENAME(old_name,new_name)                              \
  CAML_SYS_PRIM_2(CAML_CPLUGINS_RENAME,rename,old_name,new_name)
#define CAML_SYS_CHDIR(dirname)                         \
  CAML_SYS_PRIM_1(CAML_CPLUGINS_CHDIR,chdir,dirname)
#define CAML_SYS_GETENV(varname)                        \
  CAML_SYS_STRING_PRIM_1(CAML_CPLUGINS_GETENV,getenv,varname)
#define CAML_SYS_SYSTEM(command)                        \
  CAML_SYS_PRIM_1(CAML_CPLUGINS_SYSTEM,system,command)
#define CAML_SYS_READ_DIRECTORY(dirname,tbl)                            \
  CAML_SYS_PRIM_2(CAML_CPLUGINS_READ_DIRECTORY,caml_read_directory,     \
                  dirname,tbl)

extern void caml_cplugins_init(char * exe_name, char ** argv);

/* A plugin MUST define a symbol "caml_cplugin_init" with the prototype:

void caml_cplugin_init(char* exe_name, char** argv, void* query)

and use the [query] parameter to set:

static void* (*cplugins_query)(int query);

This function can then be used to query the address of other symbols
in the executable, limited to the following queries:
*/
#define CAML_CPLUGINS_PRIM_HOOK           0
#define CAML_CPLUGINS_PRIM_READ_DIRECTORY 1


/* to write plugins for CAML_SYS_READ_DIRECTORY, we will need the
   definition of struct ext_table to be public. */
  
#endif

/* Data structures */

struct ext_table {
  int size;
  int capacity;
  void ** contents;
};

extern void caml_ext_table_init(struct ext_table * tbl, int init_capa);
extern int caml_ext_table_add(struct ext_table * tbl, void * data);
extern void caml_ext_table_free(struct ext_table * tbl, int free_entries);

/* <private> */

/* GC flags and messages */

extern uintnat caml_verb_gc;
void caml_gc_message (int, char *, uintnat);

/* Memory routines */

char *caml_aligned_malloc (asize_t, int, void **);

#ifdef DEBUG
#ifdef ARCH_SIXTYFOUR
#define Debug_tag(x) (0xD700D7D7D700D6D7ul \
                      | ((uintnat) (x) << 16) \
                      | ((uintnat) (x) << 48))
#else
#define Debug_tag(x) (0xD700D6D7ul | ((uintnat) (x) << 16))
#endif /* ARCH_SIXTYFOUR */

/*
  00 -> free words in minor heap
  01 -> fields of free list blocks in major heap
  03 -> heap chunks deallocated by heap shrinking
  04 -> fields deallocated by [caml_obj_truncate]
  10 -> uninitialised fields of minor objects
  11 -> uninitialised fields of major objects
  15 -> uninitialised words of [caml_aligned_malloc] blocks
  85 -> filler bytes of [caml_aligned_malloc]

  special case (byte by byte):
  D7 -> uninitialised words of [caml_stat_alloc] blocks
*/
#define Debug_free_minor     Debug_tag (0x00)
#define Debug_free_major     Debug_tag (0x01)
#define Debug_free_shrink    Debug_tag (0x03)
#define Debug_free_truncate  Debug_tag (0x04)
#define Debug_uninit_minor   Debug_tag (0x10)
#define Debug_uninit_major   Debug_tag (0x11)
#define Debug_uninit_align   Debug_tag (0x15)
#define Debug_filler_align   Debug_tag (0x85)

#define Debug_uninit_stat    0xD7

extern void caml_set_fields (char *, unsigned long, unsigned long);
#endif /* DEBUG */


#ifndef CAML_AVOID_CONFLICTS
#define Assert CAMLassert
#endif

/* snprintf emulation for Win32 */

#ifdef _WIN32
extern int caml_snprintf(char * buf, size_t size, const char * format, ...);
#define snprintf caml_snprintf
#endif

/* </private> */

#ifdef __cplusplus
}
#endif

#endif /* CAML_MISC_H */
