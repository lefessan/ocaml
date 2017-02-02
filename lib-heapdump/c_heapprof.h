/**************************************************************************/
/*                                                                        */
/*   Copyright 2014-2017 OCamlPro SAS --- typerex-memprof                 */
/*                                                                        */
/*   Do not redistribute, without permission of OCamlPro, source          */
/*   or binary copies of this software.                                   */
/*                                                                        */
/**************************************************************************/

#ifndef __HEAPDUMP__
#define __HEAPDUMP__

// Fabrice: should be done before including this file:
// #include "mlvalues.h"
#include <stdio.h>

#ifdef __cplusplus
extern "C" {
#endif

extern int caml_memprof_next_location_id;
CAMLextern int caml_heapdump_in_dump;


typedef struct {
  const char *filename;
  const int line;
  const int locid;
} memprof_id_desc;
  
#ifdef _WIN32

#define HEAPDUMP_GETPPID() win32_getppid_c()
#define HEAPDUMP_MKDIR(dirname) mkdir(dirname)
#define HEAPDUMP_LSTAT stat
#define HEAPDUMP_GETPID _getpid
#define HEAPDUMP_DIRNAME(s) get_dirname(s)
#define HEAPDUMP_STRNDUP(s,n) ocp_strndup(s,n)
#else

#define HEAPDUMP_GETPPID() getppid()
#define HEAPDUMP_MKDIR(dirname) mkdir(dirname, 0755)
#define HEAPDUMP_LSTAT lstat
#define HEAPDUMP_GETPID getpid
#define HEAPDUMP_DIRNAME(s) dirname(s)
#define HEAPDUMP_STRNDUP(s,n) strndup(s,n)
#endif

#define HEAPDUMP_FILTER_VAR "OCP_MEMPROF_FILTER"
#define HEAPDUMP_DESTDIR_VAR "OCP_MEMPROF_DESTDIR"

#define HEAPDUMP_MAJORGC (Val_long(0))
#define HEAPDUMP_SIGNAL (Val_long(1))
#define HEAPDUMP_USER (Val_long(2))
#define HEAPDUMP_MEMPROF_RECORD (Val_long(3))

extern int ocp_heapdump_after_gc;

extern void ocp_heapdump_init(void);
extern int ocp_heapdump_is_applicable(char *exe_name);
CAMLextern char* ocp_heapdump_get_destdir(int *pid, int *heap_num);
CAMLextern void ocp_heapdump_do_dump(char *dumpname, value kind);
CAMLextern void (*ocp_heapdump_hook)(FILE *);
CAMLextern int ocp_heapdump_hook_process;
CAMLextern  void ocp_heapdump_store_uint(uint64_t c, FILE* oc);

CAMLextern void ocp_heapdump_pi_do_dump(char *filename, value md5);
CAMLextern void ocp_heapdump_pi_section(FILE* pi_file, char *name, mlsize_t size);
CAMLextern void ocp_heapdump_pi_setname(char *name);

#include "caml/ocp_memprof.h"
CAMLextern struct memprof_init ocp_memprof_info;
#ifdef __cplusplus
}
#endif

#endif // __HEAPDUMP__
