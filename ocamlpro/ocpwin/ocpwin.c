/**************************************************************************/
/*                                                                        */
/*  Copyright 2014, OCamlPro. All rights reserved.                        */
/*                                                                        */
/* Contributors:                                                          */
/* * Fabrice Le Fessant (INRIA/OCamlPro)                                  */
/*                                                                        */
/**************************************************************************/

#ifdef __CYGWIN__
#define _WIN32
#endif

#include <stdio.h>


#include "caml/mlvalues.h"
#include "caml/alloc.h"
/* #include "unixsupport.h" */
#include "caml/memory.h"
#include "caml/signals.h"
#include "caml/fail.h"

#include <string.h>

#ifdef _WIN32
#include <windows.h>
#endif

/*********************************************************************/

#ifdef _WIN32

/* from ocamltopwin, startocaml.c */
static int ReadRegistryValue(HKEY h,
                        char ** keys, int pos, int len,
			char *entry,
                        unsigned char *dest, unsigned long size)
{
    LONG ret;

    if( pos < len ){
      HKEY hret;
      /*      fprintf(stderr, "RegOpenKeyExA(%s)\n", keys[pos]); */
      if (RegOpenKeyExA(h, keys[pos], 0, KEY_QUERY_VALUE, &hret) != ERROR_SUCCESS)
        return -1;
      ret = ReadRegistryValue(hret, keys, pos+1, len, entry, dest, size);
      RegCloseKey(hret);
      return ret;
    } else {
       DWORD dwType = REG_SZ;
       /*       fprintf(stderr, "RegQueryValueExA(%s)\n", entry); */
       ret = RegQueryValueExA(h, entry, 0, &dwType, dest, &size);
       if (ret == ERROR_SUCCESS)
         return size;
       else
	 return -1;
    }
}

static HKEY TranslateHROOT(value hroot_v)
{
  switch(Int_val(hroot_v)){
  case 0 :
    /*    fprintf(stderr, "From HKEY_CLASSES_ROOT\n"); */
    return HKEY_CLASSES_ROOT;
  case 1 :
    /* fprintf(stderr, "From HKEY_CURRENT_CONFIG\n"); */
    return HKEY_CURRENT_CONFIG;
  case 2 :
    /* fprintf(stderr, "From HKEY_CURRENT_USER\n"); */
    return HKEY_CURRENT_USER;
  case 3 :
    /* fprintf(stderr, "From HKEY_LOCAL_MACHINE\n"); */
    return HKEY_LOCAL_MACHINE;
  case 4 :
    /* fprintf(stderr, "From HKEY_USERS\n"); */
    return HKEY_USERS;
  default: break;
  }
  caml_failwith("TranslateHROOT");
  ((void) hroot_v);
}

static DWORD TranslateDwType(value dwType_v)
{
  switch(Int_val(dwType_v)){
  case 0 : return REG_SZ;
  case 1: return REG_EXPAND_SZ;
  default: break;
  }
  caml_failwith("TranslateDwType");
}


#endif

/*
hroot_v: 0 = HKEY_CURRENT_USER
         1 = HKEY_LOCAL_MACHINE
return: size of result
    or  -1 : error
*/
CAMLprim value caml_ocp_win32_read_registry(value hroot_v,value keys_v,value entry_v,value result_v)
{
#ifdef _WIN32
  int ret =
     ReadRegistryValue(
		  TranslateHROOT(hroot_v),
		  (char**) keys_v,
		  0, Wosize_val(keys_v),
		  String_val(entry_v),
		  (unsigned char*)String_val(result_v),
		  caml_string_length(result_v));
  if (ret > 0)
    return Val_int(ret-1); /* Fabrice: the ending 0 is counted ! */
  else
    return Val_int(-1);
#else
  caml_failwith("No implementation for caml_ocp_win32_read_registry");
#endif
}


#ifdef HAS_PUTENV

CAMLprim value caml_ocp_putenv(value name, value val)
{
  mlsize_t namelen = caml_string_length(name);
  mlsize_t vallen = caml_string_length(val);
  char * s = (char *) caml_stat_alloc(namelen + 1 + vallen + 1);

  memmove (s, String_val(name), namelen);
  s[namelen] = '=';
  memmove (s + namelen + 1, String_val(val), vallen);
  s[namelen + 1 + vallen] = 0;
  if (putenv(s) == -1) {
    caml_stat_free(s);
    return Val_int(1);
  }
  return Val_int(0);
}

#else

CAMLprim value caml_ocp_putenv(value name, value val)
{ invalid_argument("putenv not implemented"); }

#endif // HAS_PUTENV


/* TODO include code from byterun/dynlink.c */
CAMLexport char * caml_ocpwin_find_ld_conf()
{
  return NULL;
}

#ifndef NATIVE_CODE

#define CAML_INTERNALS

#include "caml/dynlink.h"

/* TODO: expand + in stdlib */
CAMLexport char* caml_ocpwin_expand_path(char* path, char* stdlib)
{
  return path;
}

#endif















// A copy of byterun/dynlink.c




#if 0


/* Uncommenting this can help to find why dll libraries are not found */
// #define OCAML_DEBUG_DLLPATH

#include <stdio.h>

/* Dynamic loading of C primitives. */
#ifndef _WIN32
#include <libgen.h>
#endif

#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <sys/stat.h>
#include "config.h"
#ifdef HAS_UNISTD
#include <unistd.h>
#endif
#include "alloc.h"
#include "dynlink.h"
#include "fail.h"
#include "mlvalues.h"
#include "memory.h"
#include "misc.h"
#include "osdeps.h"
#include "prims.h"

#ifndef NATIVE_CODE

/* The table of primitives */
struct ext_table caml_prim_table;

#ifdef DEBUG
/* The names of primitives (for instrtrace.c) */
struct ext_table caml_prim_name_table;
#endif

/* The table of shared libraries currently opened */
static struct ext_table shared_libs;

/* The search path for shared libraries */
struct ext_table caml_shared_libs_path;

/* Look up the given primitive name in the built-in primitive table,
   then in the opened shared libraries (shared_libs) */
static c_primitive lookup_primitive(char * name)
{
  int i;
  void * res;

  for (i = 0; caml_names_of_builtin_cprim[i] != NULL; i++) {
    if (strcmp(name, caml_names_of_builtin_cprim[i]) == 0)
      return caml_builtin_cprim[i];
  }
  for (i = 0; i < shared_libs.size; i++) {
    res = caml_dlsym(shared_libs.contents[i], name);
    if (res != NULL) return (c_primitive) res;
  }
  return NULL;
}

/* Parse the OCAML_STDLIB_DIR/ld.conf file and add the directories
   listed there to the search path */

#define LD_CONF_NAME "ld.conf"

extern char * caml_exe_name;

#ifdef _WIN32    
#define FILEPATHSEP '\\'
#define access _access
#define R_OK 4
#else
#define FILEPATHSEP '/'
#endif

#ifdef _WIN32
#include <windows.h>
#include <psapi.h>
static int ReadRegistryValue(HKEY h,
                             char ** keys, int pos, int len,
                             char *entry,
                             DWORD *dwType,
                             unsigned char *dest,
                             unsigned long size)
{
    LONG ret;

    if( pos < len ){
      HKEY hret;
      if(RegOpenKeyExA(h, keys[pos], 0, KEY_QUERY_VALUE, &hret)
         != ERROR_SUCCESS)
        return -1;
      ret = ReadRegistryValue(hret, keys, pos+1,len,entry,dwType,dest,size);
      RegCloseKey(hret);
      return ret;
    } else {
      ret = RegQueryValueExA(h, entry, 0, dwType, dest, &size);
      if (ret == ERROR_SUCCESS)
        return size;
      else
        return -1;
    }
}
#endif

static char * parse_ld_conf(void)
{
  char * stdlib, * ldconfname, * config, * p, * q;
  struct stat st;
  int ldconf, nread;
  int need_free = 0;

  stdlib = getenv("OCAMLLIB");
  if (stdlib == NULL) stdlib = getenv("CAMLLIB");
#ifdef _WIN32
  /* In OCPWin, if nothing is specified by env variables, we
    use the registry to find the current switch.
   */
  if (stdlib == NULL){
    char bindir[MAX_PATH];
    char *ocpwin_reg_key[] = {
      "Software", "OCamlPro", "OCPWin", NULL
    };
    DWORD dwType;
    int ok = ReadRegistryValue(HKEY_CURRENT_USER,
                               ocpwin_reg_key, 0, 3,
                               "CurrentBinDir",
                               &dwType,
                               (unsigned char*)bindir,
                               MAX_PATH);
    if(ok){
      /* Assumes OCaml is in "lib", not "lib/ocaml" ! */
      int len = strlen(bindir);
      bindir[len-3] = 'l';
      bindir[len-2] = 'i';
      bindir[len-1] = 'b';
      stdlib = caml_stat_alloc(len+1);
      strncpy(stdlib, bindir, len+1);
      need_free = 1;
    }
  }
#else
  if (stdlib == NULL) stdlib = OCAML_STDLIB_DIR;
  if (stdlib == NULL || access(stdlib, R_OK) != 0 ) {
    /* Fabrice: is this really supposed to work on Unix anyway ?
       caml_exe_name is the name of the bytecode program, not the
       name of ocamlrun, so it will only work for bytecode programs
       located in the bin/ directory of OCaml ?
     */
    
    int prefix_len = strlen(caml_exe_name);
    int ncount = 2;
    while(ncount > 0 && prefix_len != 0){
      prefix_len--;
      if( caml_exe_name[prefix_len] == FILEPATHSEP ) ncount--;
    }
    stdlib = caml_stat_alloc( prefix_len + strlen(OCAML_STDLIB_DIR) + 2);
    strncpy(stdlib, caml_exe_name, prefix_len);
    stdlib[prefix_len] = FILEPATHSEP;
    strncpy(stdlib + prefix_len + 1, OCAML_STDLIB_DIR, 5);
    need_free = 1;
  }
#endif

  ldconfname = caml_stat_alloc(strlen(stdlib) + 2 + sizeof(LD_CONF_NAME));
  strcpy(ldconfname, stdlib);
  strcat(ldconfname, "/" LD_CONF_NAME);
#ifdef OCAML_DEBUG_DLLPATH
  fprintf(stderr, "ldconfname=%s\n", ldconfname);
#endif
  if (stat(ldconfname, &st) == -1) {
    caml_stat_free(ldconfname);
    if( need_free ) caml_stat_free(stdlib);
    return NULL;
  }
  ldconf = open(ldconfname, O_RDONLY, 0);
  if (ldconf == -1){
    if( need_free ) caml_stat_free(stdlib);
    caml_fatal_error_arg("Fatal error: cannot read loader config file %s\n",
                         ldconfname);
  }
  config = caml_stat_alloc(st.st_size + 1);
  nread = read(ldconf, config, st.st_size);
  if (nread == -1){
    if( need_free ) caml_stat_free(stdlib);
    caml_fatal_error_arg
      ("Fatal error: error while reading loader config file %s\n",
       ldconfname);
  }
  config[nread] = 0;
  q = config;
  for (p = config; *p != 0; p++) {
    if (*p == '\n') {
      *p = 0;
      if( *q == '+' ){
	int qlen = strlen(q);
	int stdlen = strlen(stdlib);
	char *dest = caml_stat_alloc(qlen + stdlen + 1);
	strncpy(dest, stdlib, stdlen);
#ifdef _WIN32
	dest[stdlen] = '\\';
#else
	dest[stdlen] = '/';
#endif
	strncpy(dest+stdlen+1, q+1, qlen);
#ifdef OCAML_DEBUG_DLLPATH
	fprintf(stderr, "dllpath[+]:'%s'\n",dest);
#endif
	caml_ext_table_add(&caml_shared_libs_path, dest);
      } else { 
#ifdef OCAML_DEBUG_DLLPATH
	fprintf(stderr, "dllpath[+]:'%s'\n",q);
#endif
	caml_ext_table_add(&caml_shared_libs_path, q);
      }
      q = p + 1;
    }
  }
  if (q < p) caml_ext_table_add(&caml_shared_libs_path, q);
  close(ldconf);
  caml_stat_free(ldconfname);
  if( need_free ) caml_stat_free(stdlib);
  return config;
}

/* Open the given shared library and add it to shared_libs.
   Abort on error. */
static void open_shared_lib(char * name)
{
  char * realname;
  void * handle;

  realname = caml_search_dll_in_path(&caml_shared_libs_path, name);
  caml_gc_message(0x100, "Loading shared library %s\n",
                  (uintnat) realname);
  handle = caml_dlopen(realname, 1, 1);
  if (handle == NULL)
    caml_fatal_error_arg2("Fatal error: cannot load shared library %s\n", name,
                          "Reason: %s\n", caml_dlerror());
  caml_ext_table_add(&shared_libs, handle);
  caml_stat_free(realname);
}

/* Build the table of primitives, given a search path and a list
   of shared libraries (both 0-separated in a char array).
   Abort the runtime system on error. */
void caml_build_primitive_table(char * lib_path,
                                char * libs,
                                char * req_prims)
{
  char * tofree1, * tofree2;
  char * p;

  /* Initialize the search path for dynamic libraries:
     - directories specified on the command line with the -I option
     - directories specified in the CAML_LD_LIBRARY_PATH
     - directories specified in the executable
     - directories specified in the file <stdlib>/ld.conf */
  tofree1 = caml_decompose_path(&caml_shared_libs_path,
                                getenv("CAML_LD_LIBRARY_PATH"));
  if (lib_path != NULL)
    for (p = lib_path; *p != 0; p += strlen(p) + 1)
      caml_ext_table_add(&caml_shared_libs_path, p);
  tofree2 = parse_ld_conf();
  /* Open the shared libraries */
  caml_ext_table_init(&shared_libs, 8);
  if (libs != NULL)
    for (p = libs; *p != 0; p += strlen(p) + 1)
      open_shared_lib(p);
  /* Build the primitive table */
  caml_ext_table_init(&caml_prim_table, 0x180);
#ifdef DEBUG
  caml_ext_table_init(&caml_prim_name_table, 0x180);
#endif
  for (p = req_prims; *p != 0; p += strlen(p) + 1) {
    c_primitive prim = lookup_primitive(p);
    if (prim == NULL  && getenv("OCAML_ALLOWS_UNKNOWN_PRIM") == NULL)
          caml_fatal_error_arg("Fatal error: unknown C primitive `%s'\n", p);
    caml_ext_table_add(&caml_prim_table, (void *) prim);
#ifdef DEBUG
    caml_ext_table_add(&caml_prim_name_table, strdup(p));
#endif
  }
  /* Clean up */
  caml_stat_free(tofree1);
  caml_stat_free(tofree2);
  caml_ext_table_free(&caml_shared_libs_path, 0);
}

/* Build the table of primitives as a copy of the builtin primitive table.
   Used for executables generated by ocamlc -output-obj. */

void caml_build_primitive_table_builtin(void)
{
  int i;
  caml_ext_table_init(&caml_prim_table, 0x180);
#ifdef DEBUG
  caml_ext_table_init(&caml_prim_name_table, 0x180);
#endif
  for (i = 0; caml_builtin_cprim[i] != 0; i++) {
    caml_ext_table_add(&caml_prim_table, (void *) caml_builtin_cprim[i]);
#ifdef DEBUG
    caml_ext_table_add(&caml_prim_name_table,
                       strdup(caml_names_of_builtin_cprim[i]));
#endif
}
}

#endif /* NATIVE_CODE */

/** dlopen interface for the bytecode linker **/

#define Handle_val(v) (*((void **) (v)))

CAMLprim value caml_dynlink_open_lib(value mode, value filename)
{
  void * handle;
  value result;

  caml_gc_message(0x100, "Opening shared library %s\n",
                  (uintnat) String_val(filename));
  handle = caml_dlopen(String_val(filename), Int_val(mode), 1);
  if (handle == NULL) caml_failwith(caml_dlerror());
  result = caml_alloc_small(1, Abstract_tag);
  Handle_val(result) = handle;
  return result;
}

CAMLprim value caml_dynlink_close_lib(value handle)
{
  caml_dlclose(Handle_val(handle));
  return Val_unit;
}

/*#include <stdio.h>*/
CAMLprim value caml_dynlink_lookup_symbol(value handle, value symbolname)
{
  void * symb;
  value result;
  symb = caml_dlsym(Handle_val(handle), String_val(symbolname));
  /* printf("%s = 0x%lx\n", String_val(symbolname), symb);
     fflush(stdout); */
  if (symb == NULL) return Val_unit /*caml_failwith(caml_dlerror())*/;
  result = caml_alloc_small(1, Abstract_tag);
  Handle_val(result) = symb;
  return result;
}

#ifndef NATIVE_CODE

CAMLprim value caml_dynlink_add_primitive(value handle)
{
  return Val_int(caml_ext_table_add(&caml_prim_table, Handle_val(handle)));
}

CAMLprim value caml_dynlink_get_current_libs(value unit)
{
  CAMLparam0();
  CAMLlocal1(res);
  int i;

  res = caml_alloc_tuple(shared_libs.size);
  for (i = 0; i < shared_libs.size; i++) {
    value v = caml_alloc_small(1, Abstract_tag);
    Handle_val(v) = shared_libs.contents[i];
    Store_field(res, i, v);
  }
  CAMLreturn(res);
}

#else

value caml_dynlink_add_primitive(value handle)
{
  caml_invalid_argument("dynlink_add_primitive");
  return Val_unit; /* not reached */
}

value caml_dynlink_get_current_libs(value unit)
{
  caml_invalid_argument("dynlink_get_current_libs");
  return Val_unit; /* not reached */
}

#endif /* NATIVE_CODE */
#endif
