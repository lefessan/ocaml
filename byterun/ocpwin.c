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
// #include "caml/unixsupport.h"
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
  caml_failwith("No implementation for caml_win32_read_registry");
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

#endif
