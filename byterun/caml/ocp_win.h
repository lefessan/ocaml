/**************************************************************************/
/*                                                                        */
/*  Copyright 2014, OCamlPro. All rights reserved.                        */
/*                                                                        */
/* Contributors:                                                          */
/* * Fabrice Le Fessant (INRIA/OCamlPro)                                  */
/*                                                                        */
/**************************************************************************/

#ifndef CAML_OCPWIN_H
#define CAML_OCPWIN_H

#ifdef _WIN32

#include <windows.h>

/* from ocamltopwin, startocaml.c */
extern int caml_ReadRegistryValue(HKEY h,
                        char ** keys, int pos, int len,
			char *entry,
                           unsigned char *dest, unsigned long size);

#endif //_WIN32

#endif // CAML_OCPWIN_H

