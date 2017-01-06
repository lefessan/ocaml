
#ifndef CAML_OCPWIN_H
#define CAML_OCPWIN_H

/* Try to find ld.conf in a relocatable way */
CAMLextern char* caml_ocpwin_find_ld_conf();

#ifndef NATIVE_CODE
/* replace '+' in [path] by [stdlib]. The allocated string is kept
   and never freed. */
CAMLextern char* caml_ocpwin_expand_path(char* path, char* stdlib);
#endif

#endif
