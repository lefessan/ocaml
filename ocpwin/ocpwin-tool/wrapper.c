/* This simple wrapper calls "ocpwin.exe" in the _same_ directory with
   the same arguments. It is a small equivalent to a simlink...
*/

#include <windows.h>
#include <psapi.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static int assembleCmdLine(char *const *argv, char **cmdLine);
static void my_execv(char *new_arg0, const char** new_args);
static int ReadRegistryValue(HKEY h,
                             char ** keys, int pos, int len,
                             char *entry,
                             DWORD *dwType,
                             unsigned char *dest,
                             unsigned long size);

int main( int argc, const char* argv[] )
{
  const char* new_args[argc+3];
  int i;

#if 0
  /* Try to find ocpwin.exe in the same directory. Won't work for bytecode
     files ! */
    int len = strlen(argv[0]);
    char new_arg0[len + 20];
    char* end;
    
    strcpy(new_arg0, argv[0]);
    /*  printf("arg0 = '%s'\n", new_arg0); */
    i = len;
    while(i > 0){
      char c = new_arg0[i-1];
      if( c == '/' || c == '\\' ){
        strcpy(new_arg0 + i, "ocpwin.exe");
        i = 0;
      } else { i--; }    
    }
  }
#endif

  unsigned char new_arg0[MAX_PATH];
  char *ocpwin_reg_key[] = {
    "Software", "OCamlPro", "OCPWin", NULL
  };
  DWORD dwType;
  int ok = ReadRegistryValue(HKEY_CURRENT_USER,
                             ocpwin_reg_key, 0, 3,
                             "CurrentOcpwinWrapper",
                             &dwType,
                             new_arg0,
                             MAX_PATH);
  if(!ok){
    fprintf(stderr, "Error: could not locate CurrentOcpwinWrapper in registry.\n");
    exit(2);
 }
  /* 
  printf("arg0 = '%s'\n", new_arg0);
  exit(2);
  */

  new_args[0] = new_arg0;
  new_args[1] = "-wrapper";
  new_args[2] = argv[0];
  for(i = 1; i<argc; i++) new_args[i+2] = argv[i];
  new_args[argc+2] = NULL;
#if 0
  /* _execv cannot be used, because it 
  (1) creates a new executable 
  (2) immediatly stops the current one
  (3) any program waiting for the current one believes it is finished when,
    actually, the new program is still waiting.
  So we have to use CreateProcess and to wait for the new process to end.
  */
  _execv(new_arg0, new_args);
#endif
  my_execv(new_arg0, new_args);
  
  return 0;
}

static void my_execv(char *new_arg0, const char** new_args)
{
  DWORD status;
  STARTUPINFO si = { 0, };
  PROCESS_INFORMATION pi = { 0, };
  char *new_cmd;
  
  /* Beware: we must quote the arguments !! */
  assembleCmdLine((char * const*)new_args, &new_cmd);
  /*    printf("cmd = [%s]\n", new_cmd); */
  
  si.cb = sizeof(si);
  if(!CreateProcess(new_arg0,
                    new_cmd,
                    NULL,
                    NULL,
                    TRUE,
                    0,
                    NULL,
                    NULL,
                    &si,
                    &pi)){
    fprintf(stderr, "CreateProcess failed (%ld).\n", GetLastError());
    fprintf(stderr, "  Could not locate ocpwin.exe in multi-switch mode.\n");
    fprintf(stderr, "  Hint: you might be trying to run a bytecode program\n");
    fprintf(stderr, "   in multi-switch mode. Compile with -custom instead.\n");
    fprintf(stderr, "  Program: [%s]\n", new_arg0);
    fprintf(stderr, "  CommandLine: [%s]\n", new_cmd);
    exit(2);
  }
  WaitForSingleObject(pi.hProcess, INFINITE);
  if( ! GetExitCodeProcess(pi.hProcess, &status)){
    fprintf(stderr, "GetExitCodeProcess failed (%ld).\n", GetLastError());
    exit(2);      
  }
  CloseHandle(pi.hProcess);
  CloseHandle(pi.hThread);
  exit(status);
}

  /* Code from gpg-browser-bridge/src/windows/createprocess.cc
     under MPL 1.1 licence: */

/*
 * Assemble the command line by concatenating the argv array.
 * On success, this function returns 0 and the resulting command
 * line is returned in *cmdLine.  On failure, it returns -1.
 */
static int assembleCmdLine(char *const *argv, char **cmdLine)
{
    char *const *arg;
    char *p, *q;
    size_t cmdLineSize;
    int numBackslashes;
    int i;
    int argNeedQuotes;

    /*
     * Find out how large the command line buffer should be.
     */
    cmdLineSize = 0;
    for (arg = argv; *arg; arg++) {
        /*
         * \ and " need to be escaped by a \.  In the worst case,
         * every character is a \ or ", so the string of length
         * may double.  If we quote an argument, that needs two ".
         * Finally, we need a space between arguments, and
         * a null byte at the end of command line.
         */
        cmdLineSize += 2 * strlen(*arg)  /* \ and " need to be escaped */
                + 2                      /* we quote every argument */
                + 1;                     /* space in between, or final null */
    }
    p = *cmdLine = (char *)malloc(cmdLineSize);
    if (p == NULL) {
        return -1;
    }

    for (arg = argv; *arg; arg++) {
        /* Add a space to separates the arguments */
        if (arg != argv) {
            *p++ = ' ';
        }
        q = *arg;
        numBackslashes = 0;
        argNeedQuotes = 0;

        /*
         * If the argument is empty or contains white space, it needs to
         * be quoted.
         */
        if (**arg == '\0' || strpbrk(*arg, " \f\n\r\t\v")) {
            argNeedQuotes = 1;
        }

        if (argNeedQuotes) {
            *p++ = '"';
        }
        while (*q) {
            if (*q == '\\') {
                numBackslashes++;
                q++;
            } else if (*q == '"') {
                if (numBackslashes) {
                    /*
                     * Double the backslashes since they are followed
                     * by a quote
                     */
                    for (i = 0; i < 2 * numBackslashes; i++) {
                        *p++ = '\\';
                    }
                    numBackslashes = 0;
                }
                /* To escape the quote */
                *p++ = '\\';
                *p++ = *q++;
            } else {
                if (numBackslashes) {
                    /*
                     * Backslashes are not followed by a quote, so
                     * don't need to double the backslashes.
                     */
                    for (i = 0; i < numBackslashes; i++) {
                        *p++ = '\\';
                    }
                    numBackslashes = 0;
                }
                *p++ = *q++;
            }
        }

        /* Now we are at the end of this argument */
        if (numBackslashes) {
            /*
             * Double the backslashes if we have a quote string
             * delimiter at the end.
             */
            if (argNeedQuotes) {
                numBackslashes *= 2;
            }
            for (i = 0; i < numBackslashes; i++) {
                *p++ = '\\';
            }
        }
        if (argNeedQuotes) {
            *p++ = '"';
        }
    }

    *p = '\0';
    return 0;
}

/* from ocamltopwin, startocaml.c */
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
      /*      fprintf(stderr, "RegOpenKeyExA(%s)\n", keys[pos]); */
      if (RegOpenKeyExA(h, keys[pos], 0, KEY_QUERY_VALUE, &hret) != ERROR_SUCCESS)
        return -1;
      ret = ReadRegistryValue(hret, keys, pos+1, len, entry, dwType, dest, size);
      RegCloseKey(hret);
      return ret;
    } else {
       /*       fprintf(stderr, "RegQueryValueExA(%s)\n", entry); */
       ret = RegQueryValueExA(h, entry, 0, dwType, dest, &size);
       if (ret == ERROR_SUCCESS)
         return size;
       else
	 return -1;
    }
}
