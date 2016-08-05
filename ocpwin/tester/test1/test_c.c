
#include <stdlib.h>
#include <stdio.h>
#include <winsock2.h>
#include <windows.h>

#include "caml/mlvalues.h"
#include "caml/alloc.h"
#include "caml/bigarray.h"
#include "caml/callback.h"
#include "caml/config.h"
#include "caml/custom.h"
#include "caml/fail.h"
#include "caml/hash.h"
#include "caml/printexc.h"
#include "caml/intext.h"
#include "caml/memory.h"
#include "caml/misc.h"
#include "caml/signals.h"
// #include "caml/socketaddr.h"
#include "caml/threads.h"
#include "caml/unixsupport.h"

value test_f(value x_v)
{
  return x_v;
}
