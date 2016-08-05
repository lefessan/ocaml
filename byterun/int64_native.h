/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2002 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* Wrapper macros around native 64-bit integer arithmetic,
   so that it has the same interface as the software emulation
   provided in int64_emul.h */

#ifndef CAML_INT64_NATIVE_H
#define CAML_INT64_NATIVE_H

#define I64_literal(hi,lo) ((int64)(hi) << 32 | (lo))
#define I64_split(x,hi,lo) (hi = (uint32)((x)>>32), lo = (uint32)(x))
#define I64_compare(x,y) (((x) > (y)) - ((x) < (y)))
#define I64_ult(x,y) ((uint64)(x) < (uint64)(y))
#define I64_neg(x) (-(x))
#define I64_add(x,y) ((x) + (y))
#define I64_sub(x,y) ((x) - (y))
#define I64_mul(x,y) ((x) * (y))
#define I64_is_zero(x) ((x) == 0)
#define I64_is_negative(x) ((x) < 0)
#define I64_is_min_int(x) ((x) == ((int64)1 << 63))
#define I64_is_minus_one(x) ((x) == -1)

/* compatibility mingw32-msvc32: under mingw32, these functions are
  implemented by ___divdi3, ___moddi3, etc. from the libgcc_s.a. 
  Here, we take the code from int64_emul.h and use it to perform the
  same operations. */
#if defined(_WIN32) && !defined(ARCH_SIXTYFOUR)

#  ifdef ARCH_BIG_ENDIAN
typedef struct { uint32 h, l; } uint64_xx, int64_xx;
#  else
typedef struct { uint32 l, h; } uint64_xx, int64_xx;
#  endif

#define LXX(v) ( (int64_xx*) &v )->l
#define HXX(v) ( (int64_xx*) &v )->h


/* Division and modulus */

#define I64_SHL1(x) HXX(x) = (HXX(x) << 1) | (LXX(x) >> 31); LXX(x) <<= 1
#define I64_SHR1(x) LXX(x) = (LXX(x) >> 1) | (HXX(x) << 31); HXX(x) >>= 1

/* Unsigned comparison */
static int I64_ucompare(uint64 x, uint64 y)
{
  if (HXX(x) > HXX(y)) return 1;
  if (HXX(x) < HXX(y)) return -1;
  if (LXX(x) > LXX(y)) return 1;
  if (LXX(x) < LXX(y)) return -1;
  return 0;
}

static void I64_udivmod(uint64 modulus, uint64 divisor,
                        uint64 * quo, uint64 * mod)
{
  int64 quotient, mask;
  int cmp;

  HXX(quotient) = 0; LXX(quotient) = 0;
  HXX(mask) = 0; LXX(mask) = 1;
  while ((int32) HXX(divisor) >= 0) {
    cmp = I64_ucompare(divisor, modulus);
    I64_SHL1(divisor);
    I64_SHL1(mask);
    if (cmp >= 0) break;
  }
  while (LXX(mask) | HXX(mask)) {
    if (I64_ucompare(modulus, divisor) >= 0) {
      HXX(quotient) |= HXX(mask); LXX(quotient) |= LXX(mask);
      modulus = I64_sub(modulus, divisor);
    }
    I64_SHR1(mask);
    I64_SHR1(divisor);
  }
  *quo = quotient;
  *mod = modulus;
}

static int64 I64_div(int64 x, int64 y)
{
  int64 q, r;
  int32 sign;

  sign = HXX(x) ^ HXX(y);
  if ((int32) HXX(x) < 0) x = I64_neg(x);
  if ((int32) HXX(y) < 0) y = I64_neg(y);
  I64_udivmod(x, y, &q, &r);
  if (sign < 0) q = I64_neg(q);
  return q;
}

static int64 I64_mod(int64 x, int64 y)
{
  int64 q, r;
  int32 sign;

  sign = HXX(x);
  if ((int32) HXX(x) < 0) x = I64_neg(x);
  if ((int32) HXX(y) < 0) y = I64_neg(y);
  I64_udivmod(x, y, &q, &r);
  if (sign < 0) r = I64_neg(r);
  return r;
}
#else
#define I64_div(x,y) ((x) / (y))
#define I64_mod(x,y) ((x) % (y))
#define I64_udivmod(x,y,quo,rem) \
  (*(rem) = (uint64)(x) % (uint64)(y), \
   *(quo) = (uint64)(x) / (uint64)(y))
#endif

#define I64_and(x,y) ((x) & (y))
#define I64_or(x,y) ((x) | (y))
#define I64_xor(x,y) ((x) ^ (y))
#define I64_lsl(x,y) ((x) << (y))
#define I64_asr(x,y) ((x) >> (y))
#define I64_lsr(x,y) ((uint64)(x) >> (y))
#define I64_to_intnat(x) ((intnat) (x))
#define I64_of_intnat(x) ((intnat) (x))
#define I64_to_int32(x) ((int32) (x))
#define I64_of_int32(x) ((int64) (x))
#define I64_to_double(x) ((double)(x))
#define I64_of_double(x) ((int64)(x))

#define I64_bswap(x) ((((x) & 0x00000000000000FFULL) << 56) | \
                      (((x) & 0x000000000000FF00ULL) << 40) | \
                      (((x) & 0x0000000000FF0000ULL) << 24) | \
                      (((x) & 0x00000000FF000000ULL) << 8) |  \
                      (((x) & 0x000000FF00000000ULL) >> 8) |  \
                      (((x) & 0x0000FF0000000000ULL) >> 24) | \
                      (((x) & 0x00FF000000000000ULL) >> 40) | \
                      (((x) & 0xFF00000000000000ULL) >> 56))

#endif /* CAML_INT64_NATIVE_H */
