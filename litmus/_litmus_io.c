/*********************************************************************/
/*                          Litmus                                   */
/*                                                                   */
/*        Luc Maranget, INRIA Paris-Rocquencourt, France.            */
/*                                                                   */
/*  Copyright 2014 Institut National de Recherche en Informatique et */
/*  en Automatique and the authors. All rights reserved.             */
/*  This file is distributed  under the terms of the Lesser GNU      */
/*  General Public License.                                          */
/*********************************************************************/

#include "litmus_io.h"

void emit_string(FILE *out,char *s) {
  char c ;
  while ((c = *s++)) emit_char(out,c);
}


/* Returns number of written chars */
#define TRU(N,T) \
static int N(char *dst,int idx,T x) {\
  int n = 0 ;\
  while (x > 0)  {\
    dst[idx--] = '0' + (x % 10) ;  n++ ;\
    x /= 10 ;\
    if (idx < 0) return n ;\
  }\
  if (n == 0) {\
    dst[idx--] = '0' ;\
    n++ ;\
  }\
  return n ;\
}
  
TRU(tr_int_uns,unsigned int)
TRU(tr_int32_uns,uint32_t)
TRU(tr_int64_uns,uint64_t)

#define TRX(N,T) \
static int N(char *dst,int idx,T x) {\
  int n = 0 ;\
  while (x > 0)  {\
    int d = x % 16 ;\
    char c = d  < 10 ? '0' + d : 'a' + d - 10 ;\
    dst[idx--] = c ;  n++ ;\
    x /= 16 ;\
    if (idx < 0) return n ;\
  }\
  if (n == 0) {\
    dst[idx--] = '0' ;\
    n++ ;\
  }\
  return n ;\
}

TRX(tr_int_hex,unsigned int)
TRX(tr_int32_hex,uint32_t)
TRX(tr_int64_hex,uint64_t)

/* int */
static const int sz = 32 ;

#define EMITU(N,T,TR)                             \
void N(FILE *out,T x) {\
  char buff[sz] ;\
  int n = TR(buff,sz-1,x) ;\
  char *p = &buff[sz-n] ;\
  for ( ; n > 0 ; n--) {\
    emit_char(out,*p++) ;\
  }\
}

#define EMIT(N,T,E) \
void N(FILE *out,T x) {\
  if (x < 0) {\
    emit_char(out,'-') ;\
    x = -x ;\
  }\
  E(out,x) ;\
}

EMITU(emit_int_uns,unsigned int,tr_int_uns)
EMITU(emit_int_hex,unsigned int,tr_int_hex)
EMIT(emit_int,int,emit_int_uns)

EMITU(emit_int32_uns,uint32_t,tr_int32_uns)
EMITU(emit_int32_hex,uint32_t,tr_int32_hex)
EMIT(emit_int32,int32_t,emit_int32_uns)

EMITU(emit_int64_uns,uint64_t,tr_int64_uns)
EMITU(emit_int64_hex,uint64_t,tr_int64_hex)
EMIT(emit_int64,int64_t,emit_int64_uns)


static void emit_nchars(FILE *out,char c,int n) {
  while (n-- > 0) emit_char(out,c) ;
}

#define EMITPADU(N,T,TR) \
void N(FILE *out,char pc,int pval,T x) {\
  int p = pval < 0 ? -pval : pval ;\
  int msz = sz < pval ? pval : sz ;\
  char buff[msz] ;\
  int n = TR(buff,msz-1,x) ;\
  if (pval > 0 && n < p) emit_nchars(out,pc,p-n) ;\
  char *q = &buff[msz-n] ;\
  for (int k = n ; k > 0 ; k--) emit_char(out,*q++) ;\
  if (pval < 0 && n < p) emit_nchars(out,pc,p-n) ;\
}

#define EMITPAD(N,T,TR) \
void N(FILE *out,char pc,int pval,T x) {\
  int p = pval < 0 ? -pval : pval ;\
  int msz = sz < pval ? pval : sz ;\
  T y = x < 0 ? -x : x ;\
  char buff[msz] ;\
  int n = TR(buff,msz-1,y) ;\
  if (x < 0) n++ ;\
  if (pval > 0 && n < p) emit_nchars(out,pc,p-n) ;\
  char *q = &buff[msz-n] ;\
  if (x < 0) emit_char(out,'-') ;\
  for (int k = n ; k > 0 ; k--) emit_char(out,*q++) ;\
  if (pval < 0 && n < p) emit_nchars(out,pc,p-n) ;\
}

EMITPADU(emit_pad_int_uns,unsigned int,tr_int_uns)
EMITPADU(emit_pad_int_hex,unsigned int,tr_int_hex)
EMITPAD(emit_pad_int,int,tr_int_uns)

EMITPADU(emit_pad_int32_uns,uint32_t,tr_int32_uns)
EMITPADU(emit_pad_int32_hex,uint32_t,tr_int32_hex)
EMITPAD(emit_pad_int32,int32_t,tr_int32_uns)

EMITPADU(emit_pad_int64_uns,uint64_t,tr_int64_uns)
EMITPADU(emit_pad_int64_hex,uint64_t,tr_int64_hex)
EMITPAD(emit_pad_int64,int64_t,tr_int64_uns)

void emit_double(FILE *out,double f) {
  double g = f < 0.0 ? -f : f ;
  g *= 100.0 ; g += 0.5 ;
  uint64_t x = g ;
  if (f < 0.0) emit_char(out,'-') ;
  emit_int64_uns(out,x / 100) ;
  emit_char(out,'.') ;
  emit_pad_int_uns(out,'0',2,x % 100) ;
}
