/****************************************************************************/
/*                           the diy toolsuite                              */
/*                                                                          */
/* Jade Alglave, University College London, UK.                             */
/* Luc Maranget, INRIA Paris-Rocquencourt, France.                          */
/*                                                                          */
/* Copyright 2026-present Institut National de Recherche en Informatique et */
/* en Automatique and the authors. All rights reserved.                     */
/*                                                                          */
/* This software is governed by the CeCILL-B license under French law and   */
/* abiding by the rules of distribution of free software. You can use,      */
/* modify and/ or redistribute the software under the terms of the CeCILL-B */
/* license as circulated by CEA, CNRS and INRIA at the following URL        */
/* "http://www.cecill.info". We also give a copy in LICENSE.txt.            */
/****************************************************************************/

#include "outhash.h"

/*
 Bob Jenkins hashword function, (Public domain)
 See <http://burtleburtle.net/bob/hash/doobs.html>
*/

#define rot(x,k) (((x)<<(k)) | ((x)>>(32-(k))))

#define mix(a,b,c) \
{ \
  a -= c;  a ^= rot(c, 4);  c += b; \
  b -= a;  b ^= rot(a, 6);  a += c; \
  c -= b;  c ^= rot(b, 8);  b += a; \
  a -= c;  a ^= rot(c,16);  c += b; \
  b -= a;  b ^= rot(a,19);  a += c; \
  c -= b;  c ^= rot(b, 4);  b += a; \
}

#define final(a,b,c) \
{ \
  c ^= b; c -= rot(b,14); \
  a ^= c; a -= rot(c,11); \
  b ^= a; b -= rot(a,25); \
  c ^= b; c -= rot(b,16); \
  a ^= c; a -= rot(c,4);  \
  b ^= a; b -= rot(a,14); \
  c ^= b; c -= rot(b,24); \
}

static uint32_t hashword(
const uint32_t *k,                   /* the key, an array of uint32_t values */
size_t          length)              /* the length of the key, in uint32_ts */
{
  uint32_t a,b,c;

  /* Set up the internal state */
  a = b = c = 0xdeadbeef ;

  /*------------------------------------------------- handle most of the key */
  while (length > 3)
  {
    a += k[0];
    b += k[1];
    c += k[2];
    mix(a,b,c);
    length -= 3;
    k += 3;
  }

  /*------------------------------------------- handle the last 3 uint32_t's */
  switch(length)                     /* all the case statements fall through */
  {
  case 3 : c+=k[2];
  case 2 : b+=k[1];
  case 1 : a+=k[0];
    final(a,b,c);
  case 0:     /* case 0: nothing left to add */
    break;
  }
  /*------------------------------------------------------ report the result */
  return c;
}

void outhash_init_key(uint32_t *p,size_t sz) {
  for (int k=0 ; k < sz ; k++) *p++ = -1;
}

void outhash_init(outhash_t *t) {
  t->nhash = 0 ;
  for (int k=0 ; k < t->hash_sz ; k++) t->mem.t[k].c = 0 ;
  int sz = t->hash_sz * t->key_sz ;
  outhash_init_key(t->mem.keys,sz) ;
}

inline static uint32_t *get_key(outhash_t *t,int k) {
  return t->mem.keys + k*t->key_sz ;
}

inline static int eq_key(uint32_t *p, uint32_t *q, size_t sz) {
  for (int k = 0 ; k < sz ; k++)
    if (*p++ != *q++) return 0 ;
  return 1 ;
}

inline static void copy_key(uint32_t *d, uint32_t *p, size_t sz) {
  for (int k = 0 ; k < sz ; k++) *d++ = *p++ ;
}

int outhash_add(outhash_t *t, uint32_t *key, count_t c, int ok) {
  uint32_t h = hashword(key,t->key_sz) ;
  h = h % t->hash_sz ;
  for (int k = 0 ; k < t->hash_sz ;  k++) {
    outhash_entry_t *p = t->mem.t + h ;
    if (p->c == 0) { /* New entry */
      copy_key(get_key(t,h),key,t->key_sz);
      p->c = c ;
      p->ok = ok ;
      t->nhash++ ;
      return 1 ;
    } else if (eq_key(key,get_key(t,h),t->key_sz)) {
      p->c += c ;
      return 1 ;
    }
    h++ ;
    h %= t->hash_sz ;
  }
  return 0 ;
}

int outhash_adds(outhash_t *t, outhash_t *f) {
  int r = 1;
  for (int k = 0 ; k < t->hash_sz ; k++) {
    outhash_entry_t *p = f->mem.t+k ;
    if (p->c > 0) {
      uint32_t *key = get_key(f,k) ;
      int rloc = outhash_add(t,key,p->c,p->ok) ;
      r = r && rloc ;
    }
  }
  return r ;
}

void outhash_dump(FILE *chan, outhash_dump_entry *dump_entry, outhash_t *t) {
  for (int k = 0 ; k < t->hash_sz ; k++) {
    outhash_entry_t *e = t->mem.t + k ;
    if (e->c > 0) {
      uint32_t *key = get_key(t,k) ;
      dump_entry(chan,e,key) ;
    }
  }
}
