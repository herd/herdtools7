/****************************************************************************/
/*                           the diy toolsuite                              */
/*                                                                          */
/* Jade Alglave, University College London, UK.                             */
/* Luc Maranget, INRIA Paris-Rocquencourt, France.                          */
/*                                                                          */
/* Copyright 2015-present Institut National de Recherche en Informatique et */
/* en Automatique and the authors. All rights reserved.                     */
/*                                                                          */
/* This software is governed by the CeCILL-B license under French law and   */
/* abiding by the rules of distribution of free software. You can use,      */
/* modify and/ or redistribute the software under the terms of the CeCILL-B */
/* license as circulated by CEA, CNRS and INRIA at the following URL        */
/* "http://www.cecill.info". We also give a copy in LICENSE.txt.            */
/****************************************************************************/

/* Notice: this file contains public domain code by Bob Jenkins */

#include <_hash.h>

void pp_hash(FILE *fp,hash_t *t,int verbose,const char **group, void (*pp_entry)(FILE *, entry_t *, int, const char **)) {
  for (int k = 0 ; k < t->hashsz ; k++) {
    entry_t *p = t->t+k ;
    if (p->c > 0) {
      pp_entry(fp,p,verbose,group) ;
    }
  }
}

#if 0
void pp_hash_ok(FILE *fp,hash_t *t,char **group) {
  for (int k = 0 ; k < t->hashsz ; k++) {
    entry_t *p = t->t+k ;
    if (p->c > 0 && p->ok) pp_entry(fp,p,1,group) ;
  }
}
#endif

void log_init(log_t *p, size_t log_t_size) {
  uint32_t *q = (uint32_t *)p ;

  for (int k = log_t_size/sizeof(uint32_t) ; k > 0 ; k--)
    *q++ = -1 ;
}

void hash_init(hash_t *t, int hashsz, entry_t *hash, size_t log_t_size) {
  t->nhash = 0 ;
  t->hashsz = hashsz ;
  t->t = hash;
  for (int k = 0 ; k < hashsz ; k++) {
    t->t[k].c = 0 ;
    log_init(t->t[k].key, log_t_size) ;
  }
}

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

uint32_t hash_log (log_t *key, size_t log_t_size) {
  return hashword((uint32_t *)key,log_t_size/sizeof(uint32_t)) ;
}

int hash_add(hash_t *t,log_t *key, param_t *v,count_t c,int ok, int (*eq_log)(log_t *, log_t *), size_t log_t_size) {
  uint32_t h = hash_log(key, log_t_size) ;
  h = h % t->hashsz ;
  for (int k = 0 ; k < t->hashsz ;  k++) {
    entry_t *p = t->t + h ;
    if (p->c == 0) { /* New entry */
      p->key = key ;
      p->p = *v ;
      p->c = c ;
      p->ok = ok ;
      t->nhash++ ;
      return 1 ;
    } else if (eq_log(key,p->key)) {
      p->c += c ;
      return 1;
    }
    h++ ;
    h %= t->hashsz ;
  }
  return 0;
}

int hash_adds(hash_t *t, hash_t *f, int (*eq_log)(log_t *, log_t *), size_t log_t_size) {
  int r = 1;
  for (int k = 0 ; k < t->hashsz ; k++) {
    entry_t *p = f->t+k ;
    if (p->c > 0) {
      int rloc = hash_add(t,p->key,&p->p,p->c,p->ok, eq_log, log_t_size) ;
      r = r && rloc;
    }
  }
  return r;
}
