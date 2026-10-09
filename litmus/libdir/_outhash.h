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

/* Notice: this file contains public domain code by Bob Jenkins */

#ifndef _OUTHASH_H
#define _OUTHASH_H 1

#include <stddef.h>
#include "presi_count.h"
#include "presi_io.h"

/********************************/
/* Abstract type of hash tables */
/********************************/

typedef struct {
  count_t c ;
  int ok ;
} outhash_entry_t ;

typedef struct {
  outhash_entry_t *t ;
  uint32_t *keys ;
} outhash_mem_t ;

typedef struct {
  size_t hash_sz,key_sz ; /* static sizes  */
  int nhash ; /* number of active entries */
  outhash_mem_t mem ;
} outhash_t ;

/**********************************/
/* Hastable usual functionalities */
/**********************************/

/* Initialise one key */
void outhash_init_key(uint32_t *key, size_t sz) ;

/* Initialise, static sizes and pointers to memory are correct at call time */
void outhash_init(outhash_t *t) ;

int outhash_add(outhash_t *t,uint32_t *key, count_t c,int ok) ;

int outhash_adds(outhash_t *t, outhash_t *f) ;

typedef
  void outhash_dump_entry(FILE *chan, outhash_entry_t *p, uint32_t *key) ;

void outhash_dump (FILE *chan, outhash_dump_entry *dump_entry, outhash_t *t) ;

#endif
