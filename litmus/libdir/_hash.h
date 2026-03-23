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
#ifndef _HASH_H
#define _HASH_H 1
#include <../utils.h>
#ifndef KVM
#include <stdio.h>
#endif
#include <log.h>
#include <param.h>
#include <count.h>
#include <stdint.h>
#include <stddef.h>

typedef struct {
  log_t *key ;
  param_t p ;
  count_t c ;
  int ok ;
} entry_t ;

typedef struct {
  int nhash ;
  int hashsz ;
  entry_t *t ;
} hash_t ;

void pp_hash(FILE *fp,hash_t *t,int verbose,const char **group, void (*pp_entry)(FILE *, entry_t *, int, const char **));

void log_init(log_t *p, size_t log_t_size);

void hash_init(hash_t *t, int hashsz, entry_t *hash, size_t log_t_size);

uint32_t hash_log (log_t *key, size_t log_t_size);

int hash_add(hash_t *t,log_t *key, param_t *v,count_t c,int ok, int (*eq_log)(log_t *, log_t *), size_t log_t_size);

int hash_adds(hash_t *t, hash_t *f, int (*eq_log)(log_t *, log_t *), size_t log_t_size);
#endif
