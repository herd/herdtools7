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
#ifndef _AFFINITY_H
#define _AFFINITY_H 1

#include "utils.h"

#ifdef CPUS_DEFINED
cpus_t *read_affinity(void) ;
#ifdef FORCE_AFFINITY
cpus_t *read_force_affinity(int n_avail, int verbose) ;
#endif
void write_affinity(cpus_t *p) ;
#endif

void write_one_affinity(int cpu) ;
#ifdef FORCE_AFFINITY
void force_one_affinity(int cpu, int sz, int verbose, char *name) ;
#endif

#endif
