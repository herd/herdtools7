/****************************************************************************/
/*                           the diy toolsuite                              */
/*                                                                          */
/* Jade Alglave, University College London, UK.                             */
/* Luc Maranget, INRIA Paris-Rocquencourt, France.                          */
/*                                                                          */
/* Copyright 2019-present Institut National de Recherche en Informatique et */
/* en Automatique and the authors. All rights reserved.                     */
/*                                                                          */
/* This software is governed by the CeCILL-B license under French law and   */
/* abiding by the rules of distribution of free software. You can use,      */
/* modify and/ or redistribute the software under the terms of the CeCILL-B */
/* license as circulated by CEA, CNRS and INRIA at the following URL        */
/* "http://www.cecill.info". We also give a copy in LICENSE.txt.            */
/****************************************************************************/
/* Authors:                                                                 */
/* Nikos Nikoleris, Arm Limited.                                            */
/****************************************************************************/
#ifndef KVM_SELF_H
#define KVM_SELF_H 1

#include <stdint.h>

void litmus_icache_sync(uintptr_t vaddr, uintptr_t vaddr_end);

size_t code_size(ins_t *p,int skip);

void litmus_pte_unset_el0(uintptr_t vaddr, uintptr_t vaddr_end);

void code_init(void *code, void *src, size_t sz);
#endif
