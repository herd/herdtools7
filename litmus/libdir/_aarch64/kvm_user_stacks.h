/****************************************************************************/
/*                           the diy toolsuite                              */
/*                                                                          */
/* Jade Alglave, University College London, UK.                             */
/* Luc Maranget, INRIA Paris, France.                                       */
/* Rémy Citérin, ARM Ltd, Cambridge, UK                                     */
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
/**************************/
/* Setup user mode stacks */
/**************************/

#ifndef KVM_USER_STACKS_H
#define KVM_USER_STACKS_H 1
#define USER_MODE 1

static uint64_t user_stack[AVAIL];

static void set_user_stack(int cpu) {
  uint64_t sp_usr = (uint64_t)thread_stack_alloc();
  sp_usr &= (~15UL); /* stack ptr needs 16-byte alignment */
  //  printf("Cpu %d has stack 0x%" PRIx64 "\n",cpu,sp_usr);
  struct thread_info *ti0 = current_thread_info();
  struct thread_info *ti = thread_info_sp(sp_usr);
  thread_info_init(ti, TIF_USER_MODE);
  ti->pgtable = ti0->pgtable;
  user_stack[cpu] = sp_usr;
}
#endif
