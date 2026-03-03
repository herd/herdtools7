/****************************************************************************/
/*                           the diy toolsuite                              */
/*                                                                          */
/* Jade Alglave, University College London, UK.                             */
/* Luc Maranget, INRIA Paris, France.                                       */
/* Rémy Citérin, ARM Ltd, Cambridge, UK                                     */
/*                                                                          */
/* Copyright 2025-present Institut National de Recherche en Informatique et */
/* en Automatique and the authors. All rights reserved.                     */
/*                                                                          */
/* This software is governed by the CeCILL-B license under French law and   */
/* abiding by the rules of distribution of free software. You can use,      */
/* modify and/ or redistribute the software under the terms of the CeCILL-B */
/* license as circulated by CEA, CNRS and INRIA at the following URL        */
/* "http://www.cecill.info". We also give a copy in LICENSE.txt.            */
/****************************************************************************/
#ifndef KVM_FAULT_TYPE_H
#define KVM_FAULT_TYPE_H
#include <arm64/asm/esr.h>
#include <kvm_fault_define.h>
#include <../utils.h>

#ifndef INSTR_SYMB_ID_UNKNOWN
#define INSTR_SYMB_ID_UNKNOWN 0
#endif

#ifndef DATA_SYMB_ID_UNKNOWN
#define DATA_SYMB_ID_UNKNOWN 0
#endif

enum fault_type_t {
  FaultUndefinedInstruction,
  FaultSupervisorCall,
  FaultPacCheckIA,
  FaultPacCheckIB,
  FaultPacCheckDA,
  FaultPacCheckDB,
  FaultMMUAddressSize,
  FaultMMUTranslation,
  FaultMMUAccessFlag,
  FaultMMUPermission,
  FaultTagCheck,
  FaultUnsupported,
  FaultUnknown,
  FaultTypes,
};

#define ESR_EL1_EC_PAC 0b011100

enum fault_type_t get_fault_type(unsigned long esr);

typedef struct {
  int instr_symb;
  int data_symb;
  enum fault_type_t type;
} fault_info_t;

typedef struct {
  fault_info_t faults[MAX_FAULTS_PER_THREAD];
  int n;
} th_faults_info_t;

void th_faults_info_init(th_faults_info_t *th_flts);

int th_faults_info_compare(th_faults_info_t *th_flts1, th_faults_info_t *th_flts2);

void pp_fault(int proc, int instr_symb, int data_symb, int ftype, const char *instr_symb_name[], const char *data_symb_name[]);

void pp_log_faults_init(int nthreads, bool (*fault_reported)[MAX_FAULTS_PER_THREAD]);

void pp_log_faults(FILE *chan, th_faults_info_t *th_flts, int proc, int instr_symb,
                   int data_symb, int ftype, bool (*fault_reported)[MAX_FAULTS_PER_THREAD], const char *instr_symb_name[], const char *data_symb_name[]);

int eq_faults(th_faults_info_t *th_flts1, th_faults_info_t *th_flts2, int nthreads);

int exists_fault(th_faults_info_t *th_flts, int instr_symb, int data_symb, int ftype);
#endif
