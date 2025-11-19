/*********************/
/* Handle MMU faults */
/*********************/

#ifdef SEE_FAULTS
static bool is_iabt(unsigned long esr) {
  unsigned int ec = esr >> ESR_EL1_EC_SHIFT;

  return (ec == ESR_EL1_EC_IABT_EL1) || (ec == ESR_EL1_EC_IABT_EL0);

}
#endif

static void record_fault(who_t *w, unsigned long pc, unsigned long esr) {
#ifdef SEE_FAULTS
  th_faults_info_t *th_flts = &th_faults[w->instance][w->proc];
  labels_t *lbls = &vars_ptr[w->instance]->labels;
  fault_info_t flt;
  unsigned long far;

  flt.type = get_fault_type(esr);

  if (flt.type == FaultSupervisorCall) {
    // SVC, SMC and HVC return address points to the next instruction
    // after the exception generating instruction. For the purposes
    // of fault recording, we are interested in the instruction that
    // generated the fault.
    pc -= 4;
  }

  flt.instr_symb = get_instr_symb_id(lbls, (ins_t *)pc);
  if (!is_iabt(esr) && get_far(esr, &far)) {
    flt.data_symb = idx_addr((intmax_t *)far, vars_ptr[w->instance]);
  } else {
    flt.data_symb = DATA_SYMB_ID_UNKNOWN;
  }

  if (!log_fault(w->proc, flt.instr_symb, flt.data_symb, flt.type)) {
    return;
  }

  if (exists_fault(th_flts, flt.instr_symb, flt.data_symb, flt.type))
    return;

  if (th_flts->n < MAX_FAULTS_PER_THREAD) {
    th_flts->faults[th_flts->n++] = flt;
  }
#endif
}

static void fault_handler(struct pt_regs *regs,unsigned int esr) {
  struct thread_info *ti = current_thread_info();
  who_t *w = &whoami[ti->cpu];
  atomic_inc_fetch(&nfaults[w->proc]);

  record_fault(w, regs->pc, esr);
#ifdef PRECISE
  labels_t *lbls = &vars_ptr[w->instance]->labels;
  regs->pc = (u64)lbls->ret[w->proc];
#else
#ifdef FAULT_SKIP
  /*
   * In skip mode, ERET should branch to the instruction
   * that follows the faulting instruction.
   * As SVC already behaves that way, do not
   * increment pc when handler execution results
   * from executing SVC.
   */
  if (esr >> ESR_EL1_EC_SHIFT != ESR_EL1_EC_SVC64)
    regs->pc += 4;
#endif
#endif
}

#define ESR_EL1_EC_PAC 0b011100

static void install_fault_handler(int cpu) {
  install_exception_handler(EL1H_SYNC, ESR_EL1_EC_IABT_EL1, fault_handler);
  install_exception_handler(EL1H_SYNC, ESR_EL1_EC_DABT_EL1, fault_handler);
  install_exception_handler(EL1H_SYNC, ESR_EL1_EC_UNKNOWN, fault_handler);
  install_exception_handler(EL1H_SYNC, ESR_EL1_EC_SVC64, fault_handler);
  // Pointer Authentication Code error code is not present in KVM-unit-tests
  install_exception_handler(EL1H_SYNC, ESR_EL1_EC_PAC, fault_handler);
#ifdef USER_MODE
  struct thread_info *ti = thread_info_sp(user_stack[cpu]);
  ti->exception_handlers[EL0_SYNC_64][ESR_EL1_EC_IABT_EL0] = fault_handler;
  ti->exception_handlers[EL0_SYNC_64][ESR_EL1_EC_DABT_EL0] = fault_handler;
  ti->exception_handlers[EL0_SYNC_64][ESR_EL1_EC_UNKNOWN] = fault_handler;
  ti->exception_handlers[EL0_SYNC_64][ESR_EL1_EC_SVC64] = fault_handler;
  ti->exception_handlers[EL0_SYNC_64][ESR_EL1_EC_PAC] = fault_handler;
#endif
}
