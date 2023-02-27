/*********************/
/* Handle MMU faults */
/*********************/

static void fault_handler(struct pt_regs *regs,unsigned int esr) {
  struct thread_info *ti = current_thread_info();
  who_t *w = &whoami[ti->cpu];
  record_fault(w, read_elr_el1(), esr);
#ifdef PRECISE
  regs->pc = (u64)label_ret[w->proc];
#else
#ifdef FAULT_SKIP
  regs->pc += 4;
#endif
#endif
}

static void install_fault_handler(int cpu) {
  install_exception_handler(EL1H_SYNC, ESR_EL1_EC_DABT_EL1, fault_handler);
  install_exception_handler(EL1H_SYNC, ESR_EL1_EC_UNKNOWN, fault_handler);
#ifdef USER_MODE
  struct thread_info *ti = thread_info_sp(user_stack[cpu]);
  ti->exception_handlers[EL0_SYNC_64][ESR_EL1_EC_DABT_EL0] = fault_handler;
  ti->exception_handlers[EL0_SYNC_64][ESR_EL1_EC_UNKNOWN] = fault_handler;
#endif
}
