/*********************/
/* Handle MMU faults */
/*********************/

static void fault_handler(struct pt_regs *regs,unsigned int esr) {
  struct thread_info *ti = current_thread_info();
  record_fault(&whoami[ti->cpu],read_elr_el1(),read_far());
}

static void install_fault_handler(void) {
  install_exception_handler(EL1H_SYNC, ESR_EL1_EC_DABT_EL1,fault_handler);
}
