static int fault[NTHREADS];

static void fault_handler(struct pt_regs *regs,unsigned int esr) {
  struct thread_info *ti = current_thread_info() ;
  int id = whoami[ti->cpu];
  assert(id >= 0 && id < NTHREADS);
  fault[id]++;
}

static void install_fault_handler(void) {
  install_exception_handler(EL1H_SYNC, ESR_EL1_EC_DABT_EL1,fault_handler);
}

static void pp_faults(void) {
  int total = 0 ;
  for (int k = 0 ; k <  NTHREADS ; k++) {
    total += fault[k];
  }
  if (total > 0) {
    printf("Faults: %d faults",total);
    for (int k=0 ; k < NTHREADS; k++) {
      int f = fault[k];
      if (f > 0) {
        printf(" P%d:%d",k,f);
      }
    }
    printf("\n");
  }
}
