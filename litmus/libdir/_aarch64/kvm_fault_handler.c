/*********************/
/* Handle MMU faults */
/*********************/

static int whoami[AVAIL];
static vars_t *vars_ptr[AVAIL];
static int fault[NTHREADS][NVARS+1];

static void fault_handler(struct pt_regs *regs,unsigned int esr) {
  struct thread_info *ti = current_thread_info() ;
  int id = whoami[ti->cpu];
  assert(id >= 0 && id < NTHREADS);
  vars_t *v = vars_ptr[ti->cpu];
  void *p = read_far();
  int idx =  idx_addr(p,v);
  fault[id][idx]++;
}

static void install_fault_handler(void) {
  install_exception_handler(EL1H_SYNC, ESR_EL1_EC_DABT_EL1,fault_handler);
}

static void pp_faults(void) {
  int total = 0 ;
  for (int k = 0 ; k <  NTHREADS ; k++) {
    for (int m = 0 ; m < NVARS+1 ; m++) {
      total += fault[k][m];
    }
  }
  if (total > 0) {
    printf("Faults %d faults",total);
    for (int k=0 ; k < NTHREADS; k++) {
      for (int m = 0 ; m < NVARS+1 ; m++) {
        int f = fault[k][m];
        if (f > 0) {
          printf(" P%d,%s:%d",k,pretty_addr[m],f);
        }
      }
    }
    printf("\n");
  }
}
