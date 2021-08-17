/**************************/
/* Setup user mode stacks */
/**************************/

#define USER_MODE 1

static void back_to_el1(struct pt_regs *regs,unsigned int esr) {
  regs->pstate |= 0b0101;
}

static uint64_t user_stack[AVAIL];

static void set_user_stack(int cpu) {
  uint64_t sp_usr = (uint64_t)thread_stack_alloc();
  sp_usr &= (~15UL); /* stack ptr needs 16-byte alignment */
  //  printf("Cpu %d has stack 0x%" PRIx64 "\n",cpu,sp_usr);
  struct thread_info *ti0 = current_thread_info();
  struct thread_info *ti = thread_info_sp(sp_usr);
  thread_info_init(ti, TIF_USER_MODE);
  ti->pgtable = ti0->pgtable;
  ti->exception_handlers[EL0_SYNC_64][ESR_EL1_EC_SVC64] = back_to_el1;
  ti0->exception_handlers[EL0_SYNC_64][ESR_EL1_EC_SVC64] = back_to_el1;
  user_stack[cpu] = sp_usr;
}
