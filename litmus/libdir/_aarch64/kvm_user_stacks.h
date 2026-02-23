/**************************/
/* Setup user mode stacks */
/**************************/

#ifndef KVM_USER_STACKS_H
#define KVM_USER_STACKS_H
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
