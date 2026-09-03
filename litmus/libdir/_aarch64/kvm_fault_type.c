#define MAX_FAULTS_PER_THREAD 8

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
  FaultDMMUTranslation,
  FaultDMMUAccessFlag,
  FaultDMMUPermission,
  FaultDMMUExclusive,
  FaultIMMUTranslation,
  FaultIMMUAccessFlag,
  FaultIMMUPermission,
  FaultIMMUExclusive,
  FaultTagCheck,
  FaultUnsupported,
  FaultUnknown,
  FaultTypes,
};

static const char *fault_type_names[] = {
  "UndefinedInstruction",
  "SupervisorCall",
  "PacCheck:IA",
  "PacCheck:IB",
  "PacCheck:DA",
  "PacCheck:DB",
  "MMU:AddressSize",
  "MMU:Translation",
  "MMU:AccessFlag",
  "MMU:Permission",
  "D-MMU:Translation",
  "D-MMU:AccessFlag",
  "D-MMU:Permission",
  "D-MMU:Exclusive",
  "I-MMU:Translation",
  "I-MMU:AccessFlag",
  "I-MMU:Permission",
  "I-MMU:Exclusive",
  "TagCheck",
  "Unsupported",
};

static enum fault_type_t get_fault_type(unsigned long esr)
{
  unsigned int ec = esr >> ESR_EL1_EC_SHIFT;
  unsigned int fsc;
  unsigned int fault_class;
  int domain = 0; /* 0 = unknown, 1 = data, 2 = instruction */

  switch (ec) {
  case ESR_EL1_EC_UNKNOWN:
    return FaultUndefinedInstruction;
  case ESR_EL1_EC_SVC64:
    return FaultSupervisorCall;
  case ESR_EL1_EC_PAC:
    return FaultPacCheckIA + (esr & 0x3U);
  case ESR_EL1_EC_DABT_EL0:
  case ESR_EL1_EC_DABT_EL1:
    domain = 1;
    break;
  case ESR_EL1_EC_IABT_EL0:
  case ESR_EL1_EC_IABT_EL1:
    domain = 2;
    break;
  default:
    break;
  }

  fsc = esr & 0x3fU;
  fault_class = fsc >> 2;

  switch (fault_class) {
  case 0:
    return FaultMMUAddressSize;
  case 1:
    if (domain == 1)
      return FaultDMMUTranslation;
    if (domain == 2)
      return FaultIMMUTranslation;
    return FaultMMUTranslation;
  case 2:
    if (domain == 1)
      return FaultDMMUAccessFlag;
    if (domain == 2)
      return FaultIMMUAccessFlag;
    return FaultMMUAccessFlag;
  case 3:
    if (domain == 1)
      return FaultDMMUPermission;
    if (domain == 2)
      return FaultIMMUPermission;
    return FaultMMUPermission;
  default:
    return FaultUnsupported;
  }
}

typedef struct {
  int instr_symb;
  int data_symb;
  enum fault_type_t type;
} fault_info_t;

static int compare_fault_info (fault_info_t *f1, fault_info_t *f2) {
  if (f1->instr_symb < f2->instr_symb) return -1;
  else if (f1->instr_symb > f2->instr_symb) return 1;
  else if (f1->data_symb < f2->data_symb) return -1;
  else if (f1->data_symb > f2->data_symb) return 1;
  else if (f1->type < f2->type) return -1;
  else if (f1->type > f2->type) return 1;
  else return 0;
}

typedef struct {
  fault_info_t faults[MAX_FAULTS_PER_THREAD];
  int n;
} th_faults_info_t;

static void do_insert(int k,fault_info_t *f,th_faults_info_t *t) {
  if (t->n >=  MAX_FAULTS_PER_THREAD) return;
  for (int i = t->n ; i > k ; i--) {
    t->faults[i] = t->faults[i-1];
  }
  t->n++;
  t->faults[k] = *f;
}

static void insert_fault(fault_info_t *f,th_faults_info_t *t) {
  for (int k=0 ; k < t->n ; k++) {
    int r = compare_fault_info(f,&t->faults[k]);
    if (r < 0) { // Insert here
      do_insert(k,f,t);
      return;
    } else if (r == 0) { // Already here
      return;
    }
  }
  do_insert(t->n,f,t);
}

static int match_fault_info(int instr_symb, int data_symb, int ftype, fault_info_t *flt) {
  int r = 1;
  if (instr_symb != INSTR_SYMB_ID_UNKNOWN) {
    r &= flt->instr_symb == instr_symb;
  }
  if (data_symb != DATA_SYMB_ID_UNKNOWN) {
    r &= flt->data_symb == data_symb;
  }
  if (ftype != FaultUnknown) {
    enum fault_type_t ft = flt->type;
    switch (ftype) {
    case  FaultMMUTranslation:
      r &=
        ft == FaultDMMUTranslation
        || ft ==  FaultIMMUTranslation
        || ft == ftype;
      break;
    case FaultMMUPermission:
      r &=
        ft == FaultDMMUPermission
        || ft ==  FaultIMMUPermission
        || ft == ftype;
      break;
    case FaultMMUAccessFlag:
      r &=
        ft == FaultDMMUAccessFlag
        || ft ==  FaultIMMUAccessFlag
        || ft == ftype;
      break;
    default:
      r &= ft == ftype;
    }
  }
  return r;
}

static void th_faults_info_init(th_faults_info_t *th_flts)
{
  for (int i = 0; i < MAX_FAULTS_PER_THREAD; i++) {
    fault_info_t *f = &th_flts->faults[i];
    f->instr_symb = INSTR_SYMB_ID_UNKNOWN;
    f->data_symb = DATA_SYMB_ID_UNKNOWN;
    f->type = FaultUnknown;
  }
  th_flts->n = 0;
}

static int th_faults_info_compare(th_faults_info_t *th_flts1, th_faults_info_t *th_flts2)
{
  if (th_flts1->n != th_flts2->n)
    return 0;

  for (int k = 0 ; k < th_flts1->n ; k++) {
    fault_info_t *f1 = &th_flts1->faults[k];
    fault_info_t *f2 = &th_flts2->faults[k];
    if (compare_fault_info(f1,f2)) return 0;
  }
  return 1;
}

static void pp_fault(int proc, int instr_symb, int data_symb, int ftype)
{
  if (instr_symb != INSTR_SYMB_ID_UNKNOWN)
    printf("fault(P%s", instr_symb_name[instr_symb]);
  else
    printf("fault(P%d", proc);
  if (data_symb != DATA_SYMB_ID_UNKNOWN)
    printf(",%s", data_symb_name[data_symb]);
  if (ftype != FaultUnknown)
    printf(",%s", fault_type_names[ftype]);
  printf(");");
}

static void pp_positive_faults(th_faults_info_t *p) {
  for (int proc=0; proc < NTHREADS; proc++) {
    th_faults_info_t *t = &p[proc];
    for (int k=0; k < t->n; k++) {
      fault_info_t *f = &t->faults[k];
      printf(" ");
      pp_fault(proc, f->instr_symb, f->data_symb, f->type);
    }
  }
}

static int match_some_fault_info
  (int instr_symb, int data_symb, enum fault_type_t ftype, th_faults_info_t *t)
{
  for (int k=0 ; k < t->n ; k++) {
    if (match_fault_info(instr_symb, data_symb, ftype,&t->faults[k]))
      return 1;
  }
  return 0;
}

static void pp_negative_fault
  (th_faults_info_t *t, int proc, int instr_symb, int data_symb, enum fault_type_t ftype)
{
  if (!match_some_fault_info(instr_symb, data_symb, ftype, t)) {
    printf(" ~");
    pp_fault(proc, instr_symb, data_symb, ftype);
  }
}

static int eq_faults(th_faults_info_t *th_flts1, th_faults_info_t *th_flts2)
{
  for (int i = 0; i < NTHREADS; i++) {
    if (!th_faults_info_compare(&th_flts1[i], &th_flts2[i]))
      return 0;
  }
  return 1;
}

