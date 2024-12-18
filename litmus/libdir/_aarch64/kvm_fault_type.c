#define MAX_FAULTS_PER_THREAD 8

enum fault_type_t {
  FaultUndefinedInstruction,
  FaultSupervisorCall,
  FaultMMUAddressSize,
  FaultMMUTranslation,
  FaultMMUAccessFlag,
  FaultMMUPermission,
  FaultTagCheck,
  FaultUnsupported,
  FaultUnknown,
  FaultTypes,
};

static const char *fault_type_names[] = {
  "UndefinedInstruction",
  "SupervisorCall",
  "MMU:AddressSize",
  "MMU:Translation",
  "MMU:AccessFlag",
  "MMU:Permission",
  "TagCheck",
  "Unsupported",
};

static enum fault_type_t get_fault_type(unsigned long esr)
{
  unsigned int ec;
  unsigned int dfsc;
  int fault_type;

  ec = esr >> ESR_EL1_EC_SHIFT;
  if (ec == ESR_EL1_EC_UNKNOWN) {
    return FaultUndefinedInstruction;
  } else if (ec == ESR_EL1_EC_SVC64) {
    return FaultSupervisorCall;
  } else {
    dfsc = esr & 0x3fU;
    fault_type = (dfsc >> 2) + FaultMMUAddressSize;
    if (fault_type > FaultTagCheck)
      return FaultUnsupported;
    else
      return fault_type;
  }
}

typedef struct {
  int instr_symb;
  int data_symb;
  enum fault_type_t type;
} fault_info_t;

typedef struct {
  fault_info_t faults[MAX_FAULTS_PER_THREAD];
  int n;
} th_faults_info_t;

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

  int i1 = 0, i2 = 0;
  while (i1 < th_flts1->n) {
    fault_info_t *f1 = &th_flts1->faults[i1];
    fault_info_t *f2 = &th_flts2->faults[i2];
    if (i2 == th_flts2->n)
      return 0;

    if (f1->instr_symb == f2->instr_symb && f1->data_symb == f2->data_symb &&
        f1->type == f2->type) {
      i1++;
      i2 = 0;
    } else {
      i2++;
    }
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

static bool fault_reported[NTHREADS][MAX_FAULTS_PER_THREAD];

static void pp_log_faults_init(void)
{
  for (int i = 0; i < NTHREADS; i++) {
    for (int j = 0; j < MAX_FAULTS_PER_THREAD; j++) {
      fault_reported[i][j] = false;
    }
  }
}

static void pp_log_faults(FILE *chan, th_faults_info_t *th_flts, int proc, int instr_symb,
                          int data_symb, int ftype)
{
  int found = 0;
  for (int i = 0; i < th_flts->n; i++) {
    fault_info_t *flt = &th_flts->faults[i];
    int cond = 1;
    if (instr_symb != INSTR_SYMB_ID_UNKNOWN) {
      cond &= flt->instr_symb == instr_symb;
    }
    if (data_symb != DATA_SYMB_ID_UNKNOWN) {
      cond &= flt->data_symb == data_symb;
    }
    if (ftype != FaultUnknown) {
      cond &= flt->type == ftype;
    }
    if (cond) {
      found = 1;
      printf(" ");

      if (!fault_reported[proc][i]) {
        pp_fault(proc, flt->instr_symb, flt->data_symb, flt->type);
        fault_reported[proc][i] = true;
      }
    }
  }
  if (!found) {
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

static int exists_fault(th_faults_info_t *th_flts, int instr_symb, int data_symb, int ftype)
{
  for (int i = 0; i < th_flts->n; i++) {
    fault_info_t *flt = &th_flts->faults[i];
    int cond = 1;
    if (instr_symb != INSTR_SYMB_ID_UNKNOWN) {
      cond &= flt->instr_symb == instr_symb;
    }
    if (data_symb != DATA_SYMB_ID_UNKNOWN) {
      cond &= flt->data_symb == data_symb;
    }
    if (ftype != FaultUnknown) {
      cond &= flt->type == ftype;
    }
    if (cond) {
      return 1;
    }
  }
  return 0;
}
