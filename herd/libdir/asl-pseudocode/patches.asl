


// GenMPAMatEL()
// =============
// Returns MPAMinfo for the specified EL.
// May be called if MPAM is not implemented (but in an version that supports
// MPAM), MPAM is disabled, or in AArch32.  In AArch32, convert the mode to
// EL if can and use that to drive MPAM information generation.  If mode
// cannot be converted, MPAM is not implemented, or MPAM is disabled return
// default MPAM information for the current security state.

// MPAMinfo GenMPAMatEL(AccessType acctype, bits(2) el)
func GenMPAMatEL(acctype:: AccessType, el::bits(2)) => MPAMinfo
begin
  return UNKNOWN :: MPAMinfo;
end

getter TSTATE[] => TMState
begin
  return UNKNOWN :: TMState;
end


// IsAligned

func IsAligned(x :: bits(N), y::integer) => boolean
begin
  return TRUE;
end

func IsAligned(x::integer, y::integer) => boolean
begin
  return TRUE;
end

// BigEndian

func BigEndian(acctype:: AccessType) => boolean
begin
  return FALSE;
end

func IsFault(addrdesc:: AddressDescriptor) => boolean
begin
  return FALSE;
end

func AArch64_TranslateAddress(address::bits(64), accdesc::AccessDescriptor, aligned::boolean, size::integer) => AddressDescriptor
begin
  return address;
end

func ELStateUsingAArch32K(el::bits(2), secure::boolean) => (boolean, boolean)
begin
    return (TRUE, FALSE);
end

// -----------------------------------------------------------------------------
//
//                              MemSingle
//
// -----------------------------------------------------------------------------

// Those functions have been translated to v1 automatically and patched. The
// lines that differ are duplicated and commented.

setter AArch64_MemSingle [
    address::bits(64),
    size::integer,
    accdesc_in::AccessDescriptor,
    aligned::boolean,
    ispair::boolean]
    = value::bits((size * 8))
begin
  assert size IN {1, 2, 4, 8, 16};
  accdesc :: AccessDescriptor = accdesc_in;
  if HaveLSE2Ext() then
    assert AllInAlignedQuantity(address, size, 16);
  else
    assert IsAligned(address, size);
  end
  if (HaveMTE2Ext() && accdesc.tagchecked) then
    accdesc.tagchecked = AArch64_AccessIsTagChecked(address, accdesc);
  end
  memaddrdesc = UNKNOWN :: AddressDescriptor;
  memaddrdesc = AArch64_TranslateAddress(address, accdesc, aligned, size);
  if IsFault(memaddrdesc) then AArch64_Abort(address, memaddrdesc.fault); end

  // if (memaddrdesc.memattrs.shareability != Shareability_NSH) then
  if (FALSE && (memaddrdesc.memattrs.shareability != Shareability_NSH)) then
    ClearExclusiveByAddress(memaddrdesc.paddress, ProcessorID(), size);
  end
  if HaveTME() then
    if (accdesc.transactional
          && (! MemHasTransactionalAccess(memaddrdesc.memattrs))) then
      FailTransaction(TMFailure_IMP, FALSE);
    end
  end
  if (HaveMTE2Ext() && accdesc.tagchecked) then
    ptag :: bits(4) = AArch64_PhysicalTag(address);
    if (! AArch64_CheckTag(memaddrdesc, accdesc, ptag)) then
      AArch64_TagCheckFault(address, accdesc);
    end
  end
  if SPESampleInFlight then
    is_load :: boolean = FALSE;
    SPESampleLoadStore(is_load, accdesc, memaddrdesc);
  end
  memstatus = UNKNOWN :: PhysMemRetStatus;
  atomic = UNKNOWN :: boolean;
  // if (((memaddrdesc.memattrs.memtype == MemType_Normal)
  if (((FALSE && (memaddrdesc.memattrs.memtype == MemType_Normal))
         && (memaddrdesc.memattrs.inner.attrs == MemAttr_WB))
        && (memaddrdesc.memattrs.outer.attrs == MemAttr_WB)) then
    atomic = TRUE;
  else
    if ((((accdesc.exclusive || accdesc.atomicop) || accdesc.acqsc)
           || accdesc.acqpc) || accdesc.relsc) then
      if ((! aligned)
            && (! ConstrainUnpredictableBool(Unpredictable_MISALIGNEDATOMIC))) then
        AArch64_Abort(address, AlignmentFault(accdesc));
      else
        atomic = TRUE;
      end
    else
      if aligned then atomic = (! ispair); else atomic = TRUE; end
    end
  end
  if atomic then
    memstatus = PhysMemWrite(memaddrdesc, size, accdesc, value);
    if IsFault(memstatus) then
      HandleExternalWriteAbort(memstatus, memaddrdesc, size, accdesc);
    end
  else
    if (aligned && ispair) then
      assert size IN {8, 16};
      halfsize = (size DIV 2);
      lowhalf = UNKNOWN :: bits((halfsize * 8));
      highhalf = UNKNOWN :: bits((halfsize * 8));
      - = value;
      memstatus = PhysMemWrite(memaddrdesc, halfsize, accdesc, lowhalf);
      if IsFault(memstatus) then
        HandleExternalWriteAbort(memstatus, memaddrdesc, halfsize, accdesc);
      end
      memaddrdesc.paddress.address = (memaddrdesc.paddress.address + halfsize);
      memstatus = PhysMemWrite(memaddrdesc, halfsize, accdesc, highhalf);
      if IsFault(memstatus) then
        HandleExternalWriteAbort(memstatus, memaddrdesc, halfsize, accdesc);
      end
    end
  end
  return;
end

setter AArch64_MemSingle [
    address::bits(64),
    size::integer,
    accdesc::AccessDescriptor,
    aligned::boolean]
    = value::bits((size * 8))
begin
  ispair :: boolean = FALSE;
  AArch64_MemSingle[address, size, accdesc, aligned, ispair] = value;
  return;
end

getter AArch64_MemSingle [
    address::bits(64),
    size::integer,
    accdesc_in::AccessDescriptor,
    aligned::boolean,
    ispair::boolean]
  => bits((size * 8))
begin
  assert size IN {1, 2, 4, 8, 16};
  value = UNKNOWN :: bits((size * 8));
  accdesc :: AccessDescriptor = accdesc_in;
  if HaveLSE2Ext() then
    assert AllInAlignedQuantity(address, size, 16);
  else
    assert IsAligned(address, size);
  end
  if (HaveMTE2Ext() && accdesc.tagchecked) then
    accdesc.tagchecked = AArch64_AccessIsTagChecked(address, accdesc);
  end
  memaddrdesc = UNKNOWN :: AddressDescriptor;
  memaddrdesc = AArch64_TranslateAddress(address, accdesc, aligned, size);
  if IsFault(memaddrdesc) then AArch64_Abort(address, memaddrdesc.fault); end
  if HaveTME() then
    if (accdesc.transactional
          && (! MemHasTransactionalAccess(memaddrdesc.memattrs))) then
      FailTransaction(TMFailure_IMP, FALSE);
    end
  end
  if (HaveMTE2Ext() && accdesc.tagchecked) then
    ptag :: bits(4) = AArch64_PhysicalTag(address);
    if (! AArch64_CheckTag(memaddrdesc, accdesc, ptag)) then
      AArch64_TagCheckFault(address, accdesc);
    end
  end
  if SPESampleInFlight then
    is_load :: boolean = TRUE;
    SPESampleLoadStore(is_load, accdesc, memaddrdesc);
  end
  atomic = UNKNOWN :: boolean;
  // if (((memaddrdesc.memattrs.memtype == MemType_Normal)
  if (((FALSE && (memaddrdesc.memattrs.memtype == MemType_Normal))
         && (memaddrdesc.memattrs.inner.attrs == MemAttr_WB))
        && (memaddrdesc.memattrs.outer.attrs == MemAttr_WB)) then
    atomic = TRUE;
  else
    if ((((accdesc.exclusive || accdesc.atomicop) || accdesc.acqsc)
           || accdesc.acqpc) || accdesc.relsc) then
      if ((! aligned)
            && (! ConstrainUnpredictableBool(Unpredictable_MISALIGNEDATOMIC))) then
        AArch64_Abort(address, AlignmentFault(accdesc));
      else
        atomic = TRUE;
      end
    else
      if aligned then atomic = (! ispair); else atomic = TRUE; end
    end
  end
  memstatus = UNKNOWN :: PhysMemRetStatus;
  if atomic then
    ( memstatus, value ) = PhysMemRead(memaddrdesc, size, accdesc);
    if IsFault(memstatus) then
      HandleExternalReadAbort(memstatus, memaddrdesc, size, accdesc);
    end
  else
    if (aligned && ispair) then
      assert size IN {8, 16};
      halfsize = (size DIV 2);
      lowhalf = UNKNOWN :: bits((halfsize * 8));
      highhalf = UNKNOWN :: bits((halfsize * 8));
      ( memstatus, lowhalf ) = PhysMemRead(memaddrdesc, halfsize, accdesc);
      if IsFault(memstatus) then
        HandleExternalReadAbort(memstatus, memaddrdesc, halfsize, accdesc);
      end
      memaddrdesc.paddress.address = (memaddrdesc.paddress.address + halfsize);
      ( memstatus, highhalf ) = PhysMemRead(memaddrdesc, halfsize, accdesc);
      if IsFault(memstatus) then
        HandleExternalReadAbort(memstatus, memaddrdesc, halfsize, accdesc);
      end
      value = [highhalf, lowhalf];
    end
  end
  return value;
end

getter AArch64_MemSingle [
    address::bits(64),
    size::integer,
    accdesc::AccessDescriptor,
    aligned::boolean]
  => bits((size * 8))
begin
  ispair :: boolean = FALSE;
  return AArch64_MemSingle[address, size, accdesc, aligned, ispair];
end


// -----------------------------------------------------------------------------
//
//                                MemAtomic
//
// -----------------------------------------------------------------------------

func MemAtomic (
    address::bits(64),
    cmpoperand::bits(size),
    operand::bits(size),
    accdesc_in::AccessDescriptor)
  => bits(size)
begin
  assert accdesc_in.atomicop;
  bytes :: integer = (size DIV 8);
  assert bytes IN {1, 2, 4, 8, 16};
  newvalue = UNKNOWN :: bits(size);
  oldvalue = UNKNOWN :: bits(size);
  accdesc :: AccessDescriptor = accdesc_in;
  aligned :: boolean = IsAligned(address, bytes);
  if (HaveMTE2Ext() && accdesc.tagchecked) then
    accdesc.tagchecked = AArch64_AccessIsTagChecked(address, accdesc);
  end
  if ((! aligned) && AArch64_UnalignedAccessFaults(accdesc, address, bytes)) then
    AArch64_Abort(address, AlignmentFault(accdesc));
  end
  memaddrdesc :: AddressDescriptor = AArch64_TranslateAddress(address,
                                       accdesc, aligned, size);
  if IsFault(memaddrdesc) then AArch64_Abort(address, memaddrdesc.fault); end
  if (FALSE && (memaddrdesc.memattrs.shareability != Shareability_NSH)) then
    ClearExclusiveByAddress(memaddrdesc.paddress, ProcessorID(), size);
  end
  if ((HaveMTE2Ext() && accdesc.tagchecked)
        && ((! HaveMTEStoreOnlyExt()) || (! StoreOnlyTagCheckingEnabled()))) then
    ptag :: bits(4) = AArch64_PhysicalTag(address);
    if (! AArch64_CheckTag(memaddrdesc, accdesc, ptag)) then
      AArch64_TagCheckFault(address, accdesc);
    end
  end
  memstatus = UNKNOWN :: PhysMemRetStatus;
  ( memstatus, oldvalue ) = PhysMemRead(memaddrdesc, bytes, accdesc);
  if IsFault(memstatus) then
    HandleExternalReadAbort(memstatus, memaddrdesc, bytes, accdesc);
  end
  if BigEndian(accdesc.acctype) then
    oldvalue = BigEndianReverse(oldvalue);
  end
  cmpfail :: boolean = FALSE;
  case accdesc.modop of
    when MemAtomicOp_ADD: newvalue = (oldvalue + operand);
    when MemAtomicOp_BIC: newvalue = (oldvalue AND (NOT operand));
    when MemAtomicOp_EOR: newvalue = (oldvalue EOR operand);
    when MemAtomicOp_ORR: newvalue = (oldvalue OR operand);
    when MemAtomicOp_SMAX:
      newvalue = Max(SInt(oldvalue), SInt(operand))[(size - 1):0];
    when MemAtomicOp_SMIN:
      newvalue = Min(SInt(oldvalue), SInt(operand))[(size - 1):0];
    when MemAtomicOp_UMAX:
      newvalue = Max(UInt(oldvalue), UInt(operand))[(size - 1):0];
    when MemAtomicOp_UMIN:
      newvalue = Min(UInt(oldvalue), UInt(operand))[(size - 1):0];
    when MemAtomicOp_SWP: newvalue = operand;
    when MemAtomicOp_CAS:
      newvalue = operand; cmpfail = (cmpoperand != oldvalue);
  end
  if (HaveMTEStoreOnlyExt() && StoreOnlyTagCheckingEnabled()) then
    if (accdesc.tagchecked && cmpfail) then
      accdesc.tagchecked = ConstrainUnpredictableBool(Unpredictable_STOREONLYTAGCHECKEDCAS);
    end
    if (HaveMTE2Ext() && accdesc.tagchecked) then
      ptag :: bits(4) = AArch64_PhysicalTag(address);
      if (! AArch64_CheckTag(memaddrdesc, accdesc, ptag)) then
        accdesc.read = FALSE; AArch64_TagCheckFault(address, accdesc);
      end
    end
  end
  if (! cmpfail) then
    if BigEndian(accdesc.acctype) then
      newvalue = BigEndianReverse(newvalue);
    end
    memstatus = PhysMemWrite(memaddrdesc, bytes, accdesc, newvalue);
    if IsFault(memstatus) then
      HandleExternalWriteAbort(memstatus, memaddrdesc, bytes, accdesc);
    end
  end
  if SPESampleInFlight then
    is_load :: boolean = FALSE;
    SPESampleLoadStore(is_load, accdesc, memaddrdesc);
  end
  return oldvalue;
end
