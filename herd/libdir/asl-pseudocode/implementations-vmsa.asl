
func SynchronizeContext()
begin
 return;
end;

func ThisInstrLength() => integer
begin
  return 32;
end;

func IsPhysicalSErrorPending() => boolean
begin
  return FALSE;
end;


// IsExternalAbortTakenSynchronously()
// ===================================
// Return an implementation specific value:
// TRUE if the fault returned for the access can be taken synchronously,
// FALSE otherwise.
//
// This might vary between accesses, for example depending on the error type
// or memory type being accessed.
// External aborts on data accesses and translation table walks on data accesses
// can be either synchronous or asynchronous.
//
// When FEAT_DoubleFault is not implemented, External aborts on instruction
// fetches and translation table walks on instruction fetches can be either
// synchronous or asynchronous.
// When FEAT_DoubleFault is implemented, all External abort exceptions on
// instruction fetches and translation table walks on instruction fetches
// must be synchronous.
// Luc: Dubious, FALSE or TRUE ?
// Luc: Dead code, never called

// type Fatal of exception {-};

//func
//  IsExternalAbortTakenSynchronously
//    (memstatus:PhysMemRetStatus,
//     iswrite:boolean,
//     desc:AddressDescriptor,
//     size:integer,
//     accdesc:AccessDescriptor) => boolean
//begin
//  __debug__(memstatus);
//  throw Fatal {-};
//  return FALSE;
//end;

// PendSErrorInterrupt()
// =====================
// Pend the SError Interrupt.

func PendSErrorInterrupt(fault:FaultRecord)
begin
  return;
end;


// WatchpointRelatedSyndrome()
// ===========================
// Update common Watchpoint related fields.

func
  WatchpointRelatedSyndrome(fault:FaultRecord,vaddress:bits(64))
    => bits(24)
begin
  return Zeros{24};
end;

