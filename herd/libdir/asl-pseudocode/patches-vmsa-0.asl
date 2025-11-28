(FaultRecord, bits(N)) AArch64.MemSwapTableDesc(FaultRecord fault_in, bits(N) prev_desc,
                                                bits(N) new_desc, bit ee,
                                                AccessDescriptor  descaccess,
                                                AddressDescriptor descpaddr, integer N)
  (fault_out, res) = AArch64_MemSwapTableDesc{N}(fault_in, prev_desc, new_desc, ee, descaccess, descpaddr);
  return (fault_out, res);

