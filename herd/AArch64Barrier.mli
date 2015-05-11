module type S = sig
  type a
  type b = 
    | ISB 
    | DMB of AArch64Base.mBReqDomain * AArch64Base.mBReqTypes
    | DSB of AArch64Base.mBReqDomain * AArch64Base.mBReqTypes
  val a_to_b : a -> b
end
