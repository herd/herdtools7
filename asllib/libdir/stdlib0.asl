//------------------------------------------------------------------------------
// Backwards compatibility for ASLv0

bits(N) ReplicateBit(boolean isZero, integer N)
  return ReplicateBit{N}(isZero);

bits(M*N) Replicate(bits(M) x, integer N)
  return Replicate{M*N,M}(x);

bits(N) Zeros(integer N)
  return Zeros{N}();

bits(N) Ones(integer N)
  return Ones{N}();

bits(N) SignExtend(bits(M) x, integer N)
  return SignExtend{N,M}(x);

bits(N) ZeroExtend(bits(M) x, integer N)
  return ZeroExtend{N,M}(x);

bits(N) Extend(bits(M) x, integer N, boolean unsigned)
  return Extend{N,M}(x, unsigned);
