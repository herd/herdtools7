//------------------------------------------------------------------------------
// Backwards compatibility for ASLv0

bits(N) ReplicateBit(boolean isZero, integer N)
  return if isZero then Zeros(N) else Ones(N);

bits(M*N) Replicate(bits(M) x, integer N)
  if M == 1 then
    return ReplicateBit(IsZero(x), M*N);
  else
    r = Zeros(M*N);
    for i=0 to N-1
        r<M*(i+1) - 1 : M*i> = x;
    return r;

bits(N) Zeros(integer N)
  return 0<N-1:0>;

bits(N) Ones(integer N)
  return NOT Zeros(N);

bits(N) SignExtend(bits(M) x, integer N)
  assert N >= M;
  return Replicate(x<M-1>, N-M) : x;

bits(N) ZeroExtend(bits(M) x, integer N)
  assert N >= M;
  return Zeros(N-M) : x;

bits(N) Extend(bits(M) x, integer N, boolean unsigned)
  return if unsigned then ZeroExtend(x, N) else SignExtend(x, N);
