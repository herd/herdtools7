bits(N+N) Dup(bits(N) b)
  return b : b;


integer main()
  x = 2;
  b = Dup(Ones(x));
  assert b == '1111';
  return 0;
