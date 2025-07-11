integer f(integer x)
  return x + 2;

integer global_x;

integer g(integer x)
  return x + global_x;

integer h(integer x)
  integer local_x = global_x;
  global_x = local_x + x;
  return global_x;

