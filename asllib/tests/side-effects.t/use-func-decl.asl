pure func f0 (x: integer) => integer
begin
  return x + 2;
end;

pure func f1 (x: integer) => integer
begin
  return f(x);
end;

var global_x1: integer = 0;

readonly func g0 (x: integer) => integer
begin
  return global_x1 + x;
end;

readonly func g1 (x: integer) => integer
begin
  return g(x);
end;

func h1 (x: integer) => integer
begin
  return h(x);
end;

