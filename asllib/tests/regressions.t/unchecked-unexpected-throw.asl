type E of exception {-};

func throwing() => integer
begin
  throw E {-};
end;

func main() => integer
begin
  var xs : array[[throwing()]] of integer;
  return 0;
end;
