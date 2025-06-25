type E of exception {-};

pure func throwing () => integer
begin
  throw E {-};
end;

type T of integer {throwing ()};

