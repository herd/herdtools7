type E of exception {-};

func throwing () => integer
begin
  throw E {-};
end;

type T of integer {throwing ()};

