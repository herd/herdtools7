type t1 of record {-};

func f1 (x: (integer, t1))
begin
  pass;
end;

func f1 (x: (integer{32}, t1))
begin
  pass;
end;
