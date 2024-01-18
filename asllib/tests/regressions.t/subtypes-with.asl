type rec1 of record {
  a: integer,
  b: integer,
};

type rec2 subtypes rec1 with { 
  c: integer,
};

type rec2bis subtypes rec1 with { 
  c: integer,
};

func main() => integer
begin
  let a = rec2 {
    a = 1,
    b = 2,
    c = 3
  };

  assert a.c == 3;
  
  var b = rec2bis {
    a = 1,
    b = 2,
    c = 4
  };
  b.c = b.c + 1;
  assert b.c == 5;

  var c = b as rec1;

  assert c.a == 1;
  c.b = 3;
  assert b.b == 2;

  return 0;
end
