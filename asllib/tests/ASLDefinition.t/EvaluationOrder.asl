// A helper function which prints its argument
func p(n: integer) => integer
begin
  print n;
  return n;
end;

// A helper function which prints its first argument and returns an array
func arr(n: integer) => array[[8]] of integer
begin
  println n;
  var arr : array[[8]] of integer;
  return arr;
end;

// A helper accessor pair taking two arguments
accessor Foo(a: integer, b: integer) <=> value_in: integer
begin
  readonly getter
    return 0;
  end;

  setter
    pass;
  end;
end;

// A helper record type
type Record of record {
  a: integer,
  b: integer,
};

func main() => integer
begin
  println "Function calls:";
  Foo(p(3), p(4)) = Foo(p(1), p(2));
  println ;

  println "Tuples:";
  - = (p(1), p(2));
  println ;

  println "Non-short-circuiting binary operations:";
  - = p(1) + p(2) + p(3);
  println ;

  println "Array-indexing:";
  - = arr(1)[[p(2)]];
  println ;

  println "Record construction:";
  - = Record{ a = p(1), b = p(2) };
  println ;

  println "Print statements:";
  println p(1), p(2), p(3), p(4);

  return 0;
end;
