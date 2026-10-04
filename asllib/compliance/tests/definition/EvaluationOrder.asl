// A helper function which prints its argument
func p(n: integer) => integer
begin
  print n;
  return n;
end;

var g : integer = 0;

// A helper function which prints its first argument, increments g,
// and returns an array of increasing values.
func arr(n: integer) => array[[8]] of integer
begin
  println n;
  var arr : array[[8]] of integer;
  arr[[1]] = 1;
  arr[[2]] = 2;
  arr[[3]] = 3;
  g = g + 1;
  return arr;
end;

// A function that reads the global variable g.
readonly func q(n: integer) => integer
begin
  return n + g;
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
  var m = arr(1)[[q(2)]];
  println m;

  println "Record construction:";
  - = Record{ a = p(1), b = p(2) };
  println ;

  println "Print statements:";
  println p(1), p(2), p(3), p(4);

  return 0;
end;
