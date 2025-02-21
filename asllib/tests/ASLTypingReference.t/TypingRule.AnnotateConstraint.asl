func foo{M}(bv: bits(M)) => integer
begin
    var z: integer{3*M} = 3*M; // type of z: integer {(3 * M)}
    return z;
end;

constant N = 15;

func main() => integer
begin
  let x: integer{1..2*N} = 1;   // type of x: integer {1..30}
  let t: integer{x..x+1} = 2;   // type of t: integer {1..2}
  var y: integer{x, t};         // type of y: integer {1, 2}

  return 0;
end;
