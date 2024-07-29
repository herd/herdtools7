type pair of (integer, boolean);

type T of array [3] of real;
type coord of enumeration { X, Y, Z };
type pointArray of array [coord] of real;

type pointRecord of record
  { x : real, y : real, z : real };

func main () => integer
begin
  let p = (0,FALSE);

  var t1 : T; var t2 : pointArray;
  assert (t1[0] == t2[X]);

  let o = pointRecord { x=0.0, y=0.0, z=0.0 };
  assert (t2[Z] == o.z);

  return 0;
end
