type Pair of (integer, boolean);

type T of array [3] of real;
type Coord of enumeration { CX, CY, CZ };
type PointArray of array [Coord] of real;

type PointRecord of record
  { x : real, y : real, z : real };

func main () => integer
begin
  let p = (0, FALSE);

  var t1 : T; var t2 : PointArray;
  assert (t1[0] == t2[CX]);

  let o = PointRecord { x=0.0, y=0.0, z=0.0 };
  assert (t2[CZ] == o.z);

  return 0;
end
