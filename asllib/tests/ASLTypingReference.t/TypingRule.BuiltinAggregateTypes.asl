type Pair of (integer, boolean);

type T of array [[3]] of real;

type PointRecord of record
  { x : real, y : real, z : real };

func main () => integer
begin
  let p = (0, FALSE);
  let o = PointRecord { x=0.0, y=0.0, z=0.0 };

  return 0;
end;
