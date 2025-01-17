// Declare an array of reals from arr1[[0]] to arr1[[3]]
type arr1 of array [[4]] of real;

// Declare an array with two entries arr2[[big]] and arr2[[little]]
type labels of enumeration {big, little};
type arr2 of array [[labels]] of bits(4);

func foo(x: array [[4]] of integer) => array [[4]] of integer
begin
  var y = x;
  y[[3]] = 2;
  return y;
end;

func main () => integer
begin
  var x: array [[4]] of integer;
  x[[1]] = 1;

  x = foo (x as array [[4]] of integer);
  let y: array [[4]] of integer = x;
  return 0;
end;
