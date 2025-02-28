// Declare an array of reals from arr1[[0]] to arr1[[3]]
type arr1 of array [[4]] of real;

// Declare an array with two entries arr2[[big]] and arr2[[little]]
type Labels of enumeration {BIG, LITTLE};
type BitsArray of array [[Labels]] of bits(4);

func foo(x: array [[4]] of integer) => array [[4]] of integer
begin
  var y = x;
  y[[3]] = 2;
  return y;
end;

func main () => integer
begin
  var int_arr: array [[4]] of integer;
  var big_little_arr: BitsArray;
  // Array write expression   Array read expression
  int_arr[[1]]              = int_arr[[3]] as integer;
  big_little_arr[[BIG]]     = big_little_arr[[LITTLE]] as bits(4);

  int_arr = foo(int_arr as array [[4]] of integer);
  let y: array [[4]] of integer = int_arr;
  return 0;
end;
