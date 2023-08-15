type l of bits(64) { [1] x, [2] y, [3] z };
type m of array[10] of l;
type n of bits(64) { [1] x, [2] y, [3] z };
type o of array[10] of n subtypes m;

func main () => integer
begin
  return 0;
end
