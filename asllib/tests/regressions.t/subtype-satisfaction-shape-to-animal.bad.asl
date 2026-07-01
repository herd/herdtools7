type aNumberOfThings of integer;
type ShapeSides of aNumberOfThings;
type AnimalLegs of aNumberOfThings;

func main() => integer
begin
  var myCircleSides: ShapeSides = 1;
  // illegal: unrelated types
  var dogLegs: AnimalLegs = myCircleSides;
  return 0;
end;
