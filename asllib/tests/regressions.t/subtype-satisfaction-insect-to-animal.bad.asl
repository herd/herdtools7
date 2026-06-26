type aNumberOfThings of integer;
type AnimalLegs of aNumberOfThings;
type InsectLegs of integer;

func main() => integer
begin
  var centipedeLegs: InsectLegs = 100;
  // illegal: unrelated types
  var animalLegs: AnimalLegs = centipedeLegs;
  return 0;
end;
