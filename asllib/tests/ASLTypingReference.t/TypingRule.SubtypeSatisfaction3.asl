type aNumberOfThings of integer;
type ShapeSides of aNumberOfThings;
type AnimalLegs of aNumberOfThings;
type InsectLegs of integer subtypes AnimalLegs;
func subtyping()
begin
    var myCircleSides: ShapeSides = 1; // legal
    var myInt : integer = myCircleSides; // legal
    var dogLegs : AnimalLegs = myCircleSides; // illegal: unrelated types
    var centipedeLegs: InsectLegs = 100; // legal
    var animalLegs : AnimalLegs = centipedeLegs; // legal
    var insectLegs : InsectLegs = animalLegs; // illegal: subtype is wrong way
end;
