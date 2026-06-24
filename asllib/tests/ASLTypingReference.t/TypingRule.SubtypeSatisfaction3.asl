type aNumberOfThings of integer;
type ShapeSides of aNumberOfThings;
type AnimalLegs of aNumberOfThings;
func subtype_satisfaction()
begin
    var myCircleSides: ShapeSides = 1; // legal
    var myInt : integer = myCircleSides; // legal
    var dogLegs : AnimalLegs = myCircleSides; // illegal: unrelated types
end;
