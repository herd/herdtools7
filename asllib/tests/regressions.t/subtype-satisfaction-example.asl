// Declare some named types
type superInt of integer;
type subInt of integer;
type uniqueInt of superInt;

func assign()
begin
    // Integer is subtype-satisfied by all the named types,
    // so it can be assigned to them by the assignment and
    // initialization type checking rules
    var myInt: integer;
    var mySuperInt : superInt  = myInt;
    // var mySubInt   : subInt    = myInt; // Illegal
    var myUniqueInt: uniqueInt = myInt;

    // Integer is subtype-satisfied by all the named types,
    // so it can be assigned from them by the assignment and
    // initialization type checking rules
    myInt = mySuperInt;
    // myInt = mySubInt; // illegal
    myInt = myUniqueInt;

    // superInt is unrelated in terms of subtype-satisfaction to mySuperInt
    // Illegal: mySubInt    = mySuperInt;
    // Illegal: myUniqueInt = mySuperInt;

    // Illegal: myUniqueInt = mySubInt;

    // uniqueInt has no related subtype
    // so it cannot be assigned to any named type
    // Illegal: mySuperInt = myUniqueInt;
    // Illegal: mySubInt = myUniqueInt;
end;

type aNumberOfThings of integer;
type ShapeSides      of aNumberOfThings;
type AnimalLegs      of aNumberOfThings;
type InsectLegs      of integer ;

func subtype_satisfaction()
begin
    var  myCircleSides: ShapeSides = 1;             // legal
    var  myInt        : integer    = myCircleSides; // legal
    // var  dogLegs      : AnimalLegs = myCircleSides; // illegal: unrelated types
    var  centipedeLegs: InsectLegs = 100;           // legal
    // var  animalLegs   : AnimalLegs = centipedeLegs; // illegal: unrelated types
end;

func main () => integer
begin
  assign ();
  subtype_satisfaction ();

  return 0;
end;

// RUN: archex.sh --eval=':set asl=1.0' --eval=':set +syntax:aslv1_colon_colon' --eval=':load %s' --eval='assert main() == 0;' | FileCheck %s

