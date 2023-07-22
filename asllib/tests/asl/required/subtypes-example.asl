// Declare some named types
type superInt of integer;
type subInt of integer subtypes superInt ;
type uniqueInt of superInt;

func assign()
begin
    // Integer is subtype-satisfied by all the named types,
    // so it can be assigned to them by the assignment and
    // initialization type checking rules
    var myInt: integer;
    var mySuperInt : superInt  = myInt;
    var mySubInt   : subInt    = myInt;
    var myUniqueInt: uniqueInt = myInt;

    // Integer is subtype-satisfied by all the named types,
    // so it can be assigned from them by the assignment and
    // initialization type checking rules
    myInt = mySuperInt;
    myInt = mySubInt;
    myInt = myUniqueInt;

    // superInt is not a subtype of anything (apart from itself)
    // so it cannot be assigned to any other named type
    // Illegal: mySubInt    = mySuperInt;
    // Illegal: myUniqueInt = mySuperInt;

    // subInt is a subtype of superInt, so the assignment and
    // initialization type checking rules permit the following:
    mySuperInt = mySubInt;
    // But subInt and uniqueInt are not subtype related
    // so do not type-satisfy each other.
    // Illegal: myUniqueInt = mySubInt;

    // uniqueInt has no related subtype or supertype
    // so it cannot be assigned to any named type
    // Illegal: mySuperInt = myUniqueInt;
    // Illegal: mySubInt = myUniqueInt;
end

type aNumberOfThings of integer;
type ShapeSides      of aNumberOfThings;
type AnimalLegs      of aNumberOfThings;
type InsectLegs      of integer subtypes AnimalLegs ;

func subtyping()
begin
    var  myCircleSides: ShapeSides = 1;             // legal
    var  myInt        : integer    = myCircleSides; // legal
    // var  dogLegs      : AnimalLegs = myCircleSides; // illegal: unrelated types
    var  centipedeLegs: InsectLegs = 100;           // legal
    var  animalLegs   : AnimalLegs = centipedeLegs; // legal
    // var  insectLegs   : InsectLegs = animalLegs;    // illegal: subtype is wrong way
end

func main () => integer
begin
  assign ();
  subtyping ();

  return 0;
end

// RUN: archex.sh --eval=':set asl=1.0' --eval=':set +syntax:aslv1_colon_colon' --eval=':load %s' --eval='assert main() == 0;' | FileCheck %s

