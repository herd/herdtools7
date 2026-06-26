// Declare some named types
type WordCount of integer;
type ByteCount of integer;
type PacketLength of WordCount;

func assign()
begin
    // Integer is subtype-satisfied by all the named types,
    // so it can be assigned to them by the assignment and
    // initialization type checking rules
    var myInt: integer;
    var myWordCount   : WordCount    = myInt;
    var myByteCount   : ByteCount    = myInt;
    var myPacketLength: PacketLength = myInt;

    // Integer is subtype-satisfied by all the named types,
    // so it can be assigned from them by the assignment and
    // initialization type checking rules
    myInt = myWordCount;
    myInt = myByteCount;
    myInt = myPacketLength;
end;

type aNumberOfThings of integer;
type ShapeSides      of aNumberOfThings;
type AnimalLegs      of aNumberOfThings;
type InsectLegs      of integer ;

func subtype_satisfaction()
begin
    var  myCircleSides: ShapeSides = 1;             // legal
    var  myInt        : integer    = myCircleSides; // legal
    var  centipedeLegs: InsectLegs = 100;           // legal
end;

func main () => integer
begin
  assign ();
  subtype_satisfaction ();

  return 0;
end;

// RUN: archex.sh --eval=':set asl=1.0' --eval=':set +syntax:aslv1_colon_colon' --eval=':load %s' --eval='assert main() == 0;' | FileCheck %s
