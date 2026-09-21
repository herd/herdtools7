type UnconstrainedIntegerType of integer;
type ConstrainedIntegerType of integer{-1, 1..1000};
type RealType of real;
type StringType of string;
type BooleanType of boolean;
type BitvectorType of bits(8) { [7] msb, [0] lsb };
type BitType of bit;
type TupleType of (integer, bits(16));
type Color of enumeration {RED, GREEN, BLUE};
type ArrayType of array[[10]] of integer;
type RecordType of record { val: integer, flag: boolean};
type ExceptionType of exception { val: integer, flag: boolean};
var c : collection { data: bits(16), status: bits(8) };

func main() => integer
begin
    var x = 5 as integer{0..20};
    return 0;
end;
