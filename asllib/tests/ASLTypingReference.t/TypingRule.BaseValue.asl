var global_base: integer;

type Color of enumeration { RED, GREEN, BLUE };

type ConstrainedInteger of integer{ 15, -7, -9..-3 };
type Packet of record {data: bits(5), time: integer, flag: boolean};
type MyException of exception {msg: string};

func main() => integer
begin
    var unconstrained_integer_base: integer;
    var constrained_integer_base: ConstrainedInteger;
    var bool_base: boolean;
    var real_base: real;
    var string_base: string;
    var enumeration_base: Color;
    println
        "global_base = ", global_base,
        ", unconstrained_integer_base = ", unconstrained_integer_base,
        ", constrained_integer_base = ", constrained_integer_base;
    println
        "bool_base = ", bool_base,
        ", real_base = ", real_base,
        ", string_base = ", string_base,
        ", enumeration_base = ", enumeration_base;

    var bits_base: bits(5);
    println "bits_base = ", bits_base;
    var tuple_base: (integer, ConstrainedInteger, Color);
    println "tuple_base = (",
        tuple_base.item0, ", ",
        tuple_base.item1, ", ",
        tuple_base.item2, ")";

    var record_base: Packet;
    println "record_base      = {data=", record_base.data,
        ", time=", record_base.time,
        ", flag=", record_base.flag, "}";
    var record_base_init: Packet = Packet{data='00000', time=0, flag=FALSE};
    println "record_base_init = {data=", record_base_init.data,
         ", time=", record_base_init.time,
         ", flag=", record_base_init.flag, "}";

    var exception_base: MyException;
    println "exception_base = {msg=", exception_base.msg, "}";
    var integer_array_base: array[[4]] of integer;
    println "integer_array_base = [[",
        integer_array_base[[0]], ", ",
        integer_array_base[[1]], ", ",
        integer_array_base[[2]], ", ",
        integer_array_base[[3]], "]]";
    var enumeration_array_base: array[[Color]] of integer;
    println "enumeration_array_base = [[",
        RED, "=", enumeration_array_base[[RED]], ", ",
        GREEN, "=", enumeration_array_base[[GREEN]], ", ",
        BLUE, "=", enumeration_array_base[[BLUE]], "]]";
    return 0;
end;
