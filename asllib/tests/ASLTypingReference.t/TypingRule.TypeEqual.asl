type Color of enumeration { RED, GREEN, BLUE };
type MyRecord of record { flag: boolean };

impdef          func Foo{N}(
    s: string,
    r: real,
    i: integer,
    ci: integer{0..N},
    bv : bits(2) { [0] lsb, [1] msb },
    a: array[[10]] of integer,
    c: Color,
    rec: MyRecord,
    t: (boolean, integer)
) begin pass; end;

implementation  func Foo{N}(
    s: string,
    r: real,
    i: integer,
    ci: integer{0..N},
    bv : bits(2) { [0] lsb, [1] msb },
    a: array[[10]] of integer,
    c: Color,
    rec: MyRecord,
    t: (boolean, integer)
) begin pass; end;
