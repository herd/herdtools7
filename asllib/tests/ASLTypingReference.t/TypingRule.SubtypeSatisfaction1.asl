type Color of enumeration {RED, GREEN, BLUE};
type SubColor subtypes Color;

type Word64WithLSB  of bits(64) { [63:32] upper,  [31:0] lower, [0] lsb};
type Word64         of bits(64) { [63:32] upper,  [31:0] lower};

func main() => integer
begin
    // real / string / boolean
    //     LHS type                     RHS type
    let q: real     = 5.0               as real;
    let s: string   = "hello"           as string;
    let b: boolean  = TRUE              as boolean;

    // Integers
    //     LHS type                     RHS type
    let i: integer = 5                  as integer;
    let j: integer = 5                  as integer{5, 7};
    let k: integer{5, 7, 8, 64} = 64    as integer{5, 7, 64};
    let m: integer{1..8} = 5            as integer{2..6};

    // Enumerations
    //     LHS type                     RHS type
    let e: Color = RED                  as SubColor;

    // Bitvectors
    //          LHS type                        RHS type
    let - :     bits(64) = Zeros{64}            as Word64;
    let sub_k:  integer{5, 64} = 64             as integer{5, 64};
    let - :     bits(k) = Zeros{64}             as bits(sub_k);
    let bv2:    bits(64) {[0] flag} = Zeros{64} as bits(64);

    // integer-indexed arrays
    var int_indexed_arr1 :  array[[3]] of integer;
    var int_indexed_arr2 :  array[[i-2]] of integer;
    int_indexed_arr2 = int_indexed_arr1 as array[[3]] of integer;
    var enum_indexed_arr1 :  array[[Color]] of integer;
    var enum_indexed_arr2 :  array[[Color]] of integer;
    enum_indexed_arr1 = enum_indexed_arr2;

    var data: (Color, integer{1..8}) = (RED as SubColor, 5 as integer{2..6});
return 0;
end;
