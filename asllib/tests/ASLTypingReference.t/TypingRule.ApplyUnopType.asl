func main() => integer
begin
    //        result type                               input type
    var b   : boolean                       = ! TRUE    as boolean;
    var i1  : integer{-5}                   = - (5      as integer{5});
    var i2  : integer{-(-5)}                = - i1;
    var ci1 : integer{0..5, 9, 10..8}       = 4         as integer{0..5, 9, 10..8};
    var ci2 : integer{-9, -8..-10, -5..0}   = - (ci1    as integer{0..5, 9, 10..8});
    var ui1 : integer                       = 9         as integer;
    var ui2 : integer                       = - ui1     as integer;

    var r1  : real                          = - 5.0     as real;
    var r2  : real                          = - r1      as real;

    var bv1 : bits(8) {[0] flag}            = Zeros{8}  as bits(8) {[0] flag};
    var bv2 : bits(8) {[0] flag}            = NOT bv1   as bits(8) {[0] flag};
    return 0;
end;
