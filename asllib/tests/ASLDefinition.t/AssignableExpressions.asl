type MyRecord of record { status: bit, time: bits(16), data: bits(8), a: array[[5]] of bits(8) };

func main() => integer
begin
    var x: integer;
    var z: integer{0..32};
    var my_array : array [[42]] of integer ;
    var bv = '11 11 1111';
    var r: MyRecord;

    // x is an example of a variable assignable expression.
    x = 5;

    // - is an example of a discarding assignable expression.
    - = 6;

    // (x , -, z ) is an example of a multi-assignment assignable expression.
    (x , -, z ) = (2 , 3 , 4) ;

    // my_array[[3]] is an example of an array assignable expression.
    my_array[[3]] = 53;

    // bv[3:0 , 7:6] is an example of a bitvector slice assignable expression.
    bv[3:0 , 7:6] = '000000';

    // Several fields can be assigned simultaneously.
    r.[data, time, status] = Ones{8} :: Zeros{16} :: '1';

    // Assignable expressions can be compound:
    r.data[0] = '1';
    r.a[[2]][1] = '0';

    return 0;
end;
