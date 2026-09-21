func main() => integer
begin
    var i = -528;
    var bv_i: bits(16) = i[15:0];
    var bv = '1111110111110000'; // -528 as a binary signed 2's complement.
    println "bv = ", bv, ", bv_i = ", bv_i;
    return 0;
end;
