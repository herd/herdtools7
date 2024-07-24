//R_BRCM: Concatenation of multiple bitvectors is done using a comma
//separated list surrounded with square brackets.
//Example:
//  var T: boolean = [ '1111', '0000' ] == '11110000';

// RUN: interp %s | FileCheck %s
// CHECK: TRUE

func main() => integer
begin
    var T: boolean = [ '1111', '0000' ] == '11110000';

    print(T);
    return 0;
end
