// RUN: interp %s | FileCheck %s

var none: bits(-: integer {1});
// legal since this is a fixed width bitvector whose determined width is 1
// not a constrained width bitvector of undetermined width
// var many: bits(-: integer {1,2});
// illegal since this is a constrained width bitvector of undetermined width
// and there is no initialization expression
config configValue: integer {1,2} = 1;
var many: bits(-: integer {1,2}) = Zeros(configValue);
// legal since this is a constrained width bitvector of undetermined width
// and this is a declaration where an initialization expression is given

func main() => integer
begin
    return 0;
end
