
func negative3(size2 : integer {8,16,32,64})
begin
    // inexact operators (eg those with rounding) don't propagate constraints
    let testA : integer {1,2,4,8} = size2 DIVRM 8; // illegal as DIVRM output is of type "integer" not "integer {1,2,4,8}"
end;

