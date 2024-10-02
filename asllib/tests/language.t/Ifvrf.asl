//I_FVRF: Pragmas can be used at the declarative scope and within a
//subprogram as a statement.
//
//func my_function_with_tool_pragmas()
//begin
//  pragma my_tool_pragma1;
//  pass;
//end

// RUN : interp %s | FileCheck %s

// ! TODO

func main() => integer
begin
     return 0;
end
