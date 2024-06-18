//R_PTNG: Statements consist of:
//- Declarations of variables, let values and constants. 
//- Assignment statements.
//- Procedure invocation statements.
//- Return statements.
//- Assertion statements.
//- Throw statements which throw exceptions. 
//- Pass statements which do nothing (nop).
//- Conditional statements. 
//- Case Statements.
//- Repetitive statements. 
//- Exception handling.

// RUN: interp %s | FileCheck %s

// ! TODO 

func main() => integer
begin
    return 0;
end
