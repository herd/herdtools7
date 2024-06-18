//I_HYBT: Subprogram invocations are also [like subprogram declarations, see
//I_NXJR] either execution-time or non-execution-time invocations.

//I_NXJR: Subprogram declarations in ASL are either execution-time
//subprogram declarations or non-execution-time subprogram declarations.


// RUN: interp %s | FileCheck %s

var counter: integer = 0;

func nonexecution() => integer
begin
    return 10;
end

func execution() => integer
begin
    counter = counter + 1;
    return counter;
end


func main() => integer
begin
    let a = nonexecution();
    let b = execution();

    return 0;
end
