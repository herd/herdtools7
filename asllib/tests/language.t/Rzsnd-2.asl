//R_ZSND: If either the start or end expression of the for-statement are
//unconstrained integers then the for-loop counter is an unconstrained
//integer. If either the start or end expression of the for-statement are
//under-constrained integers and neither the start nor the end expression
//are unconstrained integers then the for-loop counter is an
//under-constrained integer. Otherwise the for-loop counter is a constrained
//integer whose constraint is:
//- min(SC)..max(EC)U SC when the direction is to and min(SC)<= max(EC)
//- SC when the direction is to and min(SC)> max(EC)
//- max(SC)..min(EC)U SC when the direction is downto and max(SC)>= min(EC).
//- SC when the direction is downto and max(SC)< min(EC).
//where SC denotes the domain of the type of the start expression, EC
//denotes the domain of the type of the end expression, x..y denotes the
//closed integer interval between x and y inclusive and U denotes the set
//union operation.

// RUN: interp %s | FileCheck %s

func testa(N: integer)
begin
    for x = 0 to N do
        pass;
    end
end

func testb(N: integer)
begin
    for x = 0 to N do
        pass;
    end
end

func main() => integer
begin
    return 0;
end
