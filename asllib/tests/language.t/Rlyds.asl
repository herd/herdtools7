//R_LYDS: The domain D of a constraint range that contains well-constrained
//integer expressions is recursively defined as such:
//• If the constraint range consists of a single well-constrained integer
//expression that is a compile time constant c,D = {c}
//• If the constraint range consists of a lower and upper bound range c..d
//where c and d are well-constrained integer expressions that are compile
//time constants then:
//  – ifc <=d thenD = [c..d]
//  – otherwise the constraint range is illegal and should generate a
//  type-checking error
//• If the constraint range consists of a single statically evaluable
//well-constrained integer expression E, D is equal to the domain of the
//type of E
//• Consider a constraint range that consists of a lower and upper bound
//range Ec..Ed where both Ec and Ed are statically evaluable,
//well-constrained integer expressions. Let LD and UD be the domain of the
//type of Ec and Ed respectively, where mLD/MLD denotes the minimum/maximum
//element of LD and mUD/MUD denotes the minimum/maximum element of UD.
//– if MLD <= mUD then D = [mLD..MUD]
//– otherwise the constraint range is illegal and should generate a
//type-checking error.

// RUN: interp %s | FileCheck %s


var a: integer{10};
var b: integer{0..10};
var c: integer{5 + 5};
constant d: integer{0..10} = 5;
var e: integer{d..20};

func main() => integer
begin
    return 0;
end
