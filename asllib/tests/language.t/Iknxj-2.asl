//I_KNXJ: For the case where S is a bitvector type with undetermined width, 
//S will have some constraints which define its domain. Any bitvector whose
//domain is a subset of the domain of S will type satisfy S.
//
//For example bits(2) subtype-satisfies bits({2,4,8}) due to the domain rule,
//but bits(1) does not subtype-satisfy bits({2,4,8}). Similarly, bits({2,4})
//subtype-satisfies bits({2,4,8}) due to the domain rule, but bits({1,2}) 
//does not subtype-satisfy bits({2,4,8}).
//However, since the asserted type conversion bits({1,2})as bits({2,4,8}) 
//only fails the subtype-satisfaction because of the domain rule, but the
//domains do intersect, a runtime check will be inserted.

// RUN: interp %s | FileCheck %s

func testing(a: bits(2))
begin
    pass;
end

func test(a: bits(2))
begin
    testing(a);
end

func main() => integer
begin
    return 0;
end
