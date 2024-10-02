//I_GYSK: Where S is a bitvector with determined width and T has
//undetermined width, the domain requirement will stop T from being used
//where S is expected. For example, bits({2,4}) does not subtype-satisfy
//bits(4) since the domain of bits({2,4}) is not a subset of the domain of
//bits(4). 
//However, since the asserted type conversion 
//   bits({2,4}) as bits(4)
//only fails the subtype-satisfaction because of the domain rule, but the
//domains do intersect, a runtime check will be inserted.

// RUN: not interp %s | FileCheck %s

func test(a: bits({2, 4}))
begin
    var b: bits(4) = a;
end

func main() => integer
begin
    return 0;
end
