//I_LGHJ: This rule means that when operating on two bitvectors of different
//types (e.g. a bits(M) and a bits(N)), an asserted type conversion should
//be used to indicate that they are known to be of the same width. The
//intention is that, instead of silently inserting an execution-time check,
//an ASL processor will error by default without an explicit asserted type
//conversion from the author.

// RUN: interp %s | FileCheck %s

func check{M: integer{4,8}}(flag: boolean, x: bits(M), y: bits(8)) => boolean
begin
    if flag then
        return (x as bits(8)) == y; // valid
    end
end

func main() => integer
begin
    return 0;
end
