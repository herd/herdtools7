//D_MTQJ: A type is a compile-time-constant type if its structure depends
//only on:
//- compile-time-constant types
//- compile-time-constant expressions

// RUN: interp %s | FileCheck %s

// All of the following are compile-time types
constant wid = 32;

type busTy of bits(wid);
type recTy of record {
    bus: busTy,
    valid: bit
};

func constType()
begin
    var I: integer;
    var R: bits(wid);
end

func main() => integer
begin
    return 0;
end
