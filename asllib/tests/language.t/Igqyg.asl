// RUN: interp %s | FileCheck %s

var gInt: integer {1,2,3}; // a constrained mutable global

func mutables(wid: integer{})
begin
    // type checker knows wid-->wid
    constant mod = 1;
    // RHS is immutable so mod-->1
    let size01 = wid + gInt;
    // RHS is mutable so size01-->size01
    var data01: bits(size01+1);
    // size01 reduces to size01 so
    // type checker knows data01 is (size01+1) wide
    let size02 = wid + gInt + mod;
    // RHS is mutable so size02-->size02
    var data02: bits(size02);
    // size02-->size02 so
    // type checker knows data02 is (size02) wide
    data01=data02;
    // type checker emits an error "Widths do not match"
    // since it cannot tell that (size01+1)==(size02)
end

func immutables(wid: integer{})
begin
    // type checker knows wid-->wid
    constant mod = 1;
    // RHS is immutable so mod-->1
    let size01 = wid;
    // RHS is immutable so size01-->wid
    var data01: bits(size01+1);
    // size01-->wid so
    // type checker knows data01 is (wid+1) wide
    let size02 = wid + mod;
    // RHS is statically evaluable
    // and mod-->1 so
    // type checker knows size02-->(wid+1)
    var data02: bits(size02);
    // size02-->(wid+1) so
    // type checker knows data02 is (wid+1) wide
    data01=data02;
    // type checker knows that (wid+1)==(wid+1)
    // Widths match
end

func main() => integer
begin
    return 0;
end
