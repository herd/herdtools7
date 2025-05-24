constant N = 15;

func main() => integer
begin
    var l = 1;
    let x: integer{1..2 * N} = l;
    // THe following declaration fails typechecking, since the typechecker
    // is unable to prove that {2} is a subtype of integer {x..x + 1}.
    let t: integer{x..x + 1} = 2;
    return 0;
end;
