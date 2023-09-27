func f(i:integer) => integer
begin
    return i;
end

func main() => integer
begin
    let x = 3;
    let y = f(x);
    assert x == 3;
    assert y == 3;

    return 0;
end

// RUN: archex.sh --eval=':set asl=1.0' --eval=':set +syntax:aslv1_colon_colon' --eval=':load %s' --eval='assert main() == 0;' | FileCheck %s

