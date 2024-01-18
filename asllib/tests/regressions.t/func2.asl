getter X_get[i:integer] => integer
begin
    return i;
end

setter X_set[i:integer] = v:integer
begin
    let internal_i = i;
    let internal_v = v;
end

func main() => integer
begin
    X_set[2] = 3;
    let x = X_get[4];

    assert x == 4;

    return 0;
end

// RUN: archex.sh --eval=':set asl=1.0' --eval=':set +syntax:aslv1_colon_colon' --eval=':load %s' --eval='assert main() == 0;' | FileCheck %s

