// RUN: interp %s | FileCheck %s

func main() => integer
begin
    assert 10.0 == 10.0;
    assert 1_0.0 == 10.0;
    assert 10.0_ == 10.0;
    assert 10.00 == 10.0;
    assert 10_.0 == 10.0;
    assert 10.0_0 == 10.0;

    return 0;
end