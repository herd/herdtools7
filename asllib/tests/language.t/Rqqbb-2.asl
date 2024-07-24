//R_QQBB: Fixed point real numbers are written in decimal and consist of one
//or more decimal digits, a decimal point and one or more decimal digits.
//Underscores can be added between digits to aid readability.

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
