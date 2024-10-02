//R_BHMY: The config keyword is used to declare global config identifiers
//denoting global storage elements which are all of the following:
//non-compile-time-constant, non-execution-time, immutable.

// RUN: interp %s | FileCheck %s

config a: integer = 10;

func main() => integer
begin
    return 0;
end
