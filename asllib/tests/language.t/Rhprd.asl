//R_HPRD: Identifiers start with a letter or underscore and continue with
//zero or more letters, underscores or digits.

// RUN: interp %s | FileCheck %s

func main() => integer
begin
    var a: integer;
    var _a: integer;
    var abc: integer;
    var ___: integer;
    var _a0: integer;

    return 0;
end
