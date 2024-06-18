//R_GFSH: Files contain printable ASCII characters, carriage return and line
//feed.

// RUN: not interp %s | FileCheck %s

func main() => integer
begin
    let a: String = "©";
    return 0;
end
