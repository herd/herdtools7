//R_JWPH: The default entry point is the main function.

// RUN: interp %s | FileCheck %s
// CHECK: Hello World

func main() => integer
begin
    print("Hello World");
    return 0;
end
