//D_HBCP: The global type environment is a type environment which maps
//every globally declared identifier to its type.

// RUN: interp %s | FileCheck %s

// ! Nothing to test here

func main() => integer
begin
    return 0;
end
