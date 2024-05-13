//I_FKJC: Writing ASL specifications which rely on the implicit
//initialization of local variables is strongly discouraged. It is
//recommended that tools which process ASL attempt to detect code which
//reads local variables before writing to them and report this as an error
//which can be downgraded to a warning by users.

// RUN : interp %s | FileCheck %s

// ! TODO 

func main() => integer
begin
    var a = 10;
    var b = a + 5;

    return 0;
end
