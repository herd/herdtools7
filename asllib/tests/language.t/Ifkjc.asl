// RUN : interp %s | FileCheck %s

// ! How would I detect a warning here?

func main() => integer
begin
    var a = 10;
    var b = a + 5;

    return 0;
end
