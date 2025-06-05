type MyException of exception;

func main() => integer
begin
    try
        for i = 0 to 0 do
            println("i=", i);
            pass;
        end;
        // 'i' is not in scope now and can be declared as a local storage element.
        var i: real = 5.5;
        println("i=", i);
    catch
        when exn: MyException => pass;
    end;
    // 'exn' is not in scope now and can be declared as a local storage element.
    var exn: boolean = TRUE;
    println("exn=", exn);
    return 0;
end;
