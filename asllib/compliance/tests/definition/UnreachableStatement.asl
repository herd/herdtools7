func diagnostic_assertion(condition: boolean, should_check: boolean, message: string)
begin
    if should_check && !condition then
        println "diagnostic assertion failed: ", message;
        unreachable;
    end;
end;

func main() => integer
begin
    diagnostic_assertion(FALSE, TRUE, "example message");
    return 0;
end;
