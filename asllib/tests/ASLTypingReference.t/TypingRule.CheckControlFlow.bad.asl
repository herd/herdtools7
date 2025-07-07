noreturn func returning() => integer
begin
    return 0; // Illegal as the containing function is qualified with noreturn
end;
