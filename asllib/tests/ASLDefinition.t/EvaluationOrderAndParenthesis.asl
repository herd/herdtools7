func Print(x : integer) => integer
begin
    print x;
    return x;
end;

func main() => integer
begin
    - =  Print(1)  +  Print(2)  +  Print(3);
    println;
    - = (Print(1)  +  Print(2)) +  Print(3);
    println;
    - =  Print(1)  + (Print(2)  +  Print(3));
    println;
    return 0;
end;
