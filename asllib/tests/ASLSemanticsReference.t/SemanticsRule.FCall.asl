   func foo (x : integer) => integer
   begin
     return x + 1;
   end

   func bar (x : integer) => integer
   begin
     print(x);
   end

   func main () => integer
   begin
     foo(2);
     bar(3);
    return 0;
   end
