let ci : integer{1..1000} = 500;

func parameterized{A, B: integer, C: integer{ci}}(x: bits(A), y: bits(B), z: bits(C))
begin
    - = A;
    - = B;
    - = C;
end;
