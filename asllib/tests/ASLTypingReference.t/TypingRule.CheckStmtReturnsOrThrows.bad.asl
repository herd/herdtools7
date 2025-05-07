
type invalid_state of exception;

func incorrect_terminating_path{N}(v: bits(N), flag: boolean) => bits(N)
begin
    if v != Zeros{N} then
        if flag then
            return Ones{N} XOR v;
        end;
    else
        if flag then
            return v;
        else
            throw invalid_state{-};
        end;
    end;
end;
