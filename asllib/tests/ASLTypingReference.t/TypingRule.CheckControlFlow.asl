
type invalid_state of exception;

func all_terminating_paths_correct{N}(v: bits(N), flag: boolean) => bits(N)
begin
    if v != Zeros{N} then
        if flag then
            return Ones{N} XOR v;
        else
            Unreachable();
        end;
    else
        if flag then
            return v;
        else
            throw invalid_state{-};
        end;
    end;
end;
