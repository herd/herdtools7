func reconsider15()
begin
    // currently required to fail as 8 is guaranteed to be out of range. However this isn't consistent with the general approach behind
    // ASL-313. This isn't currently causing any problems in the ported pseudocode, but we might want to reconsider this for consistency.
    // If we do then this would be required be legal statically, but raise an error at runtime if the line is executed.
    let testA = Zeros(2)[8:];
    // Similar issue with negative width bit vectors
    let testB = Zeros(8)[0 +: -1];
end

