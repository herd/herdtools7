////////////////////////////////////////////////////////////////////////////////
// loops
////////////////////////////////////////////////////////////////////////////////
func positive8(N : integer {0..15}, M : integer {7,15})
begin
    for i = 0 to 7 do
        // i has type integer {0..7}
        let testA : integer {0..7} = i;
    end
    for i = 7 downto 0 do
        // i has type integer {0..7}
        let testB : integer {0..7} = i;
    end
    for i = 0 to N do
        // i has type integer {0..N}
        let testC : integer {0..15} = i;
    end
    for i = 0 to M do
        // i has type integer {0..M}
        let testD : integer {0..15} = i;
    end
    for i = N downto 0 do
        let testE : integer {0..15} = i;
    end
    for i = M downto 0 do
        let testF : integer {0..15} = i;
    end
    for i = N to 31 do
        let testG : integer {0..31} = i; // i has type integer {N..31}
    end
    for i = M to 31 do
        let testH : integer {7..31} = i; // i has type integer {M..31}
    end
    for i = 31 downto N do
        let testI : integer {0..31} = i;
    end
    for i = 31 downto M do
        let testJ : integer {7..31} = i;
    end
end

