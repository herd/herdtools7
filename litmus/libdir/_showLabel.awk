BEGIN {
    found = 0;
}
/START_litmus_P/ { found = 1 ; print $0 }
!/^[[:space:]]*[#@]/ && !/^[[:space:]]*$/  && !/InlineAsm/ && !/START_litmus/ && !/END_litmus/ && found == 1 { print $0 }
/END_litmus_P/ { found = 0 }
