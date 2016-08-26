BEGIN {
    found = 0;
}
/START _litmus_P/ { found = 1 ; print $0 }
!/^[[:space:]]*[#@]/ && !/^[[:space:]]*$/  && found == 1 { print $0 }
/END _litmus_P/ { found = 0 }
