/START _litmus_P/ { print $0 }
/_litmus_P[0-9]+_[0-9]+/ { getline; print $0 ; }
