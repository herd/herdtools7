DIR=$(dirname $0)
$DIR/herd -model uniproc  -gv -show all  -showevents mem -squished true  -webdiagrams true -o /tmp -condensed true -scale 1.2 "$@"