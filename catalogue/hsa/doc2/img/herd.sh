HERDOPTS="-I img -conf paper.cfg"
MDL=show.cat
SRC=$1
TST=$(basename $SRC .litmus)
case $TST in
  wrc+ldos)
    MOREOPTS="-showraw coh -shift 0.5,0.25,0.0"
    ;;
  isa2+scopes)
    MOREOPTS="-unshow rf -xscale 1.9 -showinitwrites false -shift 0,0.33,0.0"
    MDL=scopes.cat
    ;;
  isa2+sso)
    MOREOPTS=" -shift 0.0,-0.33,0.0 -unshow rf -yscale 1.667 -xscale 1.9 -showinitwrites false -showthread false"
    MDL=sso.cat
    ;;
  isa2+instances)
    MOREOPTS="-shift 0.0,0.33,0.0 -showthread false -unshow rf -xscale 1.9 -showinitwrites false"
    MDL=instances.cat
    ;;
esac
herd7 $HERDOPTS $MOREOPTS -model $MDL -o img $SRC > /dev/null
