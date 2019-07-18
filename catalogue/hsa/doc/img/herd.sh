HERDOPTS="-I img -conf paper.cfg"
MDL=show.cat
SRC=$1
TST=$(basename $SRC .litmus)
case $TST in
  wrc+ldos)
    MOREOPTS="-showraw coh -shift 0.5,0.25,0.0"
    ;;
  isa2+scopes)
    MOREOPTS="-unshow rf,coh,ctrl -xscale 1.9 -showinitwrites false -shift 0,0.33,0.0"
    MDL=scopes.cat
    ;;
  isa2+sso)
    MOREOPTS=" -shift 0.0,0.0,0.0 -unshow rf,hhb,coh,ctrl -yscale 1.667 -xscale 1.9 -showinitwrites false -showthread false"
    MDL=sso.cat
    ;;

  isa2+coh)
    MOREOPTS=" -shift 0.0,0.00,0.0 -yscale 1.667 -xscale 1.9 -showinitwrites true -showthread false"
    MDL=coh.cat
    ;;

  isa2+hhb)
    MOREOPTS=" -shift 0.0,-0.33,0.0 -unshow rf,ctrl -yscale 1.667 -xscale 1.9 -showinitwrites false -showthread false"
    MDL=sso.cat
    ;;

  isa2+instances)
    MOREOPTS="-shift 0.0,0.33,0.0 -showthread false -unshow rf,ctrl -xscale 1.9 -showinitwrites false"
    MDL=instances.cat
    ;;

  sb+wg)
    MOREOPTS="-showthread false -showinitwrites false -unshow SWI,SSYSTEM,SWAVE,SAGENT,hhb -skipchecks ScCohCons -nshow 1"
    MDL=doc.cat
    ;;

  sb+wi)
    MOREOPTS="-showthread false -showinitwrites false -unshow SWG,SSYSTEM,SWAVE,SAGENT,hhb -skipchecks ScCohCons -nshow 1"
    MDL=doc.cat
    ;;
  t2)
    MOREOPTS="-shift 0.0,1.75,0.0 -unshow ldo -showinitwrites false -showthread false -showlegend false -conds img/t2.cond"
    ;;
esac
herd7 $HERDOPTS $MOREOPTS -model $MDL -o img $SRC > /dev/null
