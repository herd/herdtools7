set -o errexit

HERD="herd.native"
LITMUS="litmus.native klitmus.native"
TOOLS="mfind.native moutcomes.native splitcond.native mshowhashes.native mlog2cond.native mflags.native mdiag.native recond.native mcycles.native mmixer.native knames.native mdiff.native mcmp.native madd.native mtopos.native mfilter.native mapply.native mcompare.native mhash.native mrcu.native mprog.native mnames.native ksort.native mobserved.native msort.native msum.native mselect.native mcond.native mproj.native rehash.native splitdot.native mlock.native mtrue.native mlisa2c.native"
GEN="readRelax.native atoms.native dont.native diycross.native mexpand.native atomize.native diyone.native nexts.native classify.native diy.native"
JINGLE="jingle.native"
NATIVE="$HERD $LITMUS $TOOLS $GEN $JINGLE"

cpdir () {
  FROM=$1
  TO=$2
  rm -rf $TO && mkdir -p $TO && ( cd $FROM && rsync -r . $TO )
}

VERSION=$(grep "^version:" opam | cut -d ":" -f 2 | cut -d '"' -f 2)
REV=$(git rev-parse HEAD 2>/dev/null || echo exported)
