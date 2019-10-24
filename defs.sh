set -o errexit

HERD="herd.native"
LITMUS="litmus.native klitmus.native"
TOOLS="mfind.native moutcomes.native splitcond.native mshowhashes.native mlog2cond.native mflags.native mdiag.native recond.native mcycles.native mmixer.native knames.native mdiff.native mcmp.native madd.native mtopos.native mfilter.native mapply.native mcompare.native mhash.native mrcu.native mprog.native mnames.native ksort.native mobserved.native msort.native msum.native mselect.native mcond.native mproj.native rehash.native splitdot.native mlock.native mtrue.native mlisa2c.native cat2html.native mlog2name.native"
GEN="readRelax.native atoms.native dont.native diycross.native mexpand.native atomize.native diyone.native nexts.native classify.native diy.native norm.native"
JINGLE="jingle.native gen_theme.native"
NATIVE="$HERD $LITMUS $TOOLS $GEN $JINGLE"

mk_exe () {
  D=$1
  shift
  for n
  do
    echo $n | sed -e "s|\(.*\).native|$D/\1.exe|g"
  done
}

EXE="$(mk_exe herd $HERD) $(mk_exe litmus $LITMUS) $(mk_exe gen $GEN) $(mk_exe jingle $JINGLE) $(mk_exe tools $TOOLS)"

cpdir () {
  FROM=$1
  TO=$2
  rm -rf $TO && mkdir -p $TO && ( cd $FROM && cp -r . $TO )
}

VERSION=$(grep "^version:" herdtools7.opam | cut -d ":" -f 2 | cut -d '"' -f 2)
REV=$(git rev-parse HEAD 2>/dev/null || echo exported)
