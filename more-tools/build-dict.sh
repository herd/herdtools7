#! /bin/sh
TMP=/tmp/build.$$
trap "/bin/rm -rf $TMP ; exit 2"  INT QUIT
DEFS=../herd/libdir/catdefinitions.tex
E1='\\#1'
E2='\\#2'
mkdir -p $TMP
grep -e \\[2\\] $DEFS | grep -v emph | grep newcommand |\
    sed -e 's/\\.*command{\([^}]*\)}.*/\1/g' |\
    while read command
    do
        cat <<EOF
"$command","\\$command{\{0\}}{\{1\}}";
\\par
EOF
    done  > $TMP/T.tex
grep -e \\[1\\] $DEFS | grep -v emph | grep newcommand |\
    sed -e 's/\\.*command{\([^}]*\)}.*/\1/g' |\
    while read command
    do
        cat <<EOF
"$command",
"\\$command{\{0\}}";
\\par
EOF
    done  > $TMP/U.tex
echo let relations = [
pandoc $DEFS $TMP/T.tex --wrap=none --to markdown | sed -e 's/ //g' -e 's/\\"/"/g' | grep -v '^$'
echo ]
echo
echo let sets = [
pandoc $DEFS $TMP/U.tex --wrap=none --to markdown | sed -e "s/ //g" -e 's/\\"/"/g' | grep -v '^$'
echo ]
/bin/rm -rf $TMP
