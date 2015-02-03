#! /bin/sh -x
#######################
DIR=`dirname $0`
MNAMES=mnames
MSORT=msort
COMPARE=mcompare
MAPPLY=mapply
#######################
TMP=/tmp/gen.$$
mkdir -p $TMP
TAG=$1
shift
INP=$1
shift
SRC=$TAG-src
TIDX=@$$
msort $INP > $TIDX
mkdir -p $SRC
cat <<EOF
\input{$TAG-img/index.tex}%
EOF
$MNAMES $TIDX |\
while read src name
do
dst=$(basename $src .litmus)
cp $src $SRC/$dst.litmus
cat <<EOF
\def\csname $name@base\endcsname{$TAG-$dst}%
\def\csname $name@img\endcsname{\csname images-$TAG-img-$dst\endcsname}%
\def\csname $name@src\endcsname{$SRC/$dst.litmus}%
\gentest{$name}%
EOF
(
cat <<EOF
$name
EOF
) >> $TMP/list.tex
done
if test -f $TMP/list.tex
then
cat<<EOF
\def\\$TAG{%
EOF
pp () {
  name=$1
cat <<EOF
$pre\ahref{\base{$name}.html}{\textsf{$name}}%
EOF
pre=", "
}
head --lines=-1 $TMP/list.tex |\
while read name
do
  pp $name
done
tail --lines=1  $TMP/list.tex |\
while read name
do
case $name in
"")
;;
*)
pre="{} and "
pp $name
;;
esac
done
cat <<EOF
}%
EOF
cat<<EOF
\def\\apply${TAG}#1{%
EOF
while read name
do
cat <<EOF
#1{$name}%
EOF
done < $TMP/list.tex
cat <<EOF
}%
EOF
fi
/bin/rm -rf $TMP $TIDX