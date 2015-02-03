DIR=$(dirname $0)
HERD=$1
TAG=$2
FILE=$3
GEN=/tmp/herd.$$
max=${MAX:-1000}
maxone=$(expr $max - 1)
mkdir -p $GEN
BASE=$(basename $FILE .litmus)
cat <<EOF
\def\csname images-$TAG-$BASE\endcsname{
EOF
splitcond ${FILE} -o ${GEN} |\
while read cond
do
SUF="+$(basename $cond .txt)"
COND=${GEN}/$cond
cat <<EOF
\begin{verbatim}
EOF
echo -n 'Executions for behaviour: '
awk -F\" '{print $2}' $COND | sed -e 's|(|"|' -e 's|)|"|' -e 's|/\\|;|g'
cat <<EOF
\end{verbatim}
EOF
MORE="-strictskip false"
${HERD} -conds ${COND} -suffix ${SUF} ${MORE} -web -showobserved true -show prop -nshow $max -speedcheck true -o ${GEN} ${FILE} >/dev/null 2>&1
mkdir -p $GEN
mkdir -p $TAG
splitdot -max $maxone $GEN/${BASE}${SUF}.dot |
while read name
do
  case $name in
    *.dot)
      B=$(basename $name .dot)
      D=$(dirname $name)
      F=$TAG/$B.png
      printf " \imgsrc{%s}" $F
      neato -s0.8 -Tpng  $name > $F 2>/dev/null
      /bin/rm $name
     ;;
    *)
      printf "\ldots"
     ;;
   esac
done
done
echo '}%'
rm -rf $GEN