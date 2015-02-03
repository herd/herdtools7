erasetxt ()
{
  for t in *.txt
  do
    case $t in
     LICENSE.txt|CHANGES.txt|README.txt|INSTALL.txt|VERSION.txt)
     ;;
    
     *)
       /bin/rm -f $t
       ;;
    esac
 done
}

erasesrc ()
{
  if [ !  -e _build/$1  ]
  then
     echo "Erasing $1"
    /bin/rm $1
  fi
}

TMPFILE=/tmp/tmp.$$

cleandir ()
{
  d=$1
  cp $DIR/LICENSE.txt $d && \
  cd $d && \
  sed -e 's|PREFIX=$$HOME|PREFIX=/usr/local|' -e 's|EXTRAPROGS=.*|EXTRAPROGS=|' Makefile > /tmp/tmp.$$ && mv $TMPFILE Makefile && \
  make && \
  erasetxt && \
  if [ -d lib -a ! \( -h lib \) ]
  then
    for f in lib/*.ml lib/*.ml[iyl]
    do
      erasesrc $f
    done
  fi && \
  for f in *.ml *.ml[iyl]
  do
    erasesrc $f
  done && make clean
}

extract ()
{
  FROM=$1
  FROMD=$(dirname $FROM)
  FROMB=$(basename $FROM)
  TO=$2
  TOD=$(dirname $TO)
  TOB=$(basename $TO)
  ( cd $DIR/../$FROMD && git archive --format=tar HEAD $FROMB ) | \
  ( cd $EXPORT && mkdir -p $TOD && cd $TOD && tar xmf - && \
  ( mv $FROMB $TOB 2> /dev/null || true ) &&
  if test -f $TOB/.unreleased
  then
    for f in $(cat $TOB/.unreleased)
    do
      /bin/rm -rf $TOB/$f
    done
    /bin/rm -f  $TOB/.unreleased
  fi )
}
