DEST=$HOME/public_html/diy/sources
erasetxt ()
{
  for t in *.txt
  do
    case $t in
     LICENSE.txt|README.txt|INSTALL.txt|VERSION.txt|header.txt)
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
      case $(basename $f) in
      check402.ml) ;;
      *) erasesrc $f ;;
      esac
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
  ( cd $DIR/../$FROMD && svn export $FROMB $EXPORT/$TO ) && \
  ( cd $EXPORT && \
  if test -f $TO/.unreleased
  then
    for f in $(cat $TO/.unreleased)
    do
      /bin/rm -rf $TO/$f
    done
    /bin/rm -f  $TO/.unreleased
  fi )
}
