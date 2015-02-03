set -e
DIR=`dirname $0`
REPOS=svn+ssh://secsvn@svn-rsem019.cl.cam.ac.uk/herdtools
TMP=/var/tmp
. $DIR/version.sh
. $DIR/funs.sh
NAME=tools-$V
EXPORT=$TMP/export.$$
FINAL=$EXPORT/$NAME
mkdir -p $EXPORT
( cd $EXPORT &&
  svn export -N $REPOS/tools $NAME && \
  svn export -N $REPOS/diy/LICENSE.txt && \
  ( cd $NAME && /bin/rm lib &&  svn export -N $REPOS/lib ) && \
  true )
( cleandir $FINAL )
( cd $EXPORT && tar zcf $NAME.tar.gz $NAME )
( mv $EXPORT/$NAME.tar.gz . && /bin/rm -rf $EXPORT )