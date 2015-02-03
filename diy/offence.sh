set -e
DIR=`dirname $0`
REPOS=svn+ssh://secsvn@svn-sem.cl.cam.ac.uk/WeakMemory
REPOSOFF=svn+ssh://yquem.inria.fr/home/yquem/moscova/maranget/repos
TMP=/var/tmp
V=0.01
. $DIR/funs.sh
EXPORT=$TMP/exp.$$
mkdir $EXPORT
( cd $EXPORT && svn export -N $REPOS/mem.new && \
   svn export -N $REPOS/gen.new && \
  svn export -N $REPOS/litmus.new/plumbing && \
  mkdir litmus.new && mv plumbing litmus.new  && \
  svn export -N $REPOSOFF/offence )
NAME=offence-$V
FINAL=$TMP/$NAME
/bin/rm -rf $FINAL && mkdir $FINAL
( cd $EXPORT/offence && tar chf - . ) | \
( cd $FINAL && mkdir src && cd src && tar xmf - . ) && \
/bin/rm -rf $EXPORT
( cleandir $FINAL/src )
##La doc
DOC=$DIR/../offence/doc
( cd $DOC && make all install INSTALLDIR=$FINAL )
(
cat <<EOF
This is offence $V, contents:
  -  sources, cf. src/README.txt
  -  documentation, cf. doc/index.html
EOF
) > $FINAL/README.txt
( cd $TMP && tar zcf $NAME.tar.gz $NAME )
( mv $TMP/$NAME.tar.gz . && /bin/rm -rf $FINAL )
DEST=~alglave/public_html/diy/offence
mv $NAME.tar.gz $DEST
(cd $DEST && tar zxmf $NAME.tar.gz && rm -rf doc && mv $NAME/doc . && rm -rf $NAME )