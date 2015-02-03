#! /bin/sh

DIRS="herd gen tools litmus"
COM="$@"
case $COM in
  svn*)
   echo "Do svn directly!!!"
   exit 2
   ;;
esac

for d in $DIRS
do
  echo '**' $d '**'
  ( cd $d && $COM )
done
