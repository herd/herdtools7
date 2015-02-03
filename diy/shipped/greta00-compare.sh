#! /bin/sh
#To compare log with model: sh compare.sh log
LOG=$1
ONE=/tmp/one.$$
TWO=/tmp/two.$$
build () {
  INP=$1
  OUT=$2
  grep Observation $INP |\
  while read obs name result rem
  do
   echo $name $result
  done > $OUT
}
(
cat <<EOF
Observation MP+dmb+addr Never 0 3 
Observation MP+po+addr Sometimes 1 3 
Observation MP Sometimes 1 3 
Observation LB Sometimes 1 3 
Observation MP+dsb+ctrlisb Never 0 3 
Observation LB+datas Never 0 3 
Observation MP+dmb+ctrl Sometimes 1 3 
Observation SB+dsbs Never 0 3 
Observation MP+dsb+addr Never 0 3 
Observation LB+addrs Never 0 3 
Observation MP+dsb+ctrl Sometimes 1 3 
Observation SB+dmbs Never 0 3 
Observation S+dmbs Never 0 3 
Observation MP+dmb+rs Sometimes 1 3 
Observation LB+dmbs Never 0 3 
Observation LB+dsbs Never 0 3 
Observation R+dsbs Never 0 3 
Observation RSW Sometimes 1 3 
Observation SB Sometimes 1 3 
Observation R+dmbs Never 0 3 
Observation R Sometimes 1 3 
Observation PPOAA Never 0 3 
Observation MP+dmbs Never 0 3 
Observation 2+2W Sometimes 1 3 
Observation S Sometimes 1 3 
Observation LB+rs Sometimes 1 3 
Observation RDWI Sometimes 1 3 
Observation MP+dmb+ctrlisb Never 0 3 
Observation MP+nondep+dmb Sometimes 1 3 
Observation MP+dsbs Never 0 3 
Observation LB+ctrls Never 0 3 
Observation S+dsbs Never 0 3 
Observation 2+2W+dmbs Never 0 3 
Observation PPOCA Sometimes 1 3 
Observation DataRW Sometimes 1 3 
Observation AddrRW Never 0 3 
Observation DataWW Sometimes 1 3 
Observation AddrWW Never 0 3 
Observation LB+addrs+WW Never 0 3 
Observation LB+datas+WW Sometimes 1 3 
Observation MP+dmb+fri-rfi-ctrlisb Never 0 6 
Observation MP+dmb+fri-rfi-addr Sometimes 1 6 
Observation LB+addrs+RW Never 0 3 
Observation CO-LB Never 0 3 
Observation CO-SB Never 0 3 
Observation CO-SBI Always 5 0 
Observation CO-MP Never 0 6 
Observation CO-MP+dmb+po Never 0 6 
Observation CO-MP+dmbs Always 6 0
Observation CO-2+2W Never 0 1 
Observation CO-R Never 0 4 
Observation CO-R+dmb+po Never 0 4 
Observation CO-S Never 0 5 
Observation CO-S+dmb+po Never 0 5 
Observation CO-LB+fri+pos-fri Always 5 0 
Observation CO-S+wsi+pos-fri Always 9 0 
Observation CO-MP+delay+po Never
Observation CO-MP+dmb+loop Never
Observation CO-SB+delays Never
EOF
) | build '' $ONE
build $LOG $TWO
sort $ONE |\
while read name obs
do
   RT=`grep -e "^$name " $TWO`
   if [ "$RT" != "" ]
   then       
      obs2=`echo $RT | awk '{print $2}'`
      if [ $obs = $obs2 ]
      then
          echo -n '   '
      else
        if [ $obs != Sometimes ]
        then
          echo -n '>* '
        else
          echo -n '>  '
        fi
     fi
     echo $name model=$obs log=$obs2
   fi
done
/bin/rm -f $ONE $TWO