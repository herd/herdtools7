#!/bin/sh
#The purpose of this script is to generate tests in diy7 by segmenting a config file.

nprocs="2"
csize="4"
seg_size=5 # this is amount of segments of the config file to be generated
timeout=0 #timeout in seconds. put 0 for no timeout
oneloc=0 #set to 1 for oneloc option to be turned on



# initiating variables. $file contains all relaxations to be randomised, filename is the name of the config file and relaxations is all of the relaxations.
file="DpAddrdR DpAddrdW DpAddrsR DpAddrsW DpDatadW DpDatasW DpCtrldW DpCtrlsW DpCtrlIsbdR [DpAddrdR, ISBd*R] [DpAddrdW, ISBd*R] DpCtrlIsbsR [DpAddrsR, ISBs*R] [DpAddrsW, ISBs*R] [DpAddrsR, ISBd*R] [DpAddrsW, ISBd*R] [DpAddrdR, ISBs*R] [DpAddrdW, ISBs*R] [DpAddrdR, Pod*W] [DpAddrdW, Pod*W] [DpAddrsR, Pos*W] [DpAddrsW, Pos*W] [DpAddrsR, Pod*W] [DpAddrsW, Pod*W] [DpAddrdR, Pos*W] [DpAddrdW, Pos*W] [DpAddrdW, Rfi] [DpDatadW, Rfi] [DpAddrsW, Rfi] [DpDatasW, Rfi] DMB.SYd** DMB.SYs** PodWRLA PosWRLA DMB.LDdR* DMB.LDsR* PodR*AP PodR*QP PosR*AP PosR*QP PodRRAA PodRRQA PosRRAA PosRRQA PodRRAQ PodRRQQ PosRRAQ PosRRQQ PodRWAL PodRWQL PosRWAL PosRWQL DMB.STdWW DMB.STsWW Pod*WPL Pos*WPL PodWWLL PosWWLL Pos*W LxSx LxSxAP LxSxPL LxSxAL [LxSx, RfiPA] [LxSx, RfiPQ] [LxSxAP, RfiPA] [LxSxAP, RfiPQ] [LxSxPL, RfiLA] [LxSxPL, RfiLQ] [LxSxAL, RfiLA] [LxSxAL, RfiLQ] Amo.Swp Amo.Cas Amo.LdAdd Amo.LdEor Amo.LdClr Amo.LdSet Amo.StAdd Amo.StEor Amo.StClr Amo.StSet Amo.SwpAP Amo.CasAP Amo.LdAddAP Amo.LdEorAP Amo.LdClrAP Amo.LdSetAP Amo.SwpPL Amo.CasPL Amo.LdAddPL Amo.LdEorPL Amo.LdClrPL Amo.LdSetPL Amo.StAddPL Amo.StEorPL Amo.StClrPL Amo.StSetPL Amo.SwpAL Amo.CasAL Amo.LdAddAL Amo.LdEorAL Amo.LdClrAL Amo.LdSetAL [Amo.Swp, RfiPA] [Amo.Cas, RfiPA] [Amo.LdAdd, RfiPA] [Amo.LdEor, RfiPA] [Amo.LdClr, RfiPA] [Amo.LdSet, RfiPA] [Amo.StAdd, RfiPA] [Amo.StEor, RfiPA] [Amo.StClr, RfiPA] [Amo.StSet, RfiPA] [Amo.Swp, RfiPQ] [Amo.Cas, RfiPQ] [Amo.LdAdd, RfiPQ] [Amo.LdEor, RfiPQ] [Amo.LdClr, RfiPQ] [Amo.LdSet, RfiPQ] [Amo.StAdd, RfiPQ] [Amo.StEor, RfiPQ] [Amo.StClr, RfiPQ] [Amo.StSet, RfiPQ] [Amo.SwpAP, RfiPA] [Amo.CasAP, RfiPA] [Amo.LdAddAP, RfiPA] [Amo.LdEorAP, RfiPA] [Amo.LdClrAP, RfiPA] [Amo.LdSetAP, RfiPA] [Amo.SwpAP, RfiPQ] [Amo.CasAP, RfiPQ] [Amo.LdAddAP, RfiPQ] [Amo.LdEorAP, RfiPQ] [Amo.LdClrAP, RfiPQ] [Amo.LdSetAP, RfiPQ] [Amo.SwpPL, RfiLA] [Amo.CasPL, RfiLA] [Amo.LdAddPL, RfiLA] [Amo.LdEorPL, RfiLA] [Amo.LdClrPL, RfiLA] [Amo.LdSetPL, RfiLA] [Amo.StAddPL, RfiLA] [Amo.StEorPL, RfiLA] [Amo.StClrPL, RfiLA] [Amo.StSetPL, RfiLA] [Amo.SwpPL, RfiLQ] [Amo.CasPL, RfiLQ] [Amo.LdAddPL, RfiLQ] [Amo.LdEorPL, RfiLQ] [Amo.LdClrPL, RfiLQ] [Amo.LdSetPL, RfiLQ] [Amo.StAddPL, RfiLQ] [Amo.StEorPL, RfiLQ] [Amo.StClrPL, RfiLQ] [Amo.StSetPL, RfiLQ] [Amo.SwpAL, RfiLA] [Amo.CasAL, RfiLA] [Amo.LdAddAL, RfiLA] [Amo.LdEorAL, RfiLA] [Amo.LdClrAL, RfiLA] [Amo.LdSetAL, RfiLA] [Amo.SwpAL, RfiLQ] [Amo.CasAL, RfiLQ] [Amo.LdAddAL, RfiLQ] [Amo.LdEorAL, RfiLQ] [Amo.LdClrAL, RfiLQ] [Amo.LdSetAL, RfiLQ] [Pod**, Amo.SwpAL] [Pod**, Amo.CasAL] [Pod**, Amo.LdAddAL] [Pod**, Amo.LdEorAL] [Pod**, Amo.LdClrAL] [Pod**, Amo.LdSetAL] [Pos**, Amo.SwpAL] [Pos**, Amo.CasAL] [Pos**, Amo.LdAddAL] [Pos**, Amo.LdEorAL] [Pos**, Amo.LdClrAL] [Pos**, Amo.LdSetAL] [Amo.SwpAL, Pod**] [Amo.CasAL, Pod**] [Amo.LdAddAL, Pod**] [Amo.LdEorAL, Pod**] [Amo.LdClrAL, Pod**] [Amo.LdSetAL, Pod**] [Amo.SwpAL, Pos**] [Amo.CasAL, Pos**] [Amo.LdAddAL, Pos**] [Amo.LdEorAL, Pos**] [Amo.LdClrAL, Pos**] [Amo.LdSetAL, Pos**] [Pod**, Amo.SwpAL, Pod**] [Pod**, Amo.CasAL, Pod**] [Pod**, Amo.LdAddAL, Pod**] [Pod**, Amo.LdEorAL, Pod**] [Pod**, Amo.LdClrAL, Pod**] [Pod**, Amo.LdSetAL, Pod**] [Pos**, Amo.SwpAL, Pos**] [Pos**, Amo.CasAL, Pos**] [Pos**, Amo.LdAddAL, Pos**] [Pos**, Amo.LdEorAL, Pos**] [Pos**, Amo.LdClrAL, Pos**] [Pos**, Amo.LdSetAL, Pos**]DpAddrdR DpAddrdW DpAddrsR DpAddrsW DpDatadW DpDatasW DpCtrldW DpCtrlsW DpCtrlIsbdR [DpAddrdR, ISBd*R] [DpAddrdW, ISBd*R] DpCtrlIsbsR [DpAddrsR, ISBs*R] [DpAddrsW, ISBs*R] [DpAddrsR, ISBd*R] [DpAddrsW, ISBd*R] [DpAddrdR, ISBs*R] [DpAddrdW, ISBs*R] [DpAddrdR, Pod*W] [DpAddrdW, Pod*W] [DpAddrsR, Pos*W] [DpAddrsW, Pos*W] [DpAddrsR, Pod*W] [DpAddrsW, Pod*W] [DpAddrdR, Pos*W] [DpAddrdW, Pos*W] [DpAddrdW, Rfi] [DpDatadW, Rfi] [DpAddrsW, Rfi] [DpDatasW, Rfi] DMB.SYd** DMB.SYs** PodWRLA PosWRLA DMB.LDdR* DMB.LDsR* PodR*AP PodR*QP PosR*AP PosR*QP PodRRAA PodRRQA PosRRAA PosRRQA PodRRAQ PodRRQQ PosRRAQ PosRRQQ PodRWAL PodRWQL PosRWAL PosRWQL DMB.STdWW DMB.STsWW Pod*WPL Pos*WPL PodWWLL PosWWLL Pos*W LxSx LxSxAP LxSxPL LxSxAL [LxSx, RfiPA] [LxSx, RfiPQ] [LxSxAP, RfiPA] [LxSxAP, RfiPQ] [LxSxPL, RfiLA] [LxSxPL, RfiLQ] [LxSxAL, RfiLA] [LxSxAL, RfiLQ] Amo.Swp Amo.Cas Amo.LdAdd Amo.LdEor Amo.LdClr Amo.LdSet Amo.StAdd Amo.StEor Amo.StClr Amo.StSet Amo.SwpAP Amo.CasAP Amo.LdAddAP Amo.LdEorAP Amo.LdClrAP Amo.LdSetAP Amo.SwpPL Amo.CasPL Amo.LdAddPL Amo.LdEorPL Amo.LdClrPL Amo.LdSetPL Amo.StAddPL Amo.StEorPL Amo.StClrPL Amo.StSetPL Amo.SwpAL Amo.CasAL Amo.LdAddAL Amo.LdEorAL Amo.LdClrAL Amo.LdSetAL [Amo.Swp, RfiPA] [Amo.Cas, RfiPA] [Amo.LdAdd, RfiPA] [Amo.LdEor, RfiPA] [Amo.LdClr, RfiPA] [Amo.LdSet, RfiPA] [Amo.StAdd, RfiPA] [Amo.StEor, RfiPA] [Amo.StClr, RfiPA] [Amo.StSet, RfiPA] [Amo.Swp, RfiPQ] [Amo.Cas, RfiPQ] [Amo.LdAdd, RfiPQ] [Amo.LdEor, RfiPQ] [Amo.LdClr, RfiPQ] [Amo.LdSet, RfiPQ] [Amo.StAdd, RfiPQ] [Amo.StEor, RfiPQ] [Amo.StClr, RfiPQ] [Amo.StSet, RfiPQ] [Amo.SwpAP, RfiPA] [Amo.CasAP, RfiPA] [Amo.LdAddAP, RfiPA] [Amo.LdEorAP, RfiPA] [Amo.LdClrAP, RfiPA] [Amo.LdSetAP, RfiPA] [Amo.SwpAP, RfiPQ] [Amo.CasAP, RfiPQ] [Amo.LdAddAP, RfiPQ] [Amo.LdEorAP, RfiPQ] [Amo.LdClrAP, RfiPQ] [Amo.LdSetAP, RfiPQ] [Amo.SwpPL, RfiLA] [Amo.CasPL, RfiLA] [Amo.LdAddPL, RfiLA] [Amo.LdEorPL, RfiLA] [Amo.LdClrPL, RfiLA] [Amo.LdSetPL, RfiLA] [Amo.StAddPL, RfiLA] [Amo.StEorPL, RfiLA] [Amo.StClrPL, RfiLA] [Amo.StSetPL, RfiLA] [Amo.SwpPL, RfiLQ] [Amo.CasPL, RfiLQ] [Amo.LdAddPL, RfiLQ] [Amo.LdEorPL, RfiLQ] [Amo.LdClrPL, RfiLQ] [Amo.LdSetPL, RfiLQ] [Amo.StAddPL, RfiLQ] [Amo.StEorPL, RfiLQ] [Amo.StClrPL, RfiLQ] [Amo.StSetPL, RfiLQ] [Amo.SwpAL, RfiLA] [Amo.CasAL, RfiLA] [Amo.LdAddAL, RfiLA] [Amo.LdEorAL, RfiLA] [Amo.LdClrAL, RfiLA] [Amo.LdSetAL, RfiLA] [Amo.SwpAL, RfiLQ] [Amo.CasAL, RfiLQ] [Amo.LdAddAL, RfiLQ] [Amo.LdEorAL, RfiLQ] [Amo.LdClrAL, RfiLQ] [Amo.LdSetAL, RfiLQ] [Pod**, Amo.SwpAL] [Pod**, Amo.CasAL] [Pod**, Amo.LdAddAL] [Pod**, Amo.LdEorAL] [Pod**, Amo.LdClrAL] [Pod**, Amo.LdSetAL] [Pos**, Amo.SwpAL] [Pos**, Amo.CasAL] [Pos**, Amo.LdAddAL] [Pos**, Amo.LdEorAL] [Pos**, Amo.LdClrAL] [Pos**, Amo.LdSetAL] [Amo.SwpAL, Pod**] [Amo.CasAL, Pod**] [Amo.LdAddAL, Pod**] [Amo.LdEorAL, Pod**] [Amo.LdClrAL, Pod**] [Amo.LdSetAL, Pod**] [Amo.SwpAL, Pos**] [Amo.CasAL, Pos**] [Amo.LdAddAL, Pos**] [Amo.LdEorAL, Pos**] [Amo.LdClrAL, Pos**] [Amo.LdSetAL, Pos**] [Pod**, Amo.SwpAL, Pod**] [Pod**, Amo.CasAL, Pod**] [Pod**, Amo.LdAddAL, Pod**] [Pod**, Amo.LdEorAL, Pod**] [Pod**, Amo.LdClrAL, Pod**] [Pod**, Amo.LdSetAL, Pod**] [Pos**, Amo.SwpAL, Pos**] [Pos**, Amo.CasAL, Pos**] [Pos**, Amo.LdAddAL, Pos**] [Pos**, Amo.LdEorAL, Pos**] [Pos**, Amo.LdClrAL, Pos**] [Pos**, Amo.LdSetAL, Pos**]"
filename="Forbidden-Mixed-MP4"
relaxations="Rfe Fre Wse Hat $file"

# make file into array and initialise variables
IFS=' ' read -r -a array <<< "$file"
stringRelax=()
temp=""

# create new array that respects square brackets
for one_thing in "${array[@]}"; do
    if [ "${one_thing: -1}" = "," ]
    then
        temp+="$one_thing"
    else
        if [ -z "$temp" ]
        then
            stringRelax+=("$one_thing")
        else
            stringRelax+=("$temp$one_thing")
            temp=""
        fi
    fi
done

# randomise function
shuffle() {
   local i tmp size max rand

   # $RANDOM % (i+1) is biased because of the limited range of $RANDOM
   # Compensate by using a range which is a multiple of the array size.
   size=${#array2[*]}
   max=$(( 32768 / size * size ))

   for ((i=size-1; i>0; i--)); do
      while (( (rand=$RANDOM) >= max )); do :; done
      rand=$(( rand % (i+1) ))
      tmp=${array2[i]} array2[i]=${array2[rand]} array2[rand]=$tmp
   done
}

# do the shuffling
array2=("${stringRelax[@]}")
shuffle

rm -rf src && mkdir src
total_size=${#array2[@]}
((relax_size=$total_size/$seg_size))


if [ $oneloc -eq 1 ]
then
    ol="-oneloc"
else
    ol=""
fi




# create the config files
whole=""
for ((i=0; i<${seg_size}; i++)); do
    cd src && rm -rf src${i} && mkdir src${i} && cd ..
    for aspect in "${array2[@]:(i*${relax_size}+1):${relax_size}}"; do
        whole+="$aspect "
    done
    echo "-arch AArch64

-nprocs $nprocs
-eprocs
-size $csize

$ol

-name Armv8-MIXED-ext-forbidden-$i-

-relaxlist Rfe Fre Wse Hat $whole" >./src/src${i}/${filename}${i}.conf
    whole=""
done

# run diy7
for ((i=0; i<${seg_size}; i++)); do
    if [ $timeout -eq 0 ]
    then
        diy7 -o ./src/src${i} -conf ./src/src${i}/${filename}${i}.conf
    else
        diy7 -o ./src/src${i} -conf ./src/src${i}/${filename}${i}.conf& read -t $timeout || kill $!
    fi
done



for ((i=0; i<${seg_size}; i++)); do
    echo "./src/src${i}/@all" >>@all
done
