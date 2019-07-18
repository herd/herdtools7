/bin/rm -rf run
SCRIPT=./ALL.sh
mkdir -p run
echo '**' BIS
safe -o run/BIS mapply7 -j 16 -com $SCRIPT -comargs -model,doc/doc-bis.cat $@
echo '**' CO
safe -o run/CO mapply7 -j 16 -com $SCRIPT -comargs -model,doc/doc-bis-co.cat $@
echo '**' OPT
safe -o run/OPT mapply7 -j 16 -com $SCRIPT -comargs -model,doc/doc-bis-co-opt.cat $@
echo '**' GIG
safe -o run/GIG mapply7 -j 16 -com $SCRIPT -comargs -model,doc/doc-bis-co-opt-gigogne.cat $@
echo '**' TST
safe -o run/TST mapply7 -j 16 -com $SCRIPT -comargs -model,doc/TST.cat $@

#echo '**' ABS
#safe -o run/ABS mapply7 -j 16 -com $SCRIPT -comargs -model,doc/doc-bis-co-opt-abstract.cat $@
