OPTS="-showupto 1 -rfdefault generated 	 -progtexdefault generated -show_po false  -show_po_edges false -longlegend false -show_simple_vbconds true -squished_graphs true -show_events mem -show_final_rf false -show_fr true -show_poloc false -proc_or_thread false  -condensed true"
MOREOPTS="-web_diagrams true -splat_kind true -model minimaluniproc -select_vos all -show_local_global false  -axiom no -precise_pco false -speedcheck true -speedlist true"
FILE=$1
memevents ${OPTS} ${MOREOPTS} $FILE
BASE=$(basename $FILE .litmus)
sed -e 's/pad="0.0"/pad="0.2"/g'  generated/$BASE-rf.dot | tee $BASE.dot |\
neato -Tpng  > img/$BASE.png
