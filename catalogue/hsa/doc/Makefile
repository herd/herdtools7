FILE=hsa-july
DVIPS=dvips
LATEX=rubber -f
XDVI=xdvi -s 7 -paper a4 -expert
EDITOR=emacs
GV=gv
LPR=lpr
HEVEA=hevea
HEVEAOPTS=-fix
HACHA=hacha
HACHAOPTS=-tocbis
HSA=01 02 03 04 05 06 07 09 10 11 12 13 14
HSAFIG=$(HSA:%=img/HSA%.fig)
#T1=img/t1-00.fig img/t1-01.fig 
NEWFIG=img/lb+ldos.fig img/wrc+ldos.fig\
 img/isa2+scopes.fig  img/isa2+instances.fig img/isa2+sso.fig \
 img/isa2+hhb.fig  img/isa2+coh.fig \
 img/sb.fig img/sb+wg.fig img/sb+wi.fig \
 img/t2.fig img/t2+hhb.fig img/t2+agent.fig img/t2+scope.fig  \
 img/t2+wg.fig \
 $(T1) \
 img/t3.fig  img/t3+fst.fig \
 $(HSAFIG) img/HSA07+BIS.fig  img/HSA14+SCOPE.fig img/HSA12+fences.fig

HSALISTING=$(HSA:%=img/HSA%.litmus)
LISTING=img/t1-alt.litmus $(HSALISTING)


#img/mp.fig \
 img/coww.fig img/corw1.fig img/corw2.fig img/cowr.fig img/corr.fig \
 img/lb+ppos.fig img/lb+addrs+ww.fig\
 img/mp+lwfence+ppo.fig img/wrc+lwfence+ppo.fig \
 img/isa2+lwfence+ppos.fig img/sb+ffences.fig \
 img/2+2w+lwfences.fig \
 img/wrw+2w+lwsyncs.fig img/rwc+syncs.fig\
 img/s+lwfence+ppo.fig img/r+ffences.fig \
 img/iriw+ffences.fig \
 img/A-cumul.fig img/B-cumulativity.fig \
 img/AB-cumul.fig \

DOT=img/mp-cf.dot img/mp-df1.dot img/mp-df2.dot img/mp-df3.dot img/mp-df4.dot \
#img/s-A-cumul1.dot img/s-A-cumul2.dot \
 img/fr.dot img/atom.dot \

#DDD=ins/addr.ddd ins/data.ddd ins/ctrl.ddd 
COMMON=macros.tex hsa.bib 
DEPENDS=$(COMMON) hsa-july.tex $(FIG) newfig $(DIAGRAMS) $(LISTING)

all: ps pdf

$(FILE).dvi: $(DEPENDS)
color.dvi: color.tex $(DEPENDS)

default: $(FILE).dvi
allfig: $(FIG) newfig
epsfig: allfig $(FIG:.pstex_t=.eps) $(NEWFIG:.fig=.eps) $(DOT:.dot=.eps) $(DDD:.ddd=.eps)

ps: $(FILE).ps

pdf: $(FILE).ps
	ps2pdf $(FILE).ps

broch: $(FILE).2.ps

gv: $(FILE).ps
	$(GV) $(FILE).ps&

lpr: $(FILE).ps
	$(LPR) $(FILE).ps

bib: $(FILE).dvi
	$(LATEX) $(FILE).tex
	bibtex $(FILE)
	$(LATEX) $(FILE).tex

force:
	$(LATEX) $(FILE).tex

index.html: $(FILE).html
	$(HACHA) $(HACHAOPTS) $(FILE).html

$(FILE).html: $(DEPENDS) sig-alt-full.hva
	$(HEVEA) $(HEVEAOPTS) $(FILE)

clean::
	/bin/rm -f *.aux *.dvi *.log *.dep *.bbl *.blg $(FILE).pdf $(FILE).ps
	/bin/rm -f log
	/bin/rm -f *.advi
	/bin/rm -f *.png
	$(RM) $(FIG:.pstex_t=.eps) $(NEWFIG:.fig=.eps) $(DOT:.dot=.eps) 
	$(RM) $(NEWFIG) $(DOT:.dot=.fig) 
	$(RM) *.out

%.dvi: %.tex
	$(LATEX) $*.tex || /bin/rm -f $*.dvi

%.advi: %.dvi
	dvicopy $< $@

%.ps: %.dvi
	$(DVIPS) -o $*.ps $*.dvi

%.eps: %.fig
	./figtex2eps $<

%.eps: %.tex
	latex $<
	$(DVIPS) -o $*.eps $*.dvi

%.2.ps: %.ps
	brochure $*.ps

%.png: %.dot
	neato -Tpng $*.dot > $*.png && qiv $*.png && /bin/rm -f $*.png 

A=$(FIG:.pstex_t=.pstex)
B=$(FIG:.pstex_t=.fig)
clean::
	/bin/rm -f $(FIG) $(A) $(B)
##Vertical listing
#img/t1-alt.litmus: img/t1.litmus
#	mprog7 -mode text -transpose true $< > $@
#
#clean::
#	/bin/rm $(LISTING)

##New figures

NEWDOT=$(NEWFIG:.fig=.dot)
.PRECIOUS: $(NEWDOT)

NEWPSTEXT=$(NEWFIG:.fig=.pstex_t) $(DOT:.dot=.pstex_t) $(DDD:.ddd=.pstex_t)
NEWPSTEX= $(NEWFIG:.fig=.pstex) $(DOT:.dot=.pstex) $(DDD:.ddd=.pstex)

#Specific .dot
HERD=herd7
T2FMT=-showlegend false -shift 0.0,1.75,0.0 -showthread false -showinitwrites false
img/t2+hhb.dot: img/t2.litmus
	$(HERD) -suffix +hhb -conf img/paper.cfg $(T2FMT) -unshow SWI,SWG,SWAVE,SAGENT,SSYSTEM,ldo -bell img/hsa.bell -model doc.cat -skipchecks HhbCohCons -o img img/t2.litmus >/dev/null

img/t2+agent.dot: img/t2.litmus img/t2.cond
	$(HERD) -suffix +agent -conf img/paper.cfg $(T2FMT) -unshow SWI,SWG,SWAVE,SSYSTEM,ldo,rf -bell img/hsa.bell -model doc.cat -skipchecks HhbCohCons,ScCohCons -conds img/t2.cond -o img img/t2.litmus >/dev/null

img/t2+scope.dot: img/t2.litmus img/t2.cond
	$(HERD) -suffix +scope -conf img/paper.cfg $(T2FMT) -shift 0.0,1.5,0.0 -unshow rf,same-wg -bell img/hsa.bell -model img/instances.cat -skipchecks HhbCohCons,ScCohCons -conds img/t2.cond -o img img/t2.litmus >/dev/null

img/t2+wg.dot:  img/t2.litmus img/t2.cond
	$(HERD) -suffix +wg -conf img/paper.cfg  $(T2FMT) -unshow rf,SWI,SAGENT,SWAVE,SSYSTEM,ldo -bell img/hsa.bell -model doc-bis.cat -skipchecks HhbCohCons,ScCohCons -conds img/t2.cond  img/t2.litmus  -show prop -o img >/dev/null

T3FMT=-nshow 1 -showlegend false -shift 0.0,1.5,0.0 -showthread false -showinitwrites false
img/t3.dot:  img/t3.litmus
	$(HERD) -conf img/paper.cfg  $(T3FMT) -unshow hhb,rf,SWI,SWAVE,SSYSTEM,ldo -bell img/hsa.bell -model doc-bis.cat -skipchecks ScSc img/t3.litmus -show prop -o img >/dev/null

img/t3+fst.dot:  img/t3.litmus
	$(HERD) -suffix +fst -conf img/paper.cfg  $(T3FMT) -unshow hhb,rf,SWI,SWAVE,SSYSTEM,ldo,SWG -bell img/hsa.bell -model doc.cat -skipchecks ScCohCons img/t3.litmus -show prop -o img >/dev/null

#img/t1.dot: img/t1.litmus
#	$(HERD) -conf img/paper.cfg -unshow iico_data,iico -bell img/hsa.bell -model img/sc.cat -showinitwrites false -shift 0.0,0.5,0.5,0.0 -showlegend false -o img img/t1.litmus >/dev/null 

#img/t1-00.dot img/t1-01.dot: img/t1.dot
#	splitdot7 img/t1.dot

###Tests from the documentation
DOC2=../tests/doc2
HSAFMT=-conf img/paper.cfg -bell img/hsa.bell -unshow ldo -nshow 1 -showlegend false -showthread false -showinitwrites false -speedcheck true -model doc.cat

clean::
	/bin/rm -f $(HSALISTING)

img/HSA01.dot: $(DOC2)/HSA01.litmus
	$(HERD) $(HSAFMT) -extrachars 0 -shift 1.0,0.0,0.5,0.0 -unshow SWI,SAGENT,SWAVE,SSYSTEM,hhb -skipchecks ScCohCons -o img $< > /dev/null

img/HSA01.litmus: $(DOC2)/HSA01.litmus
	mprog7 -transpose true -mode text $< > $@

img/HSA02.dot: $(DOC2)/HSA02.litmus
	$(HERD) $(HSAFMT) -unshow SWI,SAGENT,SWAVE,SSYSTEM,SWG -skipchecks HhbCohCons -o img $< > /dev/null

img/HSA02.litmus: $(DOC2)/HSA02.litmus
	cp $<  $@

img/HSA03.dot: $(DOC2)/HSA03.litmus
	$(HERD) $(HSAFMT) -shift 0.0,0.33,0.0 -unshow SWI,SAGENT,SWAVE,SSYSTEM,SWG -skipchecks HhbCohCons -o img $< > /dev/null

img/HSA03.litmus: $(DOC2)/HSA03.litmus
	cp $<  $@

img/HSA04.dot: $(DOC2)/HSA04.litmus
	$(HERD) $(HSAFMT) -shift 0.0,0.33,0.0 -unshow SWG,SWI,SAGENT,SWAVE,SSYSTEM,SzWG -skipchecks HhbCohCons -o img $< > /dev/null

img/HSA04.litmus: $(DOC2)/HSA04.litmus
	cp $<  $@

img/HSA05.dot: $(DOC2)/HSA05.litmus
	$(HERD) $(HSAFMT) -unshow SWI,SAGENT,SWAVE,SSYSTEM,SWG -skipchecks HhbCohCons -o img $< > /dev/null

img/HSA05.litmus: $(DOC2)/HSA05.litmus
	cp $<  $@

img/HSA06.dot: $(DOC2)/HSA06.litmus
	$(HERD) $(HSAFMT) -shift 0.0,0.33,0.0 -unshow SWG,SWI,SAGENT,SWAVE,SSYSTEM,SzWG -skipchecks HhbCohCons -o img $< > /dev/null

img/HSA06.litmus: $(DOC2)/HSA06.litmus
	cp $<  $@


img/HSA07.dot: $(DOC2)/HSA07.litmus
	$(HERD) $(HSAFMT) -shift 0.0,0.5 -showraw coh -unshow SWI,SAGENT,SWAVE,SSYSTEM,SWG -skipchecks HhbCohCons -o img $< > /dev/null

img/HSA07+BIS.dot: $(DOC2)/HSA07.litmus
	$(HERD) -suffix +BIS $(HSAFMT) -shift 0.0,0.5 -unshow SWI,SAGENT,SWAVE,SSYSTEM,SWG -skipchecks LoadCons -o img $< > /dev/null

img/HSA07.litmus: $(DOC2)/HSA07.litmus
	cp $<  $@

img/HSA09.dot: $(DOC2)/HSA09.litmus
	$(HERD) $(HSAFMT) -shift 0.0,0.5 -unshow SWI,SAGENT,SWAVE,SSYSTEM,SWG -skipchecks HhbCohCons -o img $< > /dev/null

img/HSA09.litmus: $(DOC2)/HSA09.litmus
	cp $<  $@

img/HSA10.dot: $(DOC2)/HSA10.litmus
	$(HERD) $(HSAFMT) -unshow SWI,SAGENT,SWAVE,SSYSTEM,SWG,hsa-race,hhb -doshow ldo -skipchecks GdoCons -o img $< > /dev/null

img/HSA10.litmus: $(DOC2)/HSA10.litmus
	cp $<  $@

img/HSA11.dot: $(DOC2)/HSA11.litmus
	$(HERD) $(HSAFMT) -unshow SWI,SAGENT,SWAVE,SSYSTEM,SWG,hsa-race,hhb -doshow ldo -skipchecks GdoCons -o img $< > /dev/null

img/HSA11.litmus: $(DOC2)/HSA11.litmus
	cp $<  $@

img/HSA12.dot: $(DOC2)/HSA12.litmus
	$(HERD) $(HSAFMT) -unshow SWI,SAGENT,SWAVE,SSYSTEM,SWG,hsa-race,hhb -o img $< > /dev/null

img/HSA12+fences.dot: $(DOC2)/HSA12+fences.litmus
	$(HERD) $(HSAFMT)  -shift 0.0,0.5 -unshow SWI,SAGENT,SWAVE,SSYSTEM,hsa-race -o img $< > /dev/null

img/HSA12.litmus: $(DOC2)/HSA12.litmus
	cp $<  $@

img/HSA13.dot: $(DOC2)/HSA13.litmus
	$(HERD) $(HSAFMT) -shift 0.0,0.75 -unshow SWI,SAGENT,SWAVE,SSYSTEM,SWG -o img $< > /dev/null

img/HSA13.litmus: $(DOC2)/HSA13.litmus
	cp $<  $@

img/HSA14.dot: $(DOC2)/HSA14.litmus
	$(HERD) $(HSAFMT) -unshow SWI,SAGENT,SWAVE,SSYSTEM,SWG -o img $< > /dev/null

img/HSA14+SCOPE.dot: $(DOC2)/HSA14.litmus
	$(HERD) -suffix +SCOPE $(HSAFMT) -shift 0,0.25 -unshow SWI,SAGENT,SWAVE,SSYSTEM,SWG,hsa-race,hhb,rf,coh -doshow matches,same-wg -symetric matches,same-wg -o img $< > /dev/null

img/HSA14.litmus: $(DOC2)/HSA14.litmus
	cp $<  $@


###General rule for dot...
%.dot:%.litmus
	sh img/herd.sh $<

%.fig: %.dot
	neato -Tfig $< > $@

%.fig:%.ddd
	dot -Tfig $< > $@

%.pstex:%.fig
	fig2dev -L pstex  $< $@

%.pstex_t:%.fig
	fig2dev -L pstex_t -p $*.pstex  $< $@

%.pdf:%.ps
	ps2pdf $<

newfig: $(NEWPSTEXT) $(NEWPSTEX)

cleanfig:
	/bin/rm -f $(NEWFIG) $(NEWPSTEXT) $(NEWPSTEX)

clean:: cleanfig

realclean:: clean
	/bin/rm -f $(NEWDOT) img/t1.dot

sync:
	touch  $(NEWDOT) $(DIAGRAMS)
