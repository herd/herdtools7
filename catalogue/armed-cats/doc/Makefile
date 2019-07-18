FILE=v8
DVIPS=dvips
LATEX=rubber -f
XDVI=xdvi  -s 7 -paper a4 -expert
EDITOR=emacs
GV=gv
LPR=lpr
HEVEA=hevea
HEVEAOPTS=-fix
HACHA=hacha
HACHAOPTS=-tocbis
NEWFIG= img/mp.fig \
 img/coww.fig img/corw1.fig img/corw2.fig img/cowr.fig img/corr.fig \
 img/lb+ppos.fig img/lb+addrs+ww.fig img/lb+acqs.fig img/lb+rels.fig \
 img/mp+lwfence+ppo.fig img/wrc+lwfence+ppo.fig \
 img/isa2+lwfence+ppos.fig img/sb+ffences.fig \
 img/2+2w+lwfences.fig img/2+2w+rels.fig \
 img/wrw+2w+lwsyncs.fig img/wrw+2w+rel+acq-rel+fence.fig img/rwc+syncs.fig\
 img/s+lwfence+ppo.fig img/s+rel+ppo.fig img/r+ffences.fig \
 img/iriw+ffences.fig \
 img/A-cumul.fig img/B-cumulativity.fig \
 img/AB-cumul.fig \
 img/sb+ra.fig img/rwc+ra.fig img/iriw+ra.fig img/r+ra.fig \
 img/mp+rel+ppo.fig img/wrc+rel+ppo.fig img/isa2+rel+ppos.fig  
DOT=img/s-A-cumul1.dot img/s-A-cumul2.dot \
 img/fr.dot img/atom.dot \
 img/mp-cf.dot img/mp-df1.dot img/mp-df2.dot img/mp-df3.dot img/mp-df4.dot \

#DDD=ins/addr.ddd ins/data.ddd ins/ctrl.ddd 
CATS=armv8.cat armv8-happens-before.cat armv8-ppo.cat \
 armv8-sc-per-location.cat  armv8-utils.cat \
 armv8-fences.cat  armv8-observation.cat \
 armv8-propagation.cat  armv8-skeleton.cat

COMMON=macros.tex armed-cats.bib  $(CATS)
DEPENDS=$(COMMON) v8.tex $(FIG) newfig $(DIAGRAMS)

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
	sh mkpng.sh $* dot > $* .png && qiv $*.png && /bin/rm -f $*.png 

A=$(FIG:.pstex_t=.pstex)
B=$(FIG:.pstex_t=.fig)
clean::
	/bin/rm -f $(FIG) $(A) $(B)

##New figures

NEWDOT=$(NEWFIG:.fig=.dot)
.PRECIOUS: $(NEWDOT) v8.ps

NEWPSTEXT=$(NEWFIG:.fig=.pstex_t) $(DOT:.dot=.pstex_t) $(DDD:.ddd=.pstex_t)
NEWPSTEX= $(NEWFIG:.fig=.pstex) $(DOT:.dot=.pstex) $(DDD:.ddd=.pstex)

%.fig: %.dot
	neato -Tfig $< -o $@

%.fig:%.ddd
	dot -Tfig $< -o $@

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
	/bin/rm -f $(NEWDOT) 

sync:
	touch  $(NEWDOT) $(DIAGRAMS)

