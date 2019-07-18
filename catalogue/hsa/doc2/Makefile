FILE=hsa
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
NEWFIG=img/lb+ldos.fig img/wrc+ldos.fig\
 img/isa2+scopes.fig  img/isa2+instances.fig img/isa2+sso.fig
DOT=
COMMON=macros.tex
DEPENDS=$(COMMON) hsa.tex newfig $(FIG) $(DIAGRAMS)

all: ps pdf

$(FILE).dvi: $(DEPENDS)
color.dvi: color.tex $(DEPENDS)

default: $(FILE).dvi
allfig: $(FIG) newfig
epsfig: allfig $(FIG:.pstex_t=.eps) $(NEWFIG:.fig=.eps) $(DOT:.dot=.eps) $(DDD:.ddd=.eps)

ps: $(FILE).ps
pdf: $(FILE).pdf

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

##New figures

NEWDOT=$(NEWFIG:.fig=.dot)
.PRECIOUS: $(NEWDOT)

NEWPSTEXT=$(NEWFIG:.fig=.pstex_t) $(DOT:.dot=.pstex_t) $(DDD:.ddd=.pstex_t)
NEWPSTEX= $(NEWFIG:.fig=.pstex) $(DOT:.dot=.pstex) $(DDD:.ddd=.pstex)

%.dot:%.litmus
	sh img/herd.sh $<

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

realclean: clean
	/bin/rm -f $(NEWDOT) 

sync:
	touch  $(NEWDOT) $(DIAGRAMS)

