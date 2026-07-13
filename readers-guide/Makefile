PDFLATEX := pdflatex -halt-on-error -file-line-error -interaction=nonstopmode
BIBTEX := bibtex
LATEX_SOURCE := ReadersGuide.tex
LATEX_BASE := $(basename $(LATEX_SOURCE))
LATEX_LOG := ReadersGuide.log
BIB_SOURCE := references.bib

.PHONY: all
all: $(BIB_SOURCE)
	$(PDFLATEX) ./$(LATEX_SOURCE)
	$(BIBTEX) $(LATEX_BASE)
	$(PDFLATEX) ./$(LATEX_SOURCE)
	$(PDFLATEX) ./$(LATEX_SOURCE)
	$(MAKE) check-latex-log

LITMUS_DIR := litmus-tests
LITMUS2DESC ?= litmus2desc
LITMUS_TESTS := \
    coWW \
    coRW1 \
    coRW2 \
    coWR \
    coRR \
    LB \
    R+WW+RW \
    R+WR+W+RW \
    MP \
    WRC \
    ISA2 \
    W+RR+WR+WR \
    PPOAA \
    PPODA \
    PPOCA \
    PPOAC \
    PPODC \
    PPOCC \
    RSW \
    RDW \
    SB \
    RWC \
    IRIW \
    S \
    WWC \
    R \
    W+RW+WR \
    W+RR+2W \
    W+RR+WR+WW \
    2+2w \
    W+RW+W+RW \
    Z6.3 \
    W+WR+RR+W \
    W+RW+2W

LITMUS_TEX := $(addprefix litmus-descriptions/,$(addsuffix .tex,$(LITMUS_TESTS)))

$(LITMUS_TEX): litmus-descriptions/%.tex: $(LITMUS_DIR)/%.litmus
	@mkdir -p $(dir $@)
	$(LITMUS2DESC) --latex $< > $@

.PHONY: check-latex-log
check-latex-log:
	@test -f $(LATEX_LOG) || { echo "$(LATEX_LOG) does not exist"; exit 1; }
	@if grep -En "LaTeX Warning: File .* not found|Package .* Warning: File .* not found|! LaTeX Error: File .* not found" $(LATEX_LOG); then \
		echo "Missing-file warning found in $(LATEX_LOG)"; \
		exit 1; \
	fi

.PHONY: descriptions
descriptions:
	$(MAKE) $(LITMUS_TEX)

.PHONY: clean
clean:
	rm -f ReadersGuide.aux ReadersGuide.bbl ReadersGuide.blg ReadersGuide.log ReadersGuide.out ReadersGuide.pdf ReadersGuide.toc
