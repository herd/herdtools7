DOT_FILES = $(shell ls *.dot)

all:
	$(foreach f, $(DOT_FILES), dot -Tpng $(f) > $(f:.dot=.png);)

%.png:
	dot -Tpng $(@:.png=.dot) > $@

clean:
	rm *.png
