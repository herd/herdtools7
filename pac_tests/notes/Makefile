

all: pacda.png xpacd.png autda_success.png autdb_failure.png ldr_success.png ldr_failure.png str_success.png str_failure.png

%.png:
	dot -Tpng $(@:.png=.dot) > $@
