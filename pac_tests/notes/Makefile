
all: pacda.png xpacd.png autda_success.png autdb_failure.png ldr_success.png ldr_failure.png str_success.png str_failure.png fpac_with_result_eq_result_success.png fpac_with_result_eq_target_success.png fpac_without_else_success.png no_fpac_predicted_fail.png no_fpac_predicted_success.png no_fpac_success.png pauth1_fail.png pauth1_success.png

%.png:
	dot -Tpng $(@:.png=.dot) > $@
