.PHONY: message

message:
	@echo 'Currently, the Makefile is only used for testing purposes.'
	@echo 'type make help for options.'

tests:
	for file in test/*.plt; do swipl --quiet -l $$file -t "run_tests"; done

profile:
	for file in test/*.plt; do swipl --quiet -l $$file -t "profile(run_tests)"; done

test_dom:
	swipl --quiet -l test/domains.plt -t "run_tests"

test_base_op:
	swipl --quiet -l test/basic_operations.plt -t "run_tests"

test_intersec:
	swipl --quiet -l test/intersection.plt -t "run_tests"

test_reduc:
	swipl --quiet -l test/reduction.plt -t "run_tests"

test_label:
	swipl --quiet -l test/labeling.plt -t "run_tests"

test_label_all:
	swipl --quiet -l test/labeling.plt -t "run_tests"
	swipl --quiet -l test/labeling_dfs.plt -t "run_tests"
	swipl --quiet -l test/labeling_id_dfs.plt -t "run_tests"

test_label_dfs_time:
	swipl --quiet -l test/labeling_dfs.plt -t "time(run_tests)"

test_label_id_dfs_time:
	swipl --quiet -l test/labeling_id_dfs.plt -t "time(run_tests)"

test_dom_conv:
	swipl --quiet -l test/domain_conversion.plt -t "run_tests"

test_str_in:
	swipl --quiet -l test/str_in.plt -t "run_tests"

test_re_parser:
	swipl --quiet -l test/reg_ex_parser.plt -t "run_tests"

help:
	@echo	'make tests - To run all tests.'
	@echo 'make profile - To run all tests and return resource calculation.'
	@echo	'make test_dom - To run only the tests on basic domains.'
	@echo	'make test_base_op - To run only the tests on basic operations on domains.'
	@echo	'make test_intersec - To run only the tests on intersection.'
	@echo 'make test_reduc - To run only the tests on reduction.'
	@echo	'make test_label - To run only the tests on domnain labeling.'
	@echo	'make test_dom_conv - To run only the tests on domain conversion.'
