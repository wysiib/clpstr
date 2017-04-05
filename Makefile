.PHONY: message

message:
	@echo 'Currently, the Makefile is only used for testing purposes.'

tests:
	for file in test/*.plt; do swipl --quiet -l $$file -t "run_tests"; done

profile:
	for file in test/*.plt; do swipl --quiet -l $$file -t "profile(run_tests)"; done
