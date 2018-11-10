.PHONY: benchmarks
benchmarks:
	swipl -G10g -l test_framework.pl -g "time(make_all_benchmarks(test_cases_short_label,short_dfs,[dfs]))" -g halt
	swipl -G10g -l test_framework.pl -g "time(make_all_benchmarks(test_cases_short_label,short_bfs,[bfs]))" -g halt
	swipl -G10g -l test_framework.pl -g "time(make_all_benchmarks(test_cases_short_label,short_id_dfs,[id_dfs]))" -g halt
