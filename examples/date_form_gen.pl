:- use_module('../src/clpstr').

% This code example generates input for test cases on a  date converter.

input_gen(Input) :-
  str_in(Day,"( (0|1|2)(1|2|3|4|5|6|7|8|9) | (10|20|30|31) )\\."),
  str_in(Month,"( (0 (1|2|3|4|5|6|7|8|9)) | (1 (0|1|2)) )\\."),
  str_in(Year,"(1|2|3|4|5|6|7|8|9) (0|1|2|3|4|5|6|7|8|9)* | 0"),
  str_concatenation(Day,Month,Temp),
  str_concatenation(Temp,Year,Input),
  str_label([Input]).

input_gen_2(Input) :-
  str_in(Input,"( (0|1|2)(1|2|3|4|5|6|7|8|9) | (10|20|30|31) )\\.( (0 (1|2|3|4|5|6|7|8|9)) | (1 (0|1|2)) )\\.((1|2|3|4|5|6|7|8|9) (0|1|2|3|4|5|6|7|8|9)* | 0)"),
  str_label([Input]).

:- begin_tests(sql_check).

test(simple_date,[true(Input == Expected),nondet]) :-
  Expected = "01.01.1",
  input_gen(Input).

:- end_tests(sql_check).
