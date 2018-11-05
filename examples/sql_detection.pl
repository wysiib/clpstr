:- use_module('../src/clpstr').

% This code example checks for attacking code in an sql injection.

input_check(Input) :-
  string(Input),
  \+ check_for_attack_code(Input).

check_for_attack_code(Input) :-
  str_in(X,Input),
  str_infix(X,"\' OR \'1\' = \'1").


%
:- begin_tests(sql_check).

test(simple_true,[true]) :-
  Test = "abc",
  input_check(Test).

test(simple_double_true,[true]) :-
  Test1 = "abc",
  input_check(Test1),
  Test2 = "cde",
  input_check(Test2).

test(simple_fail,[fail]) :-
  Test = "\' OR \'1\' = \'1",
  input_check(Test).

test(simple_fail_no_whitespace,[fail]) :-
  Test = "\'OR\'1\'=\'1",
  input_check(Test).

test(embeded_fail_Prefix,[fail]) :-
  Test = "\' OR \'1\' = \'1\'abc",
  input_check(Test).

test(embeded_fail_suffix,[fail]) :-
  Test = "abc\' OR \'1\' = \'1\'",
  input_check(Test).

test(embeded_fail_Infix,[fail]) :-
  Test = "abc\' OR \'1\' = \'1\'abc",
  input_check(Test).

test(double_test_one_fail,[fail]) :-
  Test1 = "abc",
  input_check(Test1),
  Test2 = "abc\' OR \'1\' = \'1\'abc",
  input_check(Test2).


:- end_tests(sql_check).
