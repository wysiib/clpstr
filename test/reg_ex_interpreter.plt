:- use_module(library(plunit)).

:- use_module('../src/reg_ex_parser').
:- use_module('../src/domains/basic_domains').
:- use_module('../src/domains/basic_operations').

:- begin_tests(generater_characters).

test(single_letter,[true(Actual == Expected)]) :-
  Test = `a`,
  single_char_domain("a",Expected),
  generate(Test,Actual).

test(some_letters,[true(Actual == Expected)]) :-
  Test = `abc`,
  constant_string_domain("abc",Expected),
  generate(Test,Actual).

test(any_character,[true(Actual == Expected)]) :-
  Test = `.`,
  any_char_domain(Expected),
  generate(Test,Actual).

test(some_any_characters,[true(Actual == Expected)]) :-
  Test = `...`,
  any_char_domain(D),
  concatenation(D,D,D2),
  concatenation(D,D2,Expected),
  generate(Test,Actual).

test(some_mixed_characters,[true(Actual == Expected)]) :-
  Test = `a.b`,
  single_char_domain("a",A),
  any_char_domain(Any),
  single_char_domain("b",B),
  concatenation(A,Any,D),
  concatenation(D,B,Expected),
  generate(Test,Actual).

:- end_tests(generater_characters).
