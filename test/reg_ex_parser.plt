:- use_module(library(plunit)).

:- use_module('../src/reg_ex_parser').


:- begin_tests(tree_parser).

test(alternative_brackets,[true(Res == set(a,b))]) :-
  Test = `(a | b)`,
  parse_2_tree(Test,Res).

test(alternative_no_brackets,[true(Res == set(a,b))]) :-
  Test = `a | b`,
  parse_2_tree(Test,Res).

test(quantity,[true(Res == quantity(*,a))]) :-
  Test = "a*",
  parse_2_tree(Test,Res).

:- end_tests(tree_parser).
