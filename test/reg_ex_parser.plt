:- use_module(library(plunit)).

:- use_module('../src/reg_ex_parser').


:- begin_tests(tree_parser).

test(single_letter,[true(Res == set('|',string(a),string(b)))]) :-
  Test = `a`,
  parse_2_tree(Test,Res).

test(alternative_brackets,[true(Res == set('|',string(a),string(b)))]) :-
  Test = `(a | b)`,
  parse_2_tree(Test,Res).

test(alternative_no_brackets,[true(Res == set('|',string(a),string(b)))]) :-
  Test = `a | b`,
  parse_2_tree(Test,Res).

test(multi_alternative,[fixme('not ready yet')]) :- %true(Res == set('|',a,b))]) :-
  Test = `a | b | c | d`,
  parse_2_tree(Test,_).

test(quantity,[true(Res == quantity(*,string(a)))]) :-
  Test = `a*`,
  parse_2_tree(Test,Res).

test(quantity_nested,[true(Res == quantity(*,string(a)))]) :-
  Test = `a*b*`,
  trace,
  parse_2_tree(Test,Res).

test(multi_operations,[true(Res == quantity(*,set('|',string(a),set('|',string(b),string(c)))))]) :-
  Test = `(a | b | c)*`,
  parse_2_tree(Test,Res).

:- end_tests(tree_parser).


:- begin_tests(generater).


:- end_tests(generater).
