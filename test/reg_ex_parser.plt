:- use_module(library(plunit)).

:- use_module('../src/reg_ex_parser').


:- begin_tests(tree_parser).

test(single_letter,[true(Res == [exp(string(a))])]) :-
  Test = `a`,
  parse_2_tree(Test,Res).

test(some_letters,[true(Res == [exp(string(abc))])]) :-
  Test = `abc`,
  parse_2_tree(Test,Res).

test(alternative_brackets,[true(Res == [exp(set('|',string(a),string(b)))])]) :-
  Test = `(a | b)`,
  parse_2_tree(Test,Res).

test(alternative_no_brackets,[true(Res == [exp(set('|',string(a),string(b)))])]) :-
  Test = `a | b`,
  parse_2_tree(Test,Res).

test(multi_alternative,[fixme('not ready yet')]) :- %true(Res == [exp(set('|',a,b))])]) :-
  Test = `a | b | c | d`,
  parse_2_tree(Test,_).

test(multi_alternative,[fixme('not ready yet')]) :- %true(Res == [exp(set('|',a,b))])]) :-
  Test = `a | b | c | d`,
  parse_2_tree(Test,_).

test(quantity_star,[true(Res == [exp(quantity(*,string(a)))])]) :-
  Test = `a*`,
  parse_2_tree(Test,Res).

test(quantity_plus,[true(Res == [exp(quantity(+,string(a)))])]) :-
  Test = `a+`,
  parse_2_tree(Test,Res).

test(quantity_questionmark,[true(Res == [exp(quantity(?,string(a)))])]) :-
  Test = `a?`,
  parse_2_tree(Test,Res).

test(quantity_nested,[true(Res == [exp(quantity(*,string(a))),exp(quantity(*,string(b)))])]) :-
  Test = `a*b*`,
  parse_2_tree(Test,Res).

test(quantity_nested_multi,[true(Res == [exp(quantity(*,string(a))),exp(quantity(+,string(b))),exp(quantity(?,string(c)))])]) :-
  Test = `a*b+c?`,
  parse_2_tree(Test,Res).

test(multi_operations,[true(Res == [exp(quantity(*,set('|',string(a),set('|',string(b),string(c)))))])]) :-
  Test = `(a | b | c)*`,
  parse_2_tree(Test,Res).

test(long_term_example,[true(Res == [exp(quantity(*,set('|',string(a),set('|',string(b),string(c)))))])]) :-
  Test = `(a | b | c)*`,
  parse_2_tree(Test,Res).

:- end_tests(tree_parser).


:- begin_tests(generater).


:- end_tests(generater).
