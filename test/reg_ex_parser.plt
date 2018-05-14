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

test(multi_alternative,[fixme('not ready yet')]) :- %true(Res == [exp(set('|',[a,b,c,d]))])]) :-
  Test = `a | b | c | d`,
  parse_2_tree(Test,_).

test(complex_alternative,[true(Res == [exp(set('|',quantity(+,string(a)),quantity(*,string(b))))])]) :-
  Test = `a+ | b*`,
  parse_2_tree(Test,Res).

test(multi_complex_alternative,[fixme('not ready yet')]) :-%[true(Res == [exp(set('|',[quantity(+,string(a)),quantity(*,string(b)),quantity(?,string(c))]))])]) :-
  Test = `a+ | b* | c?`,
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

test(multi_operations,[true(Res == [exp(quantity(*,set('|',string(a),set('|',string(b),string(c)))))])]) :-
  Test = `(a | b | c)*`,
  parse_2_tree(Test,Res).

:- end_tests(tree_parser).


:- begin_tests(tree_parser_nesting).

test(brackets_nested_single_letter,[true(Res == [exp(string(a)),exp(string(b))])]) :-
  Test = `(a)(b)`,
  parse_2_tree(Test,Res).

test(alternatives_nested,[true(Res == [exp(set('|',string(a),[exp(quantity(*,string(b))),exp(string(b))]))])]) :-
  Test = `a | b*a`,
  parse_2_tree(Test,Res).

test(alternatives_brackets_nested,[true(Res == [exp(set('|',string(a),string(b))),exp(set('|',string(a),string(b)))])]) :-
  Test = `(a | b) (a | b)`,
  parse_2_tree(Test,Res).

test(quantity_nested,[true(Res == [exp(quantity(*,string(a))),exp(quantity(*,string(b)))])]) :-
  Test = `a*b*`,
  parse_2_tree(Test,Res).

test(quantity_nested_multi,[true(Res == [exp(quantity(*,string(a))),exp(quantity(+,string(b))),exp(quantity(?,string(c)))])]) :-
  Test = `a*b+c?`,
  parse_2_tree(Test,Res).

test(long_term_example,[true(Res == [exp(quantity(*,set('|',string(a),set('|',string(b),string(c))))),exp(set('|',string(a),string(b))),exp(quantity(*,string(ab))),exp(quantity(+,string(c))),exp(string(a))])]) :-
  Test = `(a | b | c)* (a | b) ab*c+a`,
  parse_2_tree(Test,Res).

:- end_tests(tree_parser_nesting).


:- begin_tests(generater).


:- end_tests(generater).
