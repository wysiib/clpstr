:- use_module(library(plunit)).

:- use_module('../src/reg_ex_parser').


:- begin_tests(tree_parser_characters).

test(single_letter,[true(Res == char(a))]) :-
  Test = `a`,
  parse_2_tree(Test,Res).

test(some_letters,[true(Res == concat(char(a),concat(char(b),char(c))))]) :-
  Test = `abc`,
  parse_2_tree(Test,Res).

test(any_character,[true(Res == any)]) :-
  Test = `.`,
  parse_2_tree(Test,Res).

test(some_any_characters,[true(Res == concat(any,concat(any,any)))]) :-
  Test = `...`,
  parse_2_tree(Test,Res).

test(some_mixed_characters,[true(Res == concat(char(a),concat(any,char(b))))]) :-
  Test = `a.b`,
  parse_2_tree(Test,Res).

test(parse_minus) :-
  Regex = `-1`,
  parse_2_tree(Regex, Tree),
  assertion(Tree = concat(char(-), char('1'))).

:- end_tests(tree_parser_characters).


:- begin_tests(tree_parser_reg_ex_operations).

test(alternative_brackets,[true(Res == set(char(a),char(b)))]) :-
  Test = `(a | b)`,
  parse_2_tree(Test,Res).

test(alternative_no_brackets,[true(Res == set(char(a),char(b)))]) :-
  Test = `a | b`,
  parse_2_tree(Test,Res).

test(multi_alternative,[Actual == Expected]) :- %true(Res == [exp(set([a,b,c,d]))])]) :-
  Test = `a | b | c`,
  Expected = set(char(a),set(char(b),char(c))),
  parse_2_tree(Test,Actual).

test(alternative_brackets_two_char,[true(Res == set(concat(char(a),char(b)),concat(char(c),char(d))))]) :-
  Test = `(ab) | (cd)`,
  parse_2_tree(Test,Res).

test(alternative_no_bracket_two_char,[true(Res == set(concat(char(a),char(b)),concat(char(c),char(d))))]) :-
  Test = `ab | cd`,
  parse_2_tree(Test,Res).

test(complex_alternative,[true(Res == set(quantity(+,char(a)),quantity(*,char(b))))]) :-
  Test = `a+ | b*`,
  parse_2_tree(Test,Res).

test(multi_complex_alternative,[Actual == Expected]) :-%[true(Res == [exp(set([quantity(+,char(a)),quantity(*,char(b)),quantity(?,char(c))]))])]) :-
  Test = `a+ | b* | c?`,
  Expected = set(quantity(+,char(a)),set(quantity(*,char(b)),quantity(?,char(c)))),
  parse_2_tree(Test,Actual).

test(quantity_star,[true(Res == quantity(*,char(a)))]) :-
  Test = `a*`,
  parse_2_tree(Test,Res).

test(quantity_plus,[true(Res == quantity(+,char(a)))]) :-
  Test = `a+`,
  parse_2_tree(Test,Res).

test(quantity_questionmark,[true(Res == quantity(?,char(a)))]) :-
  Test = `a?`,
  parse_2_tree(Test,Res).

test(multi_operations,[true(Res == quantity(*,set(char(a),set(char(b),char(c)))))]) :-
  Test = `(a | b | c)*`,
  parse_2_tree(Test,Res).

test(quantity_star_two_char,[true(Res == concat(char(a),quantity(*,char(b))))]) :-
  Test = `ab*`,
  parse_2_tree(Test,Res).

:- end_tests(tree_parser_reg_ex_operations).


:- begin_tests(tree_parser_nesting).

test(brackets_nested_single_letter,[true(Res == concat(char(a),char(b)))]) :-
  Test = `(a)(b)`,
  parse_2_tree(Test,Res).

test(alternatives_nested,[true(Res == set(char(a),concat(quantity(*,char(b)),char(a))))]) :-
  Test = `a | b*a`,
  parse_2_tree(Test,Res).

test(alternatives_brackets_nested,[true(Res == concat(set(char(a),char(b)),set(char(a),char(b))))]) :-
  Test = `(a | b) (a | b)`,
  parse_2_tree(Test,Res).

test(quantity_nested,[true(Res == concat(quantity(*,char(a)),quantity(*,char(b))))]) :-
  Test = `a*b*`,
  parse_2_tree(Test,Res).

test(quantity_nested_multi,[true(Res == concat(quantity(*,char(a)),concat(quantity(+,char(b)),quantity(?,char(c)))))]) :-
  Test = `a*b+c?`,
  parse_2_tree(Test,Res).

test(long_term_example,[true(Res == concat(quantity(*,set(char(a),set(char(b),char(c)))),concat(set(char(a),char(b)),concat(char(a),concat(quantity(*,char(b)),concat(quantity(+,char(c)),char(a)))))))]) :-
  Test = `(a | b | c)* (a | b) ab*c+a`,
  parse_2_tree(Test,Res).

:- end_tests(tree_parser_nesting).
