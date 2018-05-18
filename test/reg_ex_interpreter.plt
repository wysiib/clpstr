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


:- begin_tests(generater_reg_ex_operations).

test(alternative_brackets,[true(Actual == Expected)]) :-
  Test = `(a | b)`,
  single_char_domain("a",A),
  single_char_domain("b",B),
  union(A,B,Expected),
  generate(Test,Actual).

test(alternative_no_brackets,[true(Actual == Expected)]) :-
  Test = `a | b`,
  single_char_domain("a",A),
  single_char_domain("b",B),
  union(A,B,Expected),
  generate(Test,Actual).

test(multi_alternative,[true(Actual == Expected)]) :-
  Test = `a | b | c`,
  single_char_domain("a",A),
  single_char_domain("b",B),
  single_char_domain("c",C),
  union([A,B,C],Expected),
  generate(Test,Actual).

test(complex_alternative,[true(Actual == Expected)]) :-
  Test = `a* | b+`,
  single_char_domain("a",A),
  repeat(A,TempA),
  single_char_domain("b",B),
  repeat(B,TempDom),
  concatenation(B,TempDom,TempB),
  union(TempA,TempB,Expected),
  generate(Test,Actual).

test(multi_complex_alternative,[true(Actual == Expected)]) :-
  Test = `a* | b+ | c?`,
  single_char_domain("a",A),
  repeat(A,TempA),
  single_char_domain("b",B),
  repeat(B,TempDom),
  concatenation(B,TempDom,TempB),
  single_char_domain("c",C),
  repeat(C,0,1,TempC),
  union([TempA,TempB,TempC],Expected),
  generate(Test,Actual).

test(quantity_star,[true(Actual == Expected)]) :-
  Test = `a*`,
  single_char_domain("a",A),
  repeat(A,Expected),
  generate(Test,Actual).

test(quantity_plus,[true(Actual == Expected)]) :-
  Test = `a+`,
  single_char_domain("a",A),
  repeat(A,TempDom),
  concatenation(A,TempDom,Expected),
  generate(Test,Actual).

test(quantity_questionmark,[true(Actual == Expected)]) :-
  Test = `a?`,
  single_char_domain("a",A),
  repeat(A,0,1,Expected),
  generate(Test,Actual).

test(multi_operations,[true(Actual == Expected)]) :-
  Test = `(a | b | c)*`,
  single_char_domain("a",A),
  single_char_domain("b",B),
  single_char_domain("c",C),
  union([A,B,C],TempDom),
  repeat(TempDom,Expected),
  generate(Test,Actual).

:- end_tests(generater_reg_ex_operations).

/*
:- begin_tests(generater_nesting).

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

:- end_tests(generater_nesting).*/
