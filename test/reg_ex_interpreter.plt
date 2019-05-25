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

:- begin_tests(whitespace).

test(whitespace_s) :-
  Regex = `\s`,
  whitespace_domain(WsDom),
  generate(Regex, RegDom),
  assertion(RegDom == WsDom).

:- end_tests(whitespace).


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

test(single_char_range) :-
  Regex = `[a-z]`,
  generate(Regex, Dom),
  assertion(Dom == automaton_dom([1,2],
                                 [(1,range(0'a, 0'z), 2)],
                                 [1],
                                 [2])).

test(multi_char_range) :-
  Regex = `[a-n0-7v-z]`,
  generate(Regex, Dom),
  assertion(Dom == automaton_dom([1,2],
                                 [(1,range(0'a, 0'n), 2),
                                  (1,range(0'0, 0'7), 2),
                                  (1,range(0'v, 0'z), 2)],
                                 [1],
                                 [2])).

:- end_tests(generater_reg_ex_operations).


:- begin_tests(generater_nesting).

test(brackets_nested_single_letter,[true(Actual == Expected)]) :-
  Test = `(a)(b)`,
  constant_string_domain("ab",Expected),
  generate(Test,Actual).

test(alternatives_nested,[true(Actual == Expected)]) :-
  Test = `a | b*a`,
  single_char_domain("a",A),
  single_char_domain("b",B),
  repeat(B,RepeatB),
  concatenation(RepeatB,A,TempDom),
  union(A,TempDom,Expected),
  generate(Test,Actual).

test(alternatives_brackets_nested,[true(Actual == Expected)]) :-
  Test = `(a | b) (a | b)`,
  single_char_domain("a",A),
  single_char_domain("b",B),
  union(A,B,TempDom),
  concatenation(TempDom,TempDom,Expected),
  generate(Test,Actual).

test(quantity_nested,[true(Actual == Expected)]) :-
  Test = `a*b*`,
  single_char_domain("a",A),
  repeat(A,TempDomA),
  single_char_domain("b",B),
  repeat(B,TempDomB),
  concatenation(TempDomA,TempDomB,Expected),
  generate(Test,Actual).

test(quantity_nested_multi,[true(Actual == Expected)]) :-
  Test = `a*b+c?`,
  single_char_domain("a",A),
  repeat(A,TempA),
  single_char_domain("b",B),
  repeat(B,TempDom1),
  concatenation(B,TempDom1,TempB),
  single_char_domain("c",C),
  repeat(C,0,1,TempC),
  concatenation(TempA,TempB,TempDom2),
  concatenation(TempDom2,TempC,Expected),
  generate(Test,Actual).

test(long_term_example,[true(Actual == Expected)]) :-
  Test = `(a | b | c)* (a | b) ab*c+a`,
  % (a | b | c)* --> TempDomTerm1
  single_char_domain("a",A),
  single_char_domain("b",B),
  single_char_domain("c",C),
  union([A,B,C],UnionABC),
  repeat(UnionABC,TempDomTerm1),
  % (a | b) --> TempDomTerm2
  union(A,B,TempDomTerm2),
  % ab*c+a --> TempDomTerm3
  repeat(B,RepeatB),
  concatenation(A,RepeatB,TempDom1),
  repeat(C,RepeatC),
  concatenation(C,RepeatC,TempDom2),
  concatenation(TempDom2,A,TempDom3),
  concatenation(TempDom1,TempDom3,TempDomTerm3),
  %  TempDomTerm1 TempDomTerm2 TempDomTerm3 --> Expected
  concatenation(TempDomTerm1,TempDomTerm2,TempDomTerm12),
  concatenation(TempDomTerm12,TempDomTerm3,Expected),
  generate(Test,Actual).

test(integer_parser, [true(Actual == automaton_dom([1,2,3,4,5,6,7,8],[(1,range(48,48),2),(3,range(45,45),4),(3,epsilon,5),(4,epsilon,5),(5,range(49,57),6),(6,epsilon,7),(7,range(48,57),8),(8,epsilon,7)],[1,3],[2,7]))]) :-
  Regex = `0|-?[1-9][0-9]*`,
  generate(Regex, Actual).

:- end_tests(generater_nesting).
