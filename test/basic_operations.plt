:- use_module(library(plunit)).

:- use_module('../src/domains/basic_domains').
:- use_module('../src/domains/basic_operations').


:- begin_tests(concat).

test(both_empty_string_domain_concat,[true(Res == string_dom(""))]) :-
  constant_string_domain("",S1),
  constant_string_domain("",S2),
  concatenation(S1,S2,Res).

test(left_empty_string_domain_concat,[true(Res == string_dom("a"))]) :-
  constant_string_domain("a",S1),
  constant_string_domain("",S2),
  concatenation(S1,S2,Res).

test(right_empty_string_domain_concat,[true(Res == string_dom("b"))]) :-
  constant_string_domain("",S1),
  constant_string_domain("b",S2),
  concatenation(S1,S2,Res).

test(simple_string_domain_concat,[true(Res == string_dom("ab"))]) :-
  constant_string_domain("a",S1),
  constant_string_domain("b",S2),
  concatenation(S1,S2,Res).

test(simple_automaton_concat,[true(Res == automaton_dom([1,2,3,4],[(1,range(32,126),2),(2,epsilon,3),(3,range(32,126),4)],[1],[4]))]) :-
  any_char_domain(D1),
  any_char_domain(D2),
  concatenation(D1,D2,Res).

test(mixed_domain_concat,[true(Res == automaton_dom([1,2,3,4],[(1,range(97,97),2),(2,epsilon,3),(3,range(97,97),4)],[1],[4]))]) :-
  single_char_domain("a",D1),
  constant_string_domain("a",D2),
  concatenation(D1,D2,Res).

test(mixed_domain_concat_reversed,[true(Res == automaton_dom([1,2,3,4],[(1,range(97,97),2),(2,epsilon,3),(3,range(97,97),4)],[1],[4]))]) :-
  constant_string_domain("a",D1),
  single_char_domain("a",D2),
  concatenation(D1,D2,Res).

:- end_tests(concat).


:- begin_tests(repeat).

test(constant,[true(Res == string_dom("aa"))]) :-
  constant_string_domain("a",D),
  repeat(D,2,Res).

test(simple_automaton_no_repeat,[fail]) :-
  any_char_domain(D),
  repeat(D,0,_Res).

test(simple_automaton_single_repeat,[true(Res == D)]) :-
  any_char_domain(D),
  repeat(D,1,Res).

test(simple_automaton_repeat,[true(Res == automaton_dom([1,2,3,4],[(1,range(32,126),2),(2,epsilon,3),(3,range(32,126),4)],[1],[4]))]) :-
  any_char_domain(D),
  repeat(D,2,Res).

test(simple_automaton_from_to_repeat,[true(Res == automaton_dom([1,2,3,4],[(1,range(32,126),2),(2,epsilon,3),(3,range(32,126),4)],[1],[2,4]))]) :-
  any_char_domain(D),
  repeat(D,1,2,Res).

test(simple_automaton_from_0_to_repeat,[true(Res == automaton_dom([1,2,3,4],[(1,range(32,126),2),(2,epsilon,3),(3,range(32,126),4)],[1],[1,2,4]))]) :-
  any_char_domain(D),
  repeat(D,0,2,Res).

test(simple_automaton_infinite_repeat,[true(Res == automaton_dom([1,2],[(1,range(32,126),2),(2,epsilon,1)],[1],[1,2]))]) :-
  any_char_domain(D),
  repeat(D,Res).



:- end_tests(repeat).
