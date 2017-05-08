:- use_module(library(plunit)).

:- use_module('../src/domains/basic_domains').

:- begin_tests(basic_domains).

test(constant_domain,[true(D == string_dom("abc"))]) :-
  constant_string_domain("abc",D).

test(any_char_domain,[true(D == automaton_dom([1,2],[(1,range(32,126),2)],[1],[2]))]) :-
  any_char_domain(D).

test(single_char_domain,[true(D == automaton_dom([1,2],[(1,range(97,97),2)],[1],[2]))]) :-
  single_char_domain("a",D).

:- end_tests(basic_domains).


:- begin_tests(basic_domain_getters).

test(get_all_states,[true(States == [1,2])]) :-
  any_char_domain(D),
  get_all_states(D,States).

test(get_transition,[true(Trans == [(1,range(32,126),2)])]) :-
  any_char_domain(D),
  get_transition(D,Trans).

test(get_start_states,[true(States == [1])]) :-
  any_char_domain(D),
  get_start_states(D,States).

test(get_end_states,[true(States == [2])]) :-
  any_char_domain(D),
  get_end_states(D,States).

:- end_tests(basic_domain_getters).
