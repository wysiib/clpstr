:- use_module(library(plunit)).

:- use_module('../src/domains/basic_domains').


:- begin_tests(character_ranges).

test(empty_char_range_delta) :-
  basic_domains:char_range_delta([], 1, 2, Delta),
  assertion(Delta == []).

test(single_char_range_delta) :-
  basic_domains:char_range_delta([a-z], 1, 2, Delta),
  assertion(Delta == [(1, range(0'a, 0'z), 2)]).

test(multiple_char_range_delta) :-
  basic_domains:char_range_delta([a-e, g-j, p-w], 1, 2, Delta),
  assertion(Delta == [(1, range(0'a, 0'e), 2),
                      (1, range(0'g, 0'j), 2),
                      (1, range(0'p, 0'w), 2)]).

test(char_range_automaton) :-
  char_range_domain(ranges([a-f, q-v]), Automaton),
  assertion(Automaton == automaton_dom([1,2],
                                       [(1,range(0'a, 0'f), 2), (1,range(0'q, 0'v), 2)],
                                       [1],
                                       [2])).

:- end_tests(character_ranges).
