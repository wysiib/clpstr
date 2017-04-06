:- use_module(library(plunit)).

:- use_module('../src/domains/basic_domains').
:- use_module('../src/domains/labeling').

:- begin_tests(basic_domains).

test(constant,[true(Res == "abc")]) :-
  constant_string_domain("abc",D),
  label(D,Res).

test(simple_automaton,[true(Res == "a"),nondet]) :-
  single_char_domain("a",Dom),
  label(Dom,Res).

test(any_char_domain,[true(Res == " "),nondet]) :-
  any_char_domain(D),
  label(D,Res).

test(any_char_domain_can_return_different_character,[nondet]) :-
  any_char_domain(D),
  label(D,Res),
  Res == "~".

:- end_tests(basic_domains).
