:- use_module(library(plunit)).

:- use_module('../src/clpstr').
:- use_module('../src/domains/basic_domains').
:- use_module('../src/domains/basic_operations').

:- begin_tests(str_labeling).

test(simple_string,[true(X == "true")]) :-
  constant_string_domain("true",Dom),
  str_in(X,Dom),
  str_labeling([],[X]).

test(simple_generated_dom,[true(X == "true")]) :-
  generate_domain("true",Dom),
  str_in(X,Dom),
  str_labeling([],[X]).

:- end_tests(str_labeling).
