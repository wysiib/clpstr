:- use_module(library(plunit)).

:- use_module('../src/domains/basic_domains').
:- use_module('../src/domains/basic_operations').

:- begin_tests(repeat).

test(constant,[true(Res == string_dom("aa")),blocked("uses repeat")]) :-
  constant_string_domain("a",D),
  repeat(D,2,Res).
  
:- end_tests(repeat).
