:- use_module(library(plunit)).

:- use_module('../src/domains/basic_domains').
:- use_module('../src/domains/domain_conversion').

:- begin_tests(domain_conversion).

test(constant_domain,[true(Aut == automaton_dom([1,2,3,4],[(1,range(97,97),2),(2,range(98,98),3),(3,range(99,99),4)],[1],[4]))]) :-
  constant_string_domain("abc",D),
  constant_string_domain_to_automaton(D,Aut).
  
:- end_tests(domain_conversion).