:- use_module(library(plunit)).

:- use_module('../src/domains/basic_domains').
:- use_module('../src/domains/basic_operations').

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
  
test(simple_automaton,[true(Res == automaton_dom([1,2,3],[(1,range(32,126),2),(2,range(32,126),3)],[1],[3]))]) :-
  any_char_domain(D),
  repeat(D,2,Res).
  
test(simple_automaton_from_to_repeat,[true(Res == automaton_dom([1,2,3],[(1,range(32,126),2),(2,range(32,126),3)],[1],[2,3]))]) :-
  any_char_domain(D),
  repeat(D,1,2,Res).
  
test(simple_automaton_infinite_repeat,[true(Res == automaton_dom([1,2],[(1,range(32,126),2),(2,epsilon,1)],[1],[1,2]))]) :-
  any_char_domain(D),
  repeat(D,Res).

:- end_tests(repeat).
