:- use_module(library(plunit)).

:- use_module('../src/domains/basic_domains').

:- use_module('../src/domains/domain_conversion').

:- use_module('../src/domains/basic_operations').

:- use_module('../src/domains/reductions').


:- begin_tests(epsilon_reduction).

test(simple_transition_eps_reduction,[true(Res == automaton_dom([1,2],[],[1],[1,2]))]) :-
  Test = automaton_dom([1,2],[(1,epsilon,2)],[1],[2]),
  epsilon_reduce(Test,Res).

test(simple_loop_eps_reduction,[true(Res == automaton_dom([1],[],[1],[1]))]) :-
  Test = automaton_dom([1],[(1,epsilon,1)],[1],[1]),
  epsilon_reduce(Test,Res).

test(union_eps_reduction,[true(Res == automaton_dom([1,2,3,4,5],[(1,range(98,98),5),(1,range(97,97),3),(2,range(97,97),3),(4,range(98,98),5)],[1],[3,5]))]) :-
  single_char_domain("a",A),
  single_char_domain("b",B),
  union(A,B,Test),
  epsilon_reduce(Test,Res).

test(concat_eps_reduction,[true(Res == automaton_dom([1,2,3,4],[(2,range(98,98),4),(1,range(97,97),2),(3,range(98,98),4)],[1],[4]))]) :-
  single_char_domain("a",A),
  single_char_domain("b",B),
  concatenation(A,B,Test),
  epsilon_reduce(Test,Res).

test(parallel_eps_reduction,[true(Res == automaton_dom([1,2,3,4,5],[(2,range(98,98),4),(1,range(97,97),2),(3,range(98,98),4),(2,range(97,97),5)],[1],[4,5]))]) :-
  Test = automaton_dom([1,2,3,4,5],[(1,range(97,97),2),(2,epsilon,3),(3,range(98,98),4),
                                    (2,range(97,97),5),(5,epsilon,4)],[1],[4]),
  epsilon_reduce(Test,Res).

test(double_eps_reduction,[true(Res == automaton_dom([1,2,3,4],[(2,range(97,97),4),(1,range(97,97),4),(3,range(97,97),4)],[1],[4]))]) :-
  Test = automaton_dom([1,2,3,4],[(1,epsilon,2),(2,epsilon,3),(3,range(97,97),4)],[1],[4]),
  epsilon_reduce(Test,Res).

:- end_tests(epsilon_reduction).


:- begin_tests(epsilon_closure).

test(simple_automaton_eps_closure,[true(Res == [1])]) :-
  Test = automaton_dom([1],[(1,epsilon,1)],[1],[1]),
  get_transition(Test,T),
  epsilon_closure(1,T,Res).

test(concat_eps_closure,[true(Res == [3])]) :-
  single_char_domain("a",A),
  single_char_domain("b",B),
  concatenation(A,B,Test),
  get_transition(Test,T),
  epsilon_closure(2,T,Res).

test(union_eps_closure,[true(Res == [2,4])]) :-
  single_char_domain("a",A),
  single_char_domain("b",B),
  union(A,B,Test),
  get_transition(Test,T),
  epsilon_closure(1,T,Res).

test(parallel_eps_closure,[true(Res == [2,3,4,5])]) :-
  Test = automaton_dom([1,2,3,4,5],[(1,range(97,97),2),(1,epsilon,2),(2,epsilon,3),(2,range(97,97),5),
                                    (3,range(98,98),4),(3,epsilon,5),(5,epsilon,4)],[1],[4]),
  get_transition(Test,T),
  ordered_eps_closure(1,T,Res).

:- end_tests(epsilon_closure).
