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


:- begin_tests(bin_2_new_state).

test(simple_binstate_all_ones,[true(Res == 31)]) :-
  Test = [1,1,1,1,1],
  bin_2_new_state(binstate(Test),Res).

test(simple_binstate_one_one,[true(Res == 16)]) :-
  Test = [1,0,0,0,0],
  bin_2_new_state(binstate(Test),Res).

test(simple_binstate_no_ones,[true(Res == 0)]) :-
  Test = [0,0,0,0,0],
  bin_2_new_state(binstate(Test),Res).

test(simple_binstate_one_one,[true(Res == 21)]) :-
  Test = [1,0,1,0,1],
  bin_2_new_state(binstate(Test),Res).

test(simple_binstate_empty_list,[true(Res == 0)]) :-
  Test = [],
  bin_2_new_state(binstate(Test),Res).

test(simple_binstate_no_list,[fail]) :-
  Test = 10101,
  bin_2_new_state(binstate(Test),_).

:- end_tests(bin_2_new_state).


:- begin_tests(gen_bin_states).

test(gen0,true(Res == [[0,0,0],[0,0,1],[0,1,0],[0,1,1],[1,0,0],[1,0,1],[1,1,0],[1,1,1]])) :-
  Test = [1,2,3],
  gen_bin_states(Test,Res).

/*test(gen1,true(Res == [[0,0,0],[1,0,0],[0,1,0],[0,0,1],[1,1,0],[1,0,1],[0,1,1],[1,1,1]])) :-
  Test = [1,2,3],
  gen_bin_states1(Test,Res).

test(gen2,true(Res == [[0,0,0],[1,0,0],[0,1,0],[0,0,1],[1,1,0],[1,0,1],[0,1,1],[1,1,1]])) :-
  Test = [1,2,3],
  gen_bin_states2(Test,Res).*/


:- end_tests(gen_bin_states).


:- begin_tests(nfa_2_dfa).

test(simple_three_state_nfa,[fixme('not ready yet. Need to implement DFA to NFA reduce.')]) :-% Res == automaton_dom([1,2,3,4],[(1,0,1),(1,1,2),(2,0,3),(2,1,4),(3,0,3),(3,1,4),(4,0,3),(4,1,4)],[1],[2,4])]) :-
  Test = automaton_dom([1,2,3],[(1,0,1),(1,1,1),(1,1,2),(2,0,3),(2,1,3),(3,0,3),(3,1,3)],[1],[2]),
  dfa_reduce(Test,_).

:- end_tests(nfa_2_dfa).


:- begin_tests(bfss).

test(one_state,[true(Actual == Expected)]) :-
  Test = automaton_dom([1],[],[1],[1]),
  Expected = [1],
  breadth_first_state_search(Test,1,Actual).

test(three_states_no_transitions,[true(Actual == Expected)]) :-
  Test = automaton_dom([1,2,3],[],[1],[1]),
  Expected = [1],
  breadth_first_state_search(Test,1,Actual).

test(five_states,[true(Actual == Expected)]) :-
  Test = automaton_dom([1,2,3,4,5],[(1,a,2),(2,c,3),(1,d,4),(4,e,5)],[1],[3,5]),
  Expected = [1,2,3,4,5],
  breadth_first_state_search(Test,1,Actual).

test(five_states_with_cycles,[true(Actual == Expected)]) :-
  Test = automaton_dom([1,2,3,4,5],[(1,a,2),(2,b,1),(2,c,3),(1,d,4),(4,e,5),(5,g,2)],[1],[3,5]),
  Expected = [1,2,3,4,5],
  breadth_first_state_search(Test,1,Actual).

test(five_states_with_cycles_and_unreachables,[true(Actual == Expected)]) :-
  Test = automaton_dom([1,2,3,4,5,6,7],[(1,a,2),(2,b,1),(2,c,3),(1,d,4),(4,e,5),(5,g,2),(6,h,7)],[1],[3,5]),
  Expected = [1,2,3,4,5],
  breadth_first_state_search(Test,1,Actual).

test(no_states,[fail]) :-
  Test = automaton_dom([],[],[],[]),
  breadth_first_state_search(Test,1,_).

:- end_tests(bfss).
