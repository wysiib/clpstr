:- use_module(library(plunit)).

:- use_module('../src/domains/basic_domains').

:- use_module('../src/domains/domain_conversion').

:- use_module('../src/domains/basic_operations').

:- use_module('../src/domains/reductions').
%:- use_module('../src/domains/reductions',[reachable/2,match_states/4,gen_matched_states/3,gen_matched_trans/3]).


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


:- begin_tests(remove_unused).

test(one_state,[true(Actual == Expected)]) :-
  Test = automaton_dom([1],[],[1],[1]),
  Expected = automaton_dom([1],[],[1],[1]),
  remove_unused(Test,Actual).

test(three_states_no_transitions,[true(Actual == Expected)]) :-
  Test = automaton_dom([1,2,3],[],[1],[1]),
  Expected = automaton_dom([1],[],[1],[1]),
  remove_unused(Test,Actual).

test(three_starts_no_transitions,[true(Actual == Expected)]) :-
  Test = automaton_dom([1,2,3],[],[1,2,3],[1]),
  Expected = automaton_dom([1,2,3],[],[1,2,3],[1]),
  remove_unused(Test,Actual).

test(three_ends_no_transitions,[true(Actual == Expected)]) :-
  Test = automaton_dom([1,2,3],[],[1],[1,2,3]),
  Expected = automaton_dom([1],[],[1],[1]),
  remove_unused(Test,Actual).

test(five_states,[true(Actual == Expected)]) :-
  Test = automaton_dom([1,2,3,4,5],[(1,a,2),(2,c,3),(1,d,4),(4,e,5)],[1],[3,5]),
  Expected = automaton_dom([1,2,3,4,5],[(1,a,2),(2,c,3),(1,d,4),(4,e,5)],[1],[3,5]),
  remove_unused(Test,Actual).

test(five_states_with_cycles,[true(Actual == Expected)]) :-
  Test = automaton_dom([1,2,3,4,5],[(1,a,2),(2,b,1),(2,c,3),(1,d,4),(4,e,5),(5,g,2)],[1],[3,5]),
  Expected = automaton_dom([1,2,3,4,5],[(1,a,2),(2,b,1),(2,c,3),(1,d,4),(4,e,5),(5,g,2)],[1],[3,5]),
  remove_unused(Test,Actual).

test(five_states_with_cycles_and_unreachables,[true(Actual == Expected)]) :-
  Test = automaton_dom([1,2,3,4,5,6,7],[(1,a,2),(2,b,1),(2,c,3),(1,d,4),(4,e,5),(5,g,2),(6,h,7)],[1],[3,5]),
  Expected = automaton_dom([1,2,3,4,5],[(1,a,2),(2,b,1),(2,c,3),(1,d,4),(4,e,5),(5,g,2)],[1],[3,5]),
  remove_unused(Test,Actual).

test(no_states,[fail]) :-
  Test = automaton_dom([],[],[],[]),
  remove_unused(Test,_).

:- end_tests(remove_unused).


:- begin_tests(clean_automaton_subs).

%%%%%%%% Tests on match_states %%%%%%%%
% used List of States:
% [1,3,5]

test(match_states_state_list_generate,[Res == [1,2,3]]) :-
  MatchDict = matchedStates{},
  reductions:match_states([1,3,5],MatchDict,_,Res).

test(match_states_find_value_in_dict,[true(Res == 3)]) :-
  MatchDict = matchedStates{},
  reductions:match_states([1,3,5],MatchDict,Dict,_),
  get_dict(5,Dict,Res).

test(match_states_dont_find_value_in_dict,[fail]) :-
  MatchDict = matchedStates{},
  reductions:match_states([1,3,5],MatchDict,Dict,_),
  get_dict(6,Dict,_).

test(match_states_no_dict,[throws(Error)]) :-
  subsumes_chk(Error, "Arguments are not sufficiently instantiated"),
  reductions:match_states([1,3,5],_,Dict,_),
  get_dict(5,Dict,_).

%%%%%%%% Tests on reachable %%%%%%%%

test(reachable_empty_domain,[fail]) :-
  reductions:reachable(empty,_).

test(reachable_string_domain,[true(Actual == Expected)]) :-
  Expected = ([1,2],[(1,range(97,97),2),(0,epsilon,1)]),
  constant_string_domain("a",Dom),
  reductions:reachable(Dom,Actual).

test(reachable_no_state_domain,[true(Actual == Expected)]) :-
  Expected = ([],[]),
  Test = automaton_dom([],[],[],[]),
  reductions:reachable(Test,Actual).

test(reachable_everything_in_reach,[true(Actual == Expected)]) :-
  Expected = ([1,2,3],[(2,range(98,98),3),(1,range(97,97),2),(0,epsilon,1)]),
  Test = automaton_dom([1,2,3],[(1,range(97,97),2),(2,range(98,98),3)],[1],[3]),
  reductions:reachable(Test,Actual).

test(reachable_nothing_in_reach,[true(Actual == Expected)]) :-
  Expected = ([1],[(0,epsilon,1)]),
  Test = automaton_dom([1,2,3],[(2,range(97,97),3),(2,range(98,98),3)],[1],[3]),
  reductions:reachable(Test,Actual).

%%%%%%%% Tests on gen_matched_states %%%%%%%%
% used List of States:
% [1,3,5]
% The dictionary is hard coded to be independent from match_states tests.
% if ever data structure is change, tests will fail and need to be rewritten.

test(gen_matched_states_empty,[true(Actual == Expected)]) :-
  Expected = [],
  Matches = matchedStates{1:1, 3:2, 5:3},
  reductions:gen_matched_states([],Matches,Actual).

test(gen_matched_states_all_states_available,[true(Actual == Expected)]) :-
  Expected = [1,2,3],
  Matches = matchedStates{1:1, 3:2, 5:3},
  Test = [1,3,5],
  reductions:gen_matched_states(Test,Matches,Actual).

test(gen_matched_states_some_states_available,[true(Actual == Expected)]) :-
  Expected = [1,2,3],
  Matches = matchedStates{1:1, 3:2, 5:3},
  Test = [1,2,3,4,5,6],
  reductions:gen_matched_states(Test,Matches,Actual).

%%%%%%%% Tests on gen_matched_trans %%%%%%%%
% used List of states:
% [1,3,5]
% The dictionary is hard coded to be independent from match_states tests.
% if ever data structure is change, tests will fail and need to be rewritten.

test(gen_matched_trans_empty,[true(Actual == Expected)]) :-
  Expected = [],
  Matches = matchedStates{1:1, 3:2, 5:3},
  reductions:gen_matched_trans([],Matches,Actual).

test(gen_matched_trans_all_states_available,[true(Actual == Expected)]) :-
  Expected = [(2,range(98,98),3),(1,range(97,97),2)],
  Matches = matchedStates{1:1, 3:2, 5:3},
  Test = [(1,range(97,97),3),(3,range(98,98),5)],
  reductions:gen_matched_trans(Test,Matches,Actual).

test(gen_matched_trans_some_start_states_missing,[fail]) :-
  Matches = matchedStates{1:1, 3:2, 5:3},
  Test = [(1,range(97,97),3),(2,range(98,98),5)],
  reductions:gen_matched_trans(Test,Matches,_).

test(gen_matched_trans_some_end_states_missing,[fail]) :-
  Matches = matchedStates{1:1, 3:2, 5:3},
  Test = [(1,range(97,97),3),(3,range(98,98),4)],
  reductions:gen_matched_trans(Test,Matches,_).

:- end_tests(clean_automaton_subs).


:- begin_tests(clean_automaton).

test(clean_empty_dom,[true(Res == empty)]) :-
  clean_automaton(empty,Res).

test(clean_string_dom,[true(Actual == Expected)]) :-
  constant_string_domain("Some Domain",Expected),
  clean_automaton(Expected,Actual).

test(clean_simple_automaton1,[true(Actual == Expected)]) :-
  single_char_domain("a",Expected),
  clean_automaton(Expected,Actual).

test(clean_simple_automaton2,[true(Actual == Expected)]) :-
  any_char_domain(Expected),
  clean_automaton(Expected,Actual).

test(simple_clean,[true(Actual == Expected)]) :-
  Expected = automaton_dom([1,2,3],[(1,range(97,97),2),(2,range(98,98),3)],[1],[3]),
  Test = automaton_dom([1,2,3,4,5,6],[(1,range(97,97),2),(2,range(98,98),3),(4,range(97,98),5),(5,range(97,98),6)],[1],[3]),
  clean_automaton(Test,Actual).

test(no_clean,[true(Actual == Expected)]) :-
  Expected = automaton_dom([1,2,3],[(1,range(97,97),2),(2,range(98,98),3)],[1],[3]),
  clean_automaton(Expected,Actual).

test(complex_clean_multi_start,[true(Actual == Expected)]) :-
  Expected = automaton_dom([1,2,3],[(1,range(97,97),3),(2,range(98,98),3)],[1,2],[3]),
  Test = automaton_dom([1,2,3,4,5,6],[(1,range(97,97),3),(2,range(98,98),3),(4,range(97,98),5),(5,range(97,98),6)],[1,2],[3]),
  clean_automaton(Test,Actual).

test(complex_clean_multi_way,[true(Actual == Expected)]) :-
  Expected = automaton_dom([1,2],[(1,range(97,97),2),(1,range(98,98),2)],[1],[2]),
  Test = automaton_dom([1,2,3,4,5,6],[(1,range(97,97),3),(1,range(98,98),3),(4,range(97,98),5),(5,range(97,98),6)],[1],[3]),
  clean_automaton(Test,Actual).

test(complex_clean_multi_way_split,[true(Actual == Expected)]) :-
  Expected = automaton_dom([1,2,3,4,5],[(1,range(97,97),2),(2,range(97,97),3),(3,range(97,98),5),(2,range(98,98),4),(4,range(97,98),5)],[1],[5]),
  Test = automaton_dom([1,2,3,4,5,6],[(1,range(97,97),2),(2,range(97,97),4),(2,range(98,98),5),(4,range(97,98),6),(5,range(97,98),6),(3,range(97,98),6)],[1],[6]),
  clean_automaton(Test,Actual).

% same automaton as result in complex_clean_multi_way_split
test(complex_no_clean_multi_way_split,[true(Actual == Expected)]) :-
  Expected = automaton_dom([1,2,3,4,5],[(1,range(97,97),2),(2,range(97,97),3),(3,range(97,98),5),(2,range(98,98),4),(4,range(97,98),5)],[1],[5]),
  clean_automaton(Expected,Actual).

:- end_tests(clean_automaton).
