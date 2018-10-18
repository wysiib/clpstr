:- use_module(library(plunit)).

:- use_module('../src/domains/basic_domains').

:- use_module('../src/domains/domain_conversion').


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



:- begin_tests(adjusts).

test(adjust_simple_charakter,[true(Res == [(5,range(97,97),6)])]) :-
  L = 4,
  single_char_domain("a",Dom),
  get_transition(Dom,Trans),
  adjust_transition(L,Trans,Res).

test(adjust_constant_string_domain,[true(Res == automaton_dom([5,6,7,8],[(5,range(97,97),6),
                                        (6,range(98,98),7),(7,range(99,99),8)],[5],[8]))]) :-
  L = 4,
  constant_string_domain("abc",D),
  adjust_domain(L,D,Res).


test(adjust_single_char_domain,[true(Res == automaton_dom([5,6],[(5,range(97,97),6)],[5],[6]))]) :-
  L = 4,
  single_char_domain("a",D),
  adjust_domain(L,D,Res).

:- end_tests(adjusts).


:- begin_tests(combine_domains).

test(comb_simple,[true(Res == automaton_dom([1,2,1,2],[(1,range(32,126),2),(1,range(32,126),2)],[1,1],[2,2]))]) :-
  any_char_domain(D1),
  any_char_domain(D2),
  combine_domain(D1,D2,Res).

:- end_tests(combine_domains).


:- begin_tests(size_operator).

test(simple_size_three,[true(Res == Expected)]) :-
  %trace,
  Expected = automaton_dom([1,2,3,4],[(1,range(32,126),2),(2,range(32,126),3),(3,range(32,126),4)],[1],[4]),
  generate_any_size(3,Res).

test(simple_size_six,[true(Res == Expected)]) :-
  Expected = automaton_dom([1,2,3,4,5,6,7],[(1,range(32,126),2),(2,range(32,126),3),(3,range(32,126),4),(4,range(32,126),5),(5,range(32,126),6),(6,range(32,126),7)],[1],[7]),
  generate_any_size(6,Res).

/*test(var_size,[true(Res == Expected),nondet]) :-
  Expected = automaton_dom([1,2],[(1,range(32,126),2)],[1],[2]),
  generate_any_size(_,Res).*/

test(size_control,[true]) :-
  Expected = automaton_dom([1,2],[(1,range(32,126),2)],[1],[2]),
  generate_any_size(1,Expected).

test(size_fail_no_number,[fail]) :-
  generate_any_size(a,_).

:- end_tests(size_operator).
