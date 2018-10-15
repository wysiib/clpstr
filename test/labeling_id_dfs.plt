:- use_module(library(plunit)).

:- use_module('../src/domains/basic_domains').
:- use_module('../src/domains/basic_operations').
:- use_module('../src/domains/labeling').

:- begin_tests(basic_domains).

test(constant,[all(Res == ["abc"])]) :-
  constant_string_domain("abc",D),
  labeling([id_dfs],D,Res).

test(simple_automaton,[all(Res == ["a"])]) :-
  single_char_domain("a",Dom),
  labeling([id_dfs],Dom,Res).

test(any_char_domain,[true(Res == " "),nondet]) :-
  any_char_domain(D),
  labeling([id_dfs],D,Res).

test(any_char_domain_can_return_different_character,[nondet]) :-
  any_char_domain(D),
  labeling([id_dfs],D,Res),
  Res == "~".

test(epsilon_can_be_labeled,[all(Res == [""])]) :-
  D = automaton_dom([1,2],[(1,epsilon,2)],[1],[2]),
  labeling([id_dfs],D,Res).

:- end_tests(basic_domains).


:- begin_tests(domains_with_operations).

test(simple_concat_automaton,[all(Res == ["ab"])]) :-
  single_char_domain("a",Dom1),
  single_char_domain("b",Dom2),
  concatenation(Dom1,Dom2,TestDom),
  labeling([id_dfs],TestDom,Res).

test(simple_union_automaton,[all(Res == ["a","b"])]) :-
  single_char_domain("a",Dom1),
  single_char_domain("b",Dom2),
  union(Dom1,Dom2,TestDom),
  labeling([id_dfs],TestDom,Res).

test(simple_repeat1_automaton,[true(Res == ""),nondet]) :-
  single_char_domain("a",Dom),
  repeat(Dom,TestDom),
  labeling([id_dfs],TestDom,""),
  labeling([id_dfs],TestDom,"a"),
  labeling([id_dfs],TestDom,"aa"),
  labeling([id_dfs],TestDom,"aaa"),
  labeling([id_dfs],TestDom,Res).

test(simple_repeat2_automaton,[all(Res == ["aaa"])]) :-
  single_char_domain("a",Dom),
  repeat(Dom,3,TestDom),
  labeling([id_dfs],TestDom,Res).

test(simple_repeat3_automaton,[all(Res == ["a","aa","aaa"])]) :-
  single_char_domain("a",Dom),
  repeat(Dom,1,3,TestDom),
  labeling([id_dfs],TestDom,Res).

test(any_repeat3_automaton,[all(Res == ["a","aa","aaa"])]) :-
  any_char_domain(Dom),
  repeat(Dom,1,3,TestDom),
  labeling([id_dfs],TestDom,Res).

test(any_repeat3_automaton,[true(Res == " "),nondet]) :-
  any_char_domain(Dom),
  repeat(Dom,1,3,TestDom),
  labeling([id_dfs],TestDom,"a"),
  labeling([id_dfs],TestDom,"ab"),
  labeling([id_dfs],TestDom,"abc"),
  labeling([id_dfs],TestDom,"A"),
  labeling([id_dfs],TestDom,"AB"),
  labeling([id_dfs],TestDom,"ABC"),
  labeling([id_dfs],TestDom,Res).

test(simple_intersect_automaton,[all(Res == ["ab"])]) :-
  TestDom2 = automaton_dom([1,2,3],[(1,range(97,98),2),(2,range(97,98),3)],[1],[3]),
  single_char_domain("a",Dom1),
  single_char_domain("b",Dom2),
  concatenation(Dom1,Dom2,TestDom1),
  intersection(TestDom1,TestDom2,TestDom3),
  labeling([id_dfs],TestDom3,Res).

:- end_tests(domains_with_operations).


:- begin_tests(infinite_domains).

test(simple_infinite_domain,[true(Res == "a"),nondet]) :-
  TestDom = automaton_dom([1,2],[(1,range(97,97),2),(2,range(97,97),1)],[1],[2]),
  labeling([id_dfs],TestDom,"a"),
  labeling([id_dfs],TestDom,"aaa"),
  labeling([id_dfs],TestDom,"aaaaa"),
  labeling([id_dfs],TestDom,Res).

test(one_infinite_trans_domain,[true(Res == "aa"),nondet]) :-
  TestDom = automaton_dom([1,2,3],[(1,range(97,97),2),(2,range(97,97),1),(2,range(97,97),3)],[1],[3]),
  labeling([id_dfs],TestDom,"aa"),
  labeling([id_dfs],TestDom,"aaaa"),
  labeling([id_dfs],TestDom,"aaaaaa"),
  labeling([id_dfs],TestDom,Res).

test(two_infinite_trans_domain1,[true(Res == "aaa"),nondet]) :-
  TestDom = automaton_dom([1,2,3,4],[(1,range(97,97),2),(2,range(97,97),3),(3,range(97,97),1),(3,range(97,97),4)],[1],[4]),
  labeling([id_dfs],TestDom,"aaa"),
  labeling([id_dfs],TestDom,"aaaaaa"),
  labeling([id_dfs],TestDom,"aaaaaaaaa"),
  labeling([id_dfs],TestDom,Res).

test(two_infinite_trans_domain2,[true(Res == "a"),nondet]) :-
  TestDom = automaton_dom([1,2,3,4],[(1,range(97,97),2),(2,range(98,98),3),(3,epsilon,1),(1,range(97,97),4)],[1],[4]),
  labeling([id_dfs],TestDom,"a"),
  labeling([id_dfs],TestDom,"aba"),
  labeling([id_dfs],TestDom,"ababa"),
  labeling([id_dfs],TestDom,"abababa"),
  labeling([id_dfs],TestDom,Res).

test(repeat_concat_automaton,[true(Res == "b"),nondet]) :-
  single_char_domain("a",DomA),
  repeat(DomA,RepDomA),
  single_char_domain("b",DomB),
  concatenation(RepDomA,DomB,TestDom),
  labeling([id_dfs],TestDom,"b"),
  labeling([id_dfs],TestDom,"ab"),
  labeling([id_dfs],TestDom,"aab"),
  labeling([id_dfs],TestDom,"aaab"),
  labeling([id_dfs],TestDom,Res).

test(infinite_loop_no_goal,[true(Res == "a")]) :-
  TestDom = automaton_dom([1,2,3,4],[(1,range(97,97),2),(2,range(98,98),3),(3,epsilon,2),(1,range(97,97),4)],[1],[4]),
  labeling([id_dfs],TestDom,"a"),
  \+ labeling([id_dfs],TestDom,"ab"),
  \+ labeling([id_dfs],TestDom,"abab"),
  \+ labeling([id_dfs],TestDom,"aba"),
  \+ labeling([id_dfs],TestDom,Res).

:- end_tests(infinite_domains).


:- begin_tests(labeling_fail).

test(simple_fail,[fail]) :-
  labeling([id_dfs],a,"a").

test(fail_no_end_states,[fail]) :-
  TestDom = automaton_dom([1,2],[(1,range(97,97),2)],[1],[]),
  labeling([id_dfs],TestDom,_).

test(fail_wrong_label,[fail]) :-
  TestDom = automaton_dom([1,2,3],[(1,range(97,97),2),(2,range(98,98),3)],[1],[3]),
  labeling([id_dfs],TestDom,"ba").

test(fail_wrong_label,[fail]) :-
  constant_string_domain("abc",TestDom),
  labeling([id_dfs],TestDom,"a").

test(unreachable_end,[fail]) :-
  TestDom = automaton_dom([1,2,3],[(1,range(97,97),2)],[1],[3]),
  labeling([id_dfs],TestDom,_).

test(repeat_concat_automaton_fail,[fail]) :-
  single_char_domain("a",DomA),
  repeat(DomA,RepDomA),
  single_char_domain("b",DomB),
  concatenation(RepDomA,DomB,TestDom),
  labeling([id_dfs],TestDom,"").

:- end_tests(labeling_fail).


:- begin_tests(more_complex_domains).

test(labeling_specific_size_three,[all(Res == ["aba"])]) :-
  TestDom1 = automaton_dom([1,2,3],[(1,range(97,97),2),(2,range(98,98),3),(3,epsilon,1),(3,range(97,97),4)],[1],[4]),
  any_char_domain(AnyDom),
  repeat(AnyDom,3,TestDom2),
  intersection(TestDom1,TestDom2,LabelDom),
  labeling([id_dfs],LabelDom,Res).

test(labeling_specific_size_five,[all(Res == ["ababa","ababa"])]) :-
  TestDom1 = automaton_dom([1,2,3],[(1,range(97,97),2),(2,range(98,98),3),(3,epsilon,1),(3,range(97,97),4)],[1],[4]),
  any_char_domain(AnyDom),
  repeat(AnyDom,5,TestDom2),
  intersection(TestDom1,TestDom2,LabelDom),
  labeling([id_dfs],LabelDom,Res).

test(labeling_specific_size_five,[all(Res == ["abababa","abababa","abababa","abababa"])]) :-
  TestDom1 = automaton_dom([1,2,3],[(1,range(97,97),2),(2,range(98,98),3),(3,epsilon,1),(3,range(97,97),4)],[1],[4]),
  any_char_domain(AnyDom),
  repeat(AnyDom,7,TestDom2),
  intersection(TestDom1,TestDom2,LabelDom),
  labeling([id_dfs],LabelDom,Res).

test(labeling_specific_size_fail,[fail]) :-
  TestDom1 = automaton_dom([1,2,3],[(1,range(97,97),2),(2,range(98,98),3),(3,epsilon,1),(3,range(97,97),4)],[1],[4]),
  any_char_domain(AnyDom),
  repeat(AnyDom,6,TestDom2),
  intersection(TestDom1,TestDom2,LabelDom),
  labeling([id_dfs],LabelDom,_).

:- end_tests(more_complex_domains).
