:- use_module(library(plunit)).

:- use_module('../src/domains/basic_domains').
:- use_module('../src/domains/basic_operations').
:- use_module('../src/domains/labeling').

:- begin_tests(basic_domains).

test(constant,[all(Res == ["abc"])]) :-
  constant_string_domain("abc",D),
  labeling([any],D,Res).

test(simple_automaton,[all(Res == ["a"])]) :-
  single_char_domain("a",Dom),
  labeling([any],Dom,Res).

test(any_char_domain,[true(Res == " "),nondet]) :-
  any_char_domain(D),
  labeling([any],D,Res).

test(any_char_domain_can_return_different_character,[fail]) :-
  any_char_domain(D),
  labeling([any],D,Res),
  Res == "~".

test(epsilon_can_be_labeled,[all(Res == [""])]) :-
  D = automaton_dom([1,2],[(1,epsilon,2)],[1],[2]),
  labeling([any],D,Res).

:- end_tests(basic_domains).


:- begin_tests(domains_with_operations).

test(simple_concat_automaton,[all(Res == ["ab"])]) :-
  single_char_domain("a",Dom1),
  single_char_domain("b",Dom2),
  concatenation(Dom1,Dom2,TestDom),
  labeling([any],TestDom,Res).

test(simple_union_automaton,[all(Res == ["a","b"])]) :-
  single_char_domain("a",Dom1),
  single_char_domain("b",Dom2),
  union(Dom1,Dom2,TestDom),
  labeling([any],TestDom,Res).

test(two_char_union_automaton,[all(Res == ["ab","cd"])]) :-
  Dom1 = automaton_dom([1,2,3],[(1,range(97,97),2),(2,range(98,98),3)],[1],[3]),
  Dom2 = automaton_dom([1,2,3],[(1,range(99,99),2),(2,range(100,100),3)],[1],[3]),
  union(Dom1,Dom2,TestDom),
  labeling([any],TestDom,Res).

test(simple_repeat1_automaton,[true(Res == ""),nondet]) :-
  single_char_domain("a",Dom),
  repeat(Dom,TestDom),
  labeling([any],TestDom,""),
  labeling([any],TestDom,"a"),
  \+ labeling([any],TestDom,"aa"),
  \+ labeling([any],TestDom,"aaa"),
  labeling([any],TestDom,Res).

test(simple_repeat2_automaton,[all(Res == ["aaa"])]) :-
  single_char_domain("a",Dom),
  repeat(Dom,3,TestDom),
  labeling([any],TestDom,Res).

test(simple_repeat3_automaton,[all(Res == ["a","aa","aaa"])]) :-
  single_char_domain("a",Dom),
  repeat(Dom,1,3,TestDom),
  labeling([any],TestDom,Res).

test(any_repeat3_automaton,[true(Res == " "),nondet]) :-
  any_char_domain(Dom),
  repeat(Dom,1,3,TestDom),
  labeling([any],TestDom," "),
  labeling([any],TestDom,"  "),
  labeling([any],TestDom,"   "),
  \+ labeling([any],TestDom,"A"),
  \+ labeling([any],TestDom,"AB"),
  \+ labeling([any],TestDom,"ABC"),
  labeling([any],TestDom,Res).

test(simple_intersect_automaton,[all(Res == ["ab"])]) :-
  TestDom2 = automaton_dom([1,2,3],[(1,range(97,98),2),(2,range(97,98),3)],[1],[3]),
  single_char_domain("a",Dom1),
  single_char_domain("b",Dom2),
  concatenation(Dom1,Dom2,TestDom1),
  intersection(TestDom1,TestDom2,TestDom3),
  labeling([any],TestDom3,Res).

:- end_tests(domains_with_operations).


:- begin_tests(infinite_domains).

test(simple_infinite_domain,[true(Res == "a"),nondet]) :-
  TestDom = automaton_dom([1,2],[(1,range(97,97),2),(2,range(97,97),1)],[1],[2]),
  labeling([any],TestDom,"a"),
  \+ labeling([any],TestDom,"aaa"),
  \+ labeling([any],TestDom,"aaaaa"),
  labeling([any],TestDom,Res).

test(one_infinite_trans_domain,[true(Res == "aa"),nondet]) :-
  TestDom = automaton_dom([1,2,3],[(1,range(97,97),2),(2,range(97,97),1),(2,range(97,97),3)],[1],[3]),
  labeling([any],TestDom,"aa"),
  \+ labeling([any],TestDom,"aaaa"),
  \+ labeling([any],TestDom,"aaaaaa"),
  labeling([any],TestDom,Res).

test(two_infinite_trans_domain1,[true(Res == "aaa"),nondet]) :-
  TestDom = automaton_dom([1,2,3,4],[(1,range(97,97),2),(2,range(97,97),3),(3,range(97,97),1),(3,range(97,97),4)],[1],[4]),
  labeling([any],TestDom,"aaa"),
  \+ labeling([any],TestDom,"aaaaaa"),
  \+ labeling([any],TestDom,"aaaaaaaaa"),
  labeling([any],TestDom,Res).

test(two_infinite_trans_domain2,[true(Res == "a"),nondet]) :-
  TestDom = automaton_dom([1,2,3,4],[(1,range(97,97),2),(2,range(98,98),3),(3,epsilon,1),(1,range(97,97),4)],[1],[4]),
  labeling([any],TestDom,"a"),
  \+ labeling([any],TestDom,"aba"),
  \+ labeling([any],TestDom,"ababa"),
  \+ labeling([any],TestDom,"abababa"),
  labeling([any],TestDom,Res).

test(repeat_concat_automaton,[true(Res == "ab"),nondet]) :-
  single_char_domain("a",DomA),
  repeat(DomA,RepDomA),
  single_char_domain("b",DomB),
  concatenation(RepDomA,DomB,TestDom),
  labeling([any],TestDom,"b"),
  labeling([any],TestDom,"ab"),
  \+ labeling([any],TestDom,"aab"),
  \+ labeling([any],TestDom,"aaab"),
  labeling([any],TestDom,Res).

test(infinite_loop_no_goal,[true(Res == "a"),nondet]) :-
  TestDom = automaton_dom([1,2,3,4],[(1,range(97,97),2),(2,range(98,98),3),(3,epsilon,2),(1,range(97,97),4)],[1],[4]),
  labeling([any],TestDom,"a"),
  \+ labeling([any],TestDom,"ab"),
  \+ labeling([any],TestDom,"abab"),
  \+ labeling([any],TestDom,"aba"),
  labeling([any],TestDom,Res).

:- end_tests(infinite_domains).


:- begin_tests(labeling_fail).

test(simple_fail,[fail]) :-
  labeling([any],a,"a").

test(fail_no_end_states,[fail]) :-
  TestDom = automaton_dom([1,2],[(1,range(97,97),2)],[1],[]),
  labeling([any],TestDom,_).

test(fail_wrong_label,[fail]) :-
  TestDom = automaton_dom([1,2,3],[(1,range(97,97),2),(2,range(98,98),3)],[1],[3]),
  labeling([any],TestDom,"ba").

test(fail_wrong_label,[fail]) :-
  constant_string_domain("abc",TestDom),
  labeling([any],TestDom,"a").

test(unreachable_end,[fail]) :-
  TestDom = automaton_dom([1,2,3],[(1,range(97,97),2)],[1],[3]),
  labeling([any],TestDom,_).

test(repeat_concat_automaton_fail,[fail]) :-
  single_char_domain("a",DomA),
  repeat(DomA,RepDomA),
  single_char_domain("b",DomB),
  concatenation(RepDomA,DomB,TestDom),
  labeling([any],TestDom,"").

:- end_tests(labeling_fail).


:- begin_tests(more_complex_domains).

test(labeling_specific_size_three,[all(Res == ["aba"])]) :-
  TestDom1 = automaton_dom([1,2,3],[(1,range(97,97),2),(2,range(98,98),3),(3,epsilon,1),(3,range(97,97),4)],[1],[4]),
  any_char_domain(AnyDom),
  repeat(AnyDom,3,TestDom2),
  intersection(TestDom1,TestDom2,LabelDom),
  labeling([any],LabelDom,Res).

test(labeling_specific_size_five,[all(Res == ["ababa","ababa"])]) :-
  TestDom1 = automaton_dom([1,2,3],[(1,range(97,97),2),(2,range(98,98),3),(3,epsilon,1),(3,range(97,97),4)],[1],[4]),
  any_char_domain(AnyDom),
  repeat(AnyDom,5,TestDom2),
  intersection(TestDom1,TestDom2,LabelDom),
  labeling([any],LabelDom,Res).

test(labeling_specific_size_five,[all(Res == ["abababa","abababa","abababa","abababa"])]) :-
  TestDom1 = automaton_dom([1,2,3],[(1,range(97,97),2),(2,range(98,98),3),(3,epsilon,1),(3,range(97,97),4)],[1],[4]),
  any_char_domain(AnyDom),
  repeat(AnyDom,7,TestDom2),
  intersection(TestDom1,TestDom2,LabelDom),
  labeling([any],LabelDom,Res).

test(labeling_specific_size_fail,[fail]) :-
  TestDom1 = automaton_dom([1,2,3],[(1,range(97,97),2),(2,range(98,98),3),(3,epsilon,1),(3,range(97,97),4)],[1],[4]),
  any_char_domain(AnyDom),
  repeat(AnyDom,6,TestDom2),
  intersection(TestDom1,TestDom2,LabelDom),
  labeling([any],LabelDom,_).

:- end_tests(more_complex_domains).
