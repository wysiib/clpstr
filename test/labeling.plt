:- use_module(library(plunit)).

:- use_module('../src/domains/basic_domains').
:- use_module('../src/domains/basic_operations').
:- use_module('../src/domains/labeling').

:- begin_tests(basic_domains).

test(constant,[all(Res == ["abc"])]) :-
  constant_string_domain("abc",D),
  label(D,Res).

test(simple_automaton,[all(Res == ["a"])]) :-
  single_char_domain("a",Dom),
  label(Dom,Res).

test(any_char_domain,[true(Res == " "),nondet]) :-
  any_char_domain(D),
  label(D,Res).

test(any_char_domain_can_return_different_character,[nondet]) :-
  any_char_domain(D),
  label(D,Res),
  Res == "~".

test(epsilon_can_be_labeled,[all(Res == [""])]) :-
  D = automaton_dom([1,2],[(1,epsilon,2)],[1],[2]),
  label(D,Res).

:- end_tests(basic_domains).

:- begin_tests(domains_with_operations).

test(simple_concat_automaton,[all(Res == ["ab"])]) :-
  single_char_domain("a",Dom1),
  single_char_domain("b",Dom2),
  concatenation(Dom1,Dom2,TestDom),
  label(TestDom,Res).

test(simple_union_automaton,[all(Res == ["a","b"])]) :-
  single_char_domain("a",Dom1),
  single_char_domain("b",Dom2),
  union(Dom1,Dom2,TestDom),
  label(TestDom,Res).

test(simple_repeat1_automaton,[true(Res == ""),nondet]) :-
  single_char_domain("a",Dom),
  repeat(Dom,TestDom),
  label(TestDom,""),
  label(TestDom,"a"),
  label(TestDom,"aa"),
  label(TestDom,"aaa"),
  label(TestDom,Res).

test(simple_repeat2_automaton,[all(Res == ["aaa"])]) :-
  single_char_domain("a",Dom),
  repeat(Dom,3,TestDom),
  label(TestDom,Res).

test(simple_repeat3_automaton,[all(Res == ["a","aa","aaa"])]) :-
  single_char_domain("a",Dom),
  repeat(Dom,1,3,TestDom),
  label(TestDom,Res).

test(simple_intersect_automaton,[all(Res == ["ab"])]) :-
  TestDom2 = automaton_dom([1,2,3],[(1,range(97,98),2),(2,range(97,98),3)],[1],[3]),
  single_char_domain("a",Dom1),
  single_char_domain("b",Dom2),
  concatenation(Dom1,Dom2,TestDom1),
  intersection(TestDom1,TestDom2,TestDom3),
  label(TestDom3,Res).

:- end_tests(domains_with_operations).

:- begin_tests(infinite_domains).

test(simple_infinite_domain,[true(Res == "a"),nondet]) :-
  TestDom = automaton_dom([1,2],[(1,range(97,97),2),(2,range(97,97),1)],[1],[2]),
  label(TestDom,"a"),
  label(TestDom,"aaa"),
  label(TestDom,"aaaaa"),
  label(TestDom,Res).

test(one_infinite_trans_domain,[true(Res == "aa"),nondet]) :-
  TestDom = automaton_dom([1,2,3],[(1,range(97,97),2),(2,range(97,97),1),(2,range(97,97),3)],[1],[3]),
  label(TestDom,"aa"),
  label(TestDom,"aaaa"),
  label(TestDom,"aaaaaa"),
  label(TestDom,Res).

test(repeat_concat_automaton,[true(Res == ""),nondet]) :-
  single_char_domain("a",DomA),
  repeat(DomA,RepDomA),
  single_char_domain("b",DomB),
  concatenation(RepDomA,DomB,TestDom),
  label(TestDom,"b"),
  label(TestDom,"ab"),
  label(TestDom,"aab"),
  label(TestDom,"aaab"),
  label(TestDom,Res).

:- end_tests(infinite_domains).
