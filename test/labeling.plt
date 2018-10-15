:- use_module(library(plunit)).

:- use_module('../src/domains/basic_domains').
:- use_module('../src/domains/basic_operations').
:- use_module('../src/domains/labeling').
%:- use_module('../src/domains/labeling',[unfold_tailrec/5,find_next_transition/5,alternative_transitions/5]).

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

test(any_repeat3_automaton,[all(Res == ["a","aa","aaa"])]) :-
  any_char_domain(Dom),
  repeat(Dom,1,3,TestDom),
  label(TestDom,Res).

test(any_repeat3_automaton,[true(Res == " "),nondet]) :-
  any_char_domain(Dom),
  repeat(Dom,1,3,TestDom),
  label(TestDom,"a"),
  label(TestDom,"ab"),
  label(TestDom,"abc"),
  label(TestDom,"A"),
  label(TestDom,"AB"),
  label(TestDom,"ABC"),
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

test(two_infinite_trans_domain1,[true(Res == "aaa"),nondet]) :-
  TestDom = automaton_dom([1,2,3,4],[(1,range(97,97),2),(2,range(97,97),3),(3,range(97,97),1),(3,range(97,97),4)],[1],[4]),
  label(TestDom,"aaa"),
  label(TestDom,"aaaaaa"),
  label(TestDom,"aaaaaaaaa"),
  label(TestDom,Res).

test(two_infinite_trans_domain2,[true(Res == "a"),nondet]) :-
  TestDom = automaton_dom([1,2,3,4],[(1,range(97,97),2),(2,range(98,98),3),(3,epsilon,1),(1,range(97,97),4)],[1],[4]),
  label(TestDom,"a"),
  label(TestDom,"aba"),
  label(TestDom,"ababa"),
  label(TestDom,"abababa"),
  label(TestDom,Res).

test(repeat_concat_automaton,[true(Res == "b"),nondet]) :-
  single_char_domain("a",DomA),
  repeat(DomA,RepDomA),
  single_char_domain("b",DomB),
  concatenation(RepDomA,DomB,TestDom),
  label(TestDom,"b"),
  label(TestDom,"ab"),
  label(TestDom,"aab"),
  label(TestDom,"aaab"),
  label(TestDom,Res).

test(infinite_loop_no_goal,[true(Res == "a"),nondet]) :-
  TestDom = automaton_dom([1,2,3,4],[(1,range(97,97),2),(2,range(98,98),3),(3,epsilon,2),(1,range(97,97),4)],[1],[4]),
  label(TestDom,"a"),
  \+ label(TestDom,"ab"),
  \+ label(TestDom,"abab"),
  \+ label(TestDom,"aba"),
  label(TestDom,Res).

:- end_tests(infinite_domains).


:- begin_tests(labeling_fail).

test(simple_fail,[fail]) :-
  label(a,"a").

test(fail_no_end_states,[fail]) :-
  TestDom = automaton_dom([1,2],[(1,range(97,97),2)],[1],[]),
  label(TestDom,_).

test(fail_wrong_label,[fail]) :-
  TestDom = automaton_dom([1,2,3],[(1,range(97,97),2),(2,range(98,98),3)],[1],[3]),
  label(TestDom,"ba").

test(fail_wrong_label,[fail]) :-
  constant_string_domain("abc",TestDom),
  label(TestDom,"a").

test(unreachable_end,[fail]) :-
  TestDom = automaton_dom([1,2,3],[(1,range(97,97),2)],[1],[3]),
  label(TestDom,_).

test(repeat_concat_automaton_fail,[fail]) :-
  single_char_domain("a",DomA),
  repeat(DomA,RepDomA),
  single_char_domain("b",DomB),
  concatenation(RepDomA,DomB,TestDom),
  label(TestDom,"").

:- end_tests(labeling_fail).


:- begin_tests(more_complex_domains).

test(labeling_specific_size_three,[all(Res == ["aba"])]) :-
  TestDom1 = automaton_dom([1,2,3],[(1,range(97,97),2),(2,range(98,98),3),(3,epsilon,1),(3,range(97,97),4)],[1],[4]),
  any_char_domain(AnyDom),
  repeat(AnyDom,3,TestDom2),
  intersection(TestDom1,TestDom2,LabelDom),
  label(LabelDom,Res).

test(labeling_specific_size_five,[all(Res == ["ababa","ababa"])]) :-
  TestDom1 = automaton_dom([1,2,3],[(1,range(97,97),2),(2,range(98,98),3),(3,epsilon,1),(3,range(97,97),4)],[1],[4]),
  any_char_domain(AnyDom),
  repeat(AnyDom,5,TestDom2),
  intersection(TestDom1,TestDom2,LabelDom),
  label(LabelDom,Res).

test(labeling_specific_size_five,[all(Res == ["abababa","abababa","abababa","abababa"])]) :-
  TestDom1 = automaton_dom([1,2,3],[(1,range(97,97),2),(2,range(98,98),3),(3,epsilon,1),(3,range(97,97),4)],[1],[4]),
  any_char_domain(AnyDom),
  repeat(AnyDom,7,TestDom2),
  intersection(TestDom1,TestDom2,LabelDom),
  label(LabelDom,Res).

test(labeling_specific_size_fail,[fail]) :-
  TestDom1 = automaton_dom([1,2,3],[(1,range(97,97),2),(2,range(98,98),3),(3,epsilon,1),(3,range(97,97),4)],[1],[4]),
  any_char_domain(AnyDom),
  repeat(AnyDom,6,TestDom2),
  intersection(TestDom1,TestDom2,LabelDom),
  label(LabelDom,_).

:- end_tests(more_complex_domains).


:- begin_tests(alternative_transitions).

test(at_unvisited_no_alt,[true(Actual == Expected),nondet]) :-
  TestTrans = [(1,epsilon,2),(2,range(97,97),3)],
  Test = (1,epsilon,2),
  His = history{},
  Expected = (1,epsilon,2),
  labeling:alternative_transitions(Test,TestTrans,His,His,Actual).

test(at_visited_no_alt,[true(Actual == Expected),nondet]) :-
  TestTrans = [(1,epsilon,2),(2,range(97,97),3)],
  Test = (1,epsilon,2),
  His = history{2:visited},
  Expected = (1,epsilon,2),
  labeling:alternative_transitions(Test,TestTrans,His,His,Actual).

test(at_unvisited_alt,[true((Actual,ActualHis) == (Expected,ExpectedHis)),nondet]) :-
  TestTrans = [(1,epsilon,2),(1,range(97,97),3)],
  Test = (1,epsilon,2),
  His = history{2:visited},
  Expected = (1,range(97,97),3),
  ExpectedHis = history{2:visited, 3:visited},
  labeling:alternative_transitions(Test,TestTrans,His,ActualHis,Actual).

test(at_visited_alt,[true(Actual == Expected),nondet]) :-
  TestTrans = [(1,epsilon,2),(1,range(97,97),3)],
  Test = (1,epsilon,2),
  His = history{2:visited, 3:visited},
  Expected = (1,epsilon,2),
  labeling:alternative_transitions(Test,TestTrans,His,His,Actual).

test(at_two_visited_alt,[true((Actual,ActualHis) == (Expected,ExpectedHis)),nondet]) :-
  TestTrans = [(1,epsilon,2),(1,range(97,97),3),(1,range(97,98),4)],
  Test = (1,epsilon,2),
  His = history{2:visited, 3:visited},
  Expected = (1,range(97,98),4),
  ExpectedHis = history{2:visited, 3:visited, 4:visited},
  labeling:alternative_transitions(Test,TestTrans,His,ActualHis,Actual).

:- end_tests(alternative_transitions).


:- begin_tests(find_next_transition).

test(fnt_unvisited_no_alt,[true((Actual,ActualHis) == (Expected,ExpectedHis)),nondet]) :-
  TestTrans = [(1,epsilon,2),(2,range(97,97),3)],
  His = history{},
  Expected = (1,epsilon,2),
  ExpectedHis = history{2:visited},
  labeling:find_next_transition(1,TestTrans,His,ActualHis,Actual).

test(fnt_visited_no_alt,[true(Actual == Expected),nondet]) :-
  TestTrans = [(1,epsilon,2),(2,range(97,97),3)],
  His = history{2:visited},
  Expected = (1,epsilon,2),
  labeling:find_next_transition(1,TestTrans,His,His,Actual).

test(fnt_unvisited_alt,[true((Actual,ActualHis) == (Expected,ExpectedHis)),nondet]) :-
  TestTrans = [(1,epsilon,2),(1,range(97,97),3)],
  His = history{2:visited},
  Expected = (1,range(97,97),3),
  ExpectedHis = history{2:visited, 3:visited},
  labeling:find_next_transition(1,TestTrans,His,ActualHis,Actual).

test(fnt_visited_alt,[true(Actual == Expected),nondet]) :-
  TestTrans = [(1,epsilon,2),(1,range(97,97),3)],
  His = history{2:visited, 3:visited},
  Expected = (1,epsilon,2),
  labeling:find_next_transition(1,TestTrans,His,His,Actual).

test(fnt_two_visited_alt,[true((Actual,ActualHis) == (Expected,ExpectedHis)),nondet]) :-
  TestTrans = [(1,epsilon,2),(1,range(97,97),3),(1,range(97,98),4)],
  His = history{2:visited, 3:visited},
  Expected = (1,range(97,98),4),
  ExpectedHis = history{2:visited, 3:visited, 4:visited},
  labeling:find_next_transition(1,TestTrans,His,ActualHis,Actual).

:- end_tests(find_next_transition).


:- begin_tests(unfold_tailrec).

test(uf_simple_automaton,[true(Res == [97]),nondet]) :-
  single_char_domain("a",TestDom),
  get_end_states(TestDom,Ends),
  get_transition(TestDom,Trans),
  History = history{1:visited},
  labeling:unfold_tailrec(1,Trans,Ends,History,Res).

test(uf_epsilon_can_be_labeled,[true(Res == []),nondet]) :-
  TestDom = automaton_dom([1,2],[(1,epsilon,2)],[1],[2]),
  get_end_states(TestDom,Ends),
  get_transition(TestDom,Trans),
  History = history{1:visited},
  labeling:unfold_tailrec(1,Trans,Ends,History,Res).

test(uf_simple_infinite_domain,[true(Res == [97]),nondet]) :-
  TestDom = automaton_dom([1,2],[(1,range(97,97),2),(2,range(97,97),1)],[1],[2]),
  get_end_states(TestDom,Ends),
  get_transition(TestDom,Trans),
  History = history{1:visited},
  labeling:unfold_tailrec(1,Trans,Ends,History,[97]),
  labeling:unfold_tailrec(1,Trans,Ends,History,[97,97,97]),
  labeling:unfold_tailrec(1,Trans,Ends,History,[97,97,97,97,97]),
  labeling:unfold_tailrec(1,Trans,Ends,History,Res).

test(uf_one_infinite_trans_domain,[true(Res == [97,97]),nondet]) :-
  TestDom = automaton_dom([1,2,3],[(1,range(97,97),2),(2,range(97,97),1),(2,range(97,97),3)],[1],[3]),
  get_end_states(TestDom,Ends),
  get_transition(TestDom,Trans),
  History = history{1:visited},
  labeling:unfold_tailrec(1,Trans,Ends,History,[97,97]),
  labeling:unfold_tailrec(1,Trans,Ends,History,[97,97,97,97]),
  labeling:unfold_tailrec(1,Trans,Ends,History,[97,97,97,97,97,97]),
  labeling:unfold_tailrec(1,Trans,Ends,History,Res).

test(uf_fail_no_end_states,[fail]) :-
  TestDom = automaton_dom([1,2],[(1,range(97,97),2)],[1],[]),
  get_end_states(TestDom,Ends),
  get_transition(TestDom,Trans),
  History = history{1:visited},
  labeling:unfold_tailrec(1,Trans,Ends,History,_).

test(uf_unreachable_end,[fail]) :-
  TestDom = automaton_dom([1,2,3],[(1,range(97,97),2)],[1],[3]),
  get_end_states(TestDom,Ends),
  get_transition(TestDom,Trans),
  History = history{1:visited},
  labeling:unfold_tailrec(1,Trans,Ends,History,_).

:- end_tests(unfold_tailrec).

:- begin_tests(unfold_tailrec_bfs).

test(uf_bfs_simple_automaton,[true(Res == [range(97,97)]),nondet]) :-
  single_char_domain("a",TestDom),
  get_end_states(TestDom,Ends),
  get_transition(TestDom,Trans),
  labeling:queue_new(Queue),
  labeling:unfold_tailrec_bfs(1,Trans,Ends,Queue,Res).

test(uf_bfs_epsilon_can_be_labeled,[true(Res == []),nondet]) :-
  TestDom = automaton_dom([1,2],[(1,epsilon,2)],[1],[2]),
  get_end_states(TestDom,Ends),
  get_transition(TestDom,Trans),
  labeling:queue_new(Queue),
  labeling:unfold_tailrec_bfs(1,Trans,Ends,Queue,Res).

test(uf_bfs_simple_infinite_domain,[true(Res == [range(97,97)]),nondet]) :-
  TestDom = automaton_dom([1,2],[(1,range(97,97),2),(2,range(97,97),1)],[1],[2]),
  get_end_states(TestDom,Ends),
  get_transition(TestDom,Trans),
  labeling:queue_new(Queue),
  labeling:unfold_tailrec_bfs(1,Trans,Ends,Queue,[range(97,97)]),
  labeling:unfold_tailrec_bfs(1,Trans,Ends,Queue,[range(97,97),range(97,97),range(97,97)]),
  labeling:unfold_tailrec_bfs(1,Trans,Ends,Queue,[range(97,97),range(97,97),range(97,97),range(97,97),range(97,97)]),
  labeling:unfold_tailrec_bfs(1,Trans,Ends,Queue,Res).

test(uf_bfs_one_infinite_trans_domain,[fixme("currently no labeling in bfs"),true(Res == [range(97,97),range(97,97)]),nondet]) :-
  TestDom = automaton_dom([1,2,3],[(1,range(97,97),2),(2,range(97,97),1),(2,range(97,97),3)],[1],[3]),
  get_end_states(TestDom,Ends),
  get_transition(TestDom,Trans),
  labeling:queue_new(Queue),
  labeling:unfold_tailrec_bfs(1,Trans,Ends,Queue,[range(97,97),range(97,97)]),
  labeling:unfold_tailrec_bfs(1,Trans,Ends,Queue,[range(97,97),range(97,97),range(97,97),range(97,97)]),
  labeling:unfold_tailrec_bfs(1,Trans,Ends,Queue,[range(97,97),range(97,97),range(97,97),range(97,97),range(97,97),range(97,97)]),
  labeling:unfold_tailrec_bfs(1,Trans,Ends,Queue,Res).

test(uf_bfs_fail_no_end_states,[fail]) :-
  TestDom = unfold_tailrec_bfs([1,2],[(1,range(97,97),2)],[1],[]),
  get_end_states(TestDom,Ends),
  get_transition(TestDom,Trans),
  labeling:queue_new(Queue),
  labeling:unfold_tailrec_bfs(1,Trans,Ends,Queue,_).

test(uf_bfs_unreachable_end,[fail]) :-
  TestDom = automaton_dom([1,2,3],[(1,range(97,97),2)],[1],[3]),
  get_end_states(TestDom,Ends),
  get_transition(TestDom,Trans),
  labeling:queue_new(Queue),
  labeling:unfold_tailrec_bfs(1,Trans,Ends,Queue,_).

:- end_tests(unfold_tailrec_bfs).
