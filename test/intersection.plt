:- use_module(library(plunit)).

:- use_module('../src/domains/basic_domains').
:- use_module('../src/domains/basic_operations').
%:- use_module('../src/domains/basic_operations',[state_in_state_product/4]).

/**
* basic_operations intersection tests.
*/


:- begin_tests(constant_domains).

test(constant_domain_intersection_same,[true(D == D1)]) :-
  constant_string_domain("abc",D1),
  intersection(D1,D1,D).
test(constant_domain_intersection_different,[true(D == empty)]) :-
  constant_string_domain("abc",D1),
  constant_string_domain("def",D2),
  intersection(D1,D2,D).

:- end_tests(constant_domains).

:- begin_tests(mixed_domains).

test(simple_mixed_one_char,[true(D == string_dom("a"))]) :-
  constant_string_domain("a",D1),
  any_char_domain(D2),
  intersection(D1,D2,D).
test(simple_mixed_one_char_reverse,[true(D == string_dom("a"))]) :-
  constant_string_domain("a",D1),
  any_char_domain(D2),
  intersection(D2,D1,D).
test(simple_mixed_card,[true(D == empty)]) :-
  constant_string_domain("ab",D1),
  any_char_domain(D2),
  intersection(D1,D2,D).
test(simple_mixed_card_reverse,[true(D == empty)]) :-
  constant_string_domain("ab",D1),
  any_char_domain(D2),
  intersection(D2,D1,D).

:- end_tests(mixed_domains).

:- begin_tests(automaton_domains).

test(any_char_single_char_automaton_intersection,[true(Res == automaton_dom([1,2],[(1,range(97,97),2)],[1],[2]))]) :-
  any_char_domain(D1),
  single_char_domain("a",D2),
  intersection(D1,D2,Res).

test(empty_result1,[true(Res == empty)]) :-
  Test = automaton_dom([1,2,3],[(1,range(97,97),2),(2,range(98,98),3)],[1],[3]),
  single_char_domain("a",D2),
  intersection(Test,D2,Res).

test(empty_result2,[true(Res == empty)]) :-
  Test = automaton_dom([1,2,3],[(1,range(98,98),2),(1,range(97,97),2)],[1],[3]),
  single_char_domain("a",D2),
  intersection(Test,D2,Res).

test(empty_result3,[true(Res == empty)]) :-
  Test = automaton_dom([1,2,3],[(1,range(97,98),2),(1,range(97,98),2)],[1],[3]),
  single_char_domain("a",D2),
  intersection(Test,D2,Res).

test(single_eps_trans_two_times_intersec,[true(Actual == Expected)]) :-
  Expected = automaton_dom([1,2,3,4],[(1,epsilon,2),(2,range(97,97),3),(3,epsilon,4)],[1],[4]),
  Testdom1 = automaton_dom([1,2,3],[(1,epsilon,2),(2,range(97,97),3)],[1],[3]),
  Testdom2 = automaton_dom([1,2,3],[(1,range(97,97),2),(2,epsilon,3)],[1],[3]),
  intersection(Testdom1,Testdom2,Actual).

test(two_eps_trans_intersec,[true(Actual == Expected)]) :-
  Expected = automaton_dom([1,2,3,4],[(1,epsilon,2),(2,epsilon,3),(3,range(97,97),4)],[1],[4]),
  Testdom1 = automaton_dom([1,2],[(1,range(97,97),2)],[1],[2]),
  Testdom2 = automaton_dom([1,2,3,4],[(1,epsilon,2),(2,epsilon,3),(3,range(97,97),4)],[1],[4]),
  intersection(Testdom1,Testdom2,Actual).

% This tests produces an unleavable trahs state.
% From 4, there exist no transitions to other states,
% but there exist a path from start state 1 to state 4.
test(single_eps_trans_two_times_intersec_thrash_state,[true(Actual == Expected)]) :-
  Expected = automaton_dom([1,2,3,4,5,6],[(1,epsilon,2),(2,range(97,97),5),(5,epsilon,6),(1,range(97,97),3),(3,epsilon,4),(3,range(97,97),6)],[1],[6]),
  Testdom1 = automaton_dom([1,2,3],[(1,epsilon,2),(1,range(97,97),2),(2,range(97,97),3)],[1],[3]),
  Testdom2 = automaton_dom([1,2,3],[(1,range(97,97),2),(2,epsilon,3),(2,range(97,97),3)],[1],[3]),
  intersection(Testdom1,Testdom2,Actual).

test(concat_intersec,[true(Res == Expected)]) :-
  Expected = automaton_dom([1,2,3,4],[(1,range(97,97),2),(2,epsilon,3),(3,range(98,98),4)],[1],[4]),
  Test = automaton_dom([1,2,3],[(1,range(97,98),2),(2,range(97,98),3)],[1],[3]),
  single_char_domain("a",D1),
  single_char_domain("b",D2),
  concatenation(D1,D2,D3),
  intersection(Test,D3,Res).

:- end_tests(automaton_domains).

:- begin_tests(empty_domains).

test(is_empty) :-
  constant_string_domain("abc",D1),
  constant_string_domain("def",D2),
  intersection(D1,D2,D),
  is_empty(D).

:- end_tests(empty_domains).


/*:- begin_tests(subrange).

test(simple_range, [Res == Expected]) :-
  Testdelta = []

:- end_tests(subrange).*/

:- begin_tests(sisproduct).

% For these testcases
% automaton 1:
% automaton_dom([1,2],[(1,range(97,98),2)],[1],[2]).
%
% automaton 2:
% automaton_dom([1,2,3],[(1,range(97,97),2),(1,epsilon,2),(2,range(98,98),3)],[1],[3]).

test(single_trans1,[true(Res == Expected)]) :-
  Expected = (1,range(97,97),5),
  Testdelta1 = [(1,range(97,98),2)],
  Testdelta2 = [(1,range(97,97),2)],
  basic_operations:state_in_state_product(Testdelta1,Testdelta2,3,Res).

test(single_trans2,[true(Res == Expected)]) :-
  Expected = (2,range(98,98),6),
  Testdelta1 = [(1,range(97,98),2)],
  Testdelta2 = [(2,range(98,98),3)],
  basic_operations:state_in_state_product(Testdelta1,Testdelta2,3,Res).

test(single_trans3,[true(Res == Expected)]) :-
  Expected = (1,epsilon,2),
  Testdelta1 = [(1,range(97,98),2)],
  Testdelta2 = [(1,epsilon,2)],
  basic_operations:state_in_state_product(Testdelta1,Testdelta2,3,Res).

test(double_trans,all(Res == [(1,range(97,97),5),(1,epsilon,2)])) :-
  %Expected = [(1,range(97,97),2),(1,epsilon,2)],
  Testdelta1 = [(1,range(97,98),2)],
  Testdelta2 = [(1,range(97,97),2),(1,epsilon,2)],
  basic_operations:state_in_state_product(Testdelta1,Testdelta2,3,Res).


% For this testcase
% automaton 1:
% automaton_dom([1,2],[(1,epsilon,2)],[1],[2]).
%
% automaton 2:
% automaton_dom([1,2],[(1,epsilon,2)],[1],[2]).

test(double_eps_trans,[true(Res == Expected)]) :-
  Expected = (1,epsilon,4),
  Testdelta = [(1,epsilon,2)],
  basic_operations:state_in_state_product(Testdelta,Testdelta,2,Res).


% For this testcase
% automaton 1:
% automaton_dom([1,2,3],[(1,range(97,97),2),(2,epsilon,3)],[1],[3]).
%
% automaton 2:
% automaton_dom([1,2,3],[(1,epsilon,2),(2,range(97,97),3)],[1],[3]).

test(single_eps_trans_two_times,all(Res == [(1,epsilon,4),(2,epsilon,6),(4,range(97,97),8),(5,epsilon,6)])) :-
  %Expected = [(1,epsilon,4),(2,epsilon,6),(4,range(97,97),8),(5,epsilon,6)]
  % (8,epsilon,9) is missing because no epsilon selfloops on state 3
  Testdelta1 = [(1,epsilon,2),(2,range(97,97),3)],
  Testdelta2 = [(1,range(97,97),2),(2,epsilon,3)],
  basic_operations:state_in_state_product(Testdelta1,Testdelta2,3,Res).


% For this testcase
% automaton 1:
% automaton_dom([1,2,3],[(1,range(97,98),2),(2,range(97,98),3)],[1],[3]).
%
% automaton 2:
% automaton_dom([1,2,3,4],[(1,range(97,97),2),(2,epsilon,3),(3,range(98,98),4)],[1],[4]).
% aka
% single_char_domain("a",D1), single_char_domain("b",D2), concatenation(D1,D2,D3).

test(simple_delta,[true(Res == Expected)]) :-
  Expected = (5,range(97,97),10),
  Testdelta1 = [(2,range(97,98),3)],
  Testdelta2 = [(1,range(97,97),2)],
  basic_operations:state_in_state_product(Testdelta1,Testdelta2,4,Res).

test(cross_epsilon,[true(Res == Expected)]) :-
  Expected = (6,epsilon,7),
  Testdelta1 = [(2,range(97,98),3)],
  Testdelta2 = [(2,epsilon,3)],
  basic_operations:state_in_state_product(Testdelta1,Testdelta2,4,Res).

% same result as in concat_intersec above
test(cross_epsilon,[true(Res == Expected)]) :-
  Expected = (6,epsilon,7),
  Testdelta1 = [(2,range(97,98),3)],
  Testdelta2 = [(2,epsilon,3)],
  basic_operations:state_in_state_product(Testdelta1,Testdelta2,4,Res).


% For this testcase
% automaton 1:
% automaton_dom([1,2,3],[(1,range(97,98),2),(2,range(97,98),3)],[1],[3]).
%
% automaton 1:
% single_char_domain("a",D1),
% single_char_domain("b",D2),
% concatenation(D1,D2,D3).
% automaton_dom([1,2,3,4],[(1,range(97,98),2),(2,epsilon,3),(3,range(97,98),4)],[1],[4]).
%
% plus epsilon self loops on the accepting states
%
% see concat_intersec test

test(concat_sisproduct_findall,[true(Actual == Expected)]) :-
  Expected = [(1,range(97,97),6),(2,epsilon,3),(4,epsilon,4),(6,epsilon,7),(7,range(98,98),12),(8,epsilon,8),(9,epsilon,9),(10,epsilon,11),(11,epsilon,11),(12,epsilon,12)],
  Testdelta1 = [(1,range(97,97),2),(2,range(98,98),3),(3,epsilon,3)],
  Testdelta2 = [(1,range(97,97),2),(2,epsilon,3),(3,range(98,98),4),(4,epsilon,4)],
  findall(Delta,basic_operations:state_in_state_product(Testdelta1,Testdelta2,4,Delta),Actual).


:- end_tests(sisproduct).


:- begin_tests(infinite_domains).

% These testcases represent the str_prefix str_infix and str_suffix testcases
% from clpstr.plt respectively  

test(intersec_prefix,[true]) :-
  Dom1 = automaton_dom([1,2,3,4,5],[(1,range(116,116),2),(2,range(114,114),3),(3,range(117,117),4),(4,range(101,101),5)],[1],[5]),
  Dom2 = automaton_dom([1,2,3,4,5,6,7],
    [(1,range(116,116),2),(2,range(114,114),3),(3,range(117,117),4),(4,range(101,101),5),
    (5,epsilon,6),(6,range(32,126),7),(7,epsilon,6)],
    [1],[6,7]),
  intersection(Dom1,Dom2,Res),
  Res \= empty, Res \= [].

test(intersec_suffix,[true]) :-
  Dom1 = automaton_dom([1,2,3,4,5],[(1,range(116,116),2),(2,range(114,114),3),(3,range(117,117),4),(4,range(101,101),5)],[1],[5]),
  Dom2 = automaton_dom([1,2,3,4,5,6,7],
    [(1,range(32,126),2),(2,epsilon,1),(1,epsilon,3),(2,epsilon,3),
    (3,range(116,116),4),(4,range(114,114),5),(5,range(117,117),6),(6,range(101,101),7)],
    [1],[7]),
  intersection(Dom1,Dom2,Res),
  Res \= empty, Res \= [].

test(intersec_infix,[true]) :-
  Dom1 = automaton_dom([1,2,3,4,5],[(1,range(116,116),2),(2,range(114,114),3),(3,range(117,117),4),(4,range(101,101),5)],[1],[5]),
  Dom2 = automaton_dom([1,2,3,4,5,6,7],
    [(1,range(32,126),2),(2,epsilon,1),(1,epsilon,3),(2,epsilon,3),
    (3,range(116,116),4),(4,range(114,114),5),(5,range(117,117),6),(6,range(101,101),7),
    (7,epsilon,8),(8,range(32,126),9),(9,epsilon,8)],
    [1],[8,9]),
  intersection(Dom1,Dom2,Res),
  Res \= empty, Res \= [].

test(intersec_prefix,[true]) :-
  Dom1 = automaton_dom([1,2,3,4,5,6,7],
    [(1,range(116,116),2),(2,range(114,114),3),(3,range(117,117),4),(4,range(101,101),5),
    (5,epsilon,6),(6,range(32,126),7),(7,epsilon,6)],
    [1],[6,7]),
  Dom2 = automaton_dom([1,2,3,4,5,6,7,8],
    [(1,range(32,126),2),(2,epsilon,3),(3,range(32,126),4),(4,epsilon,5),(5,range(32,126),6),(6,epsilon,7),(7,range(32,126),8)],
    [1],[8]),
  intersection(Dom1,Dom2,Res),
  Res \= empty, Res \= [].

test(intersec_suffix,[true]) :-
  Dom1 = automaton_dom([1,2,3,4,5,6,7],
    [(1,range(32,126),2),(2,epsilon,1),(1,epsilon,3),
    (3,range(116,116),4),(4,range(114,114),5),(5,range(117,117),6),(6,range(101,101),7)],
    [1],[7]),
  Dom2 = automaton_dom([1,2,3,4,5,6,7,8],
    [(1,range(32,126),2),(2,epsilon,3),(3,range(32,126),4),(4,epsilon,5),(5,range(32,126),6),(6,epsilon,7),(7,range(32,126),8)],
    [1],[8]),
  intersection(Dom1,Dom2,Res),
  Res \= empty, Res \= [].

test(intersec_infix,[true]) :-
  Dom1 = automaton_dom([1,2,3,4,5,6,7,8,9,10,11,12],
    [(1,range(32,126),2),(2,epsilon,1),(1,epsilon,3),(2,epsilon,3),
    (3,range(116,116),4),(4,epsilon,5),(5,range(114,114),6),(6,epsilon,7),(7,range(117,117),8),(8,epsilon,9),(9,range(101,101),10),
    (10,epsilon,11),(11,range(32,126),12),(12,epsilon,11)],
    [1],[11,12]),
  Dom2 = automaton_dom([1,2,3,4,5,6,7,8],
    [(1,range(32,126),2),(2,epsilon,3),(3,range(32,126),4),(4,epsilon,5),(5,range(32,126),6),(6,epsilon,7),(7,range(32,126),8)],
    [1],[8]),
  intersection(Dom1,Dom2,Res),
  Res \= empty, Res \= [].

:- end_tests(infinite_domains).
