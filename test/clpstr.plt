:- use_module(library(plunit)).
:- use_module(library(clpfd)).
:- use_module(library(chr), [find_chr_constraint/1]).

:- use_module('../src/clpstr').
:- use_module('../src/domains/basic_domains').
:- use_module('../src/domains/basic_operations').
:- use_module('../src/domains/labeling').

:- begin_tests(str_labeling).

test(simple_string,[true(X == "true")]) :-
  constant_string_domain("true",Dom),
  str_in(X,Dom),
  str_labeling([],[X]).

test(simple_generated_dom,[true(X == "true")]) :-
  generate_domain("true",Dom),
  str_in(X,Dom),
  str_labeling([],[X]).

test(simple_concat_dom,[true(Res == ["tr","ue","true"])]) :-
  generate_domain("tr",Dom1),
  str_in(X,Dom1),
  generate_domain("ue",Dom2),
  str_in(Y,Dom2),
  str_concatenation(X,Y,Z),
  Res = [X,Y,Z],
  str_labeling([],Res).

test(simple_concat_dom_label_twice,[true((Res,Res2) == (["true"],["tr","ue"]))]) :-
  generate_domain("tr",Dom1),
  str_in(X,Dom1),
  generate_domain("ue",Dom2),
  str_in(Y,Dom2),
  str_concatenation(X,Y,Z),
  Res = [Z],
  Res2 = [X,Y],
  str_labeling([],Res),
  str_labeling([],Res2).

test(simple_concat_dom_only_label_concat,[true(Res == ["true"])]) :-
  generate_domain("tr",Dom1),
  str_in(X,Dom1),
  generate_domain("ue",Dom2),
  str_in(Y,Dom2),
  str_concatenation(X,Y,Z),
  Res = [Z],
  str_labeling([],Res).

test(simple_label_twice,[true(Res == ["tr","ue"])]) :-
  generate_domain("tr",Dom1),
  str_in(X,Dom1),
  generate_domain("ue",Dom2),
  str_in(Y,Dom2),
  Res = [X,Y],
  str_labeling([],Res).

test(simple_label_choice_twice,[all(X == ["tr","ue"])]) :-
  generate_domain("tr|ue",Dom),
  str_in(X,Dom),
  str_labeling([],[X]).

:- end_tests(str_labeling).

:- begin_tests(str_label).

test(str_label_simple_string,[true(X == "true")]) :-
  constant_string_domain("true",Dom),
  str_in(X,Dom),
  str_label([X]).

test(str_label_simple_generated_dom,[true(X == "true")]) :-
  generate_domain("true",Dom),
  str_in(X,Dom),
  str_label([X]).

test(str_label_simple_concat_dom,[true(Res == ["tr","ue","true"])]) :-
  generate_domain("tr",Dom1),
  str_in(X,Dom1),
  generate_domain("ue",Dom2),
  str_in(Y,Dom2),
  str_concatenation(X,Y,Z),
  Res = [X,Y,Z],
  str_label(Res).

test(str_label_simple_concat_dom_label_twice,[true((Res,Res2) == (["true"],["tr","ue"]))]) :-
  generate_domain("tr",Dom1),
  str_in(X,Dom1),
  generate_domain("ue",Dom2),
  str_in(Y,Dom2),
  str_concatenation(X,Y,Z),
  Res = [Z],
  Res2 = [X,Y],
  str_label(Res),
  str_label(Res2).

test(str_label_simple_concat_dom_only_label_concat,[true(Res == ["true"])]) :-
  generate_domain("tr",Dom1),
  str_in(X,Dom1),
  generate_domain("ue",Dom2),
  str_in(Y,Dom2),
  str_concatenation(X,Y,Z),
  Res = [Z],
  str_label(Res).

test(str_label_simple_label_twice,[true(Res == ["tr","ue"])]) :-
  generate_domain("tr",Dom1),
  str_in(X,Dom1),
  generate_domain("ue",Dom2),
  str_in(Y,Dom2),
  Res = [X,Y],
  str_label(Res).

test(str_label_simple_label_choice_twice,[all(X == ["tr","ue"])]) :-
  generate_domain("tr|ue",Dom),
  str_in(X,Dom),
  str_label([X]).

:- end_tests(str_label).

:- begin_tests(str_size).

test(simple_size_without_labeling,[true]) :-
  str_size(_,3).

test(simple_size_collision,[fail]) :-
  str_size(X,3),
  str_size(X,4).

test(size_in_collision_fail,[fail]) :-
  str_size(X,3),
  constant_string_domain("abcd",D),
  str_in(X,D).

test(size_in_collision_succeed,[true]) :-
  str_size(X,3),
  constant_string_domain("abc",D),
  str_in(X,D).


:- end_tests(str_size).


:- begin_tests(operational_constraints).

test(simple_str_concat,[true]):-
  generate_domain("tr",Dom1),
  str_in(X,Dom1),
  generate_domain("ue",Dom2),
  str_in(Y,Dom2),
  str_concatenation(X,Y,_).

test(str_concat_compare_string,[true,nondet]):-
  generate_domain("tr",Dom1),
  str_in(X,Dom1),
  generate_domain("ue",Dom2),
  str_in(Y,Dom2),
  str_concatenation(X,Y,Z),
  % in this case concat will accept strings, because of reg_ex_parser
  %EDom1 = automaton_dom([1,2,3],[(1,range(116,116),2),(2,range(114,114),3)],[1],[3]),
  %EDom2 = automaton_dom([1,2,3],[(1,range(117,117),2),(2,range(101,101),3)],[1],[3]),
  constant_string_domain("tr",EDom1),
  constant_string_domain("ue",EDom2),
  concatenation(EDom1,EDom2,Edom3),
  find_chr_constraint(str_in(Z,Edom3)).

test(str_concat_compare_automaton,[true,nondet]):-
  EDom1 = automaton_dom([1,2,3],[(1,range(116,116),2),(2,range(114,114),3)],[1],[3]),
  EDom2 = automaton_dom([1,2,3],[(1,range(117,117),2),(2,range(101,101),3)],[1],[3]),
  str_in(X,EDom1),
  str_in(Y,EDom2),
  str_concatenation(X,Y,Z),
  concatenation(EDom1,EDom2,Edom3),
  find_chr_constraint(str_in(Z,Edom3)).

test(simple_str_repeat1,[true]):-
  generate_domain("true",Dom1),
  str_in(X,Dom1),
  str_repeat(X,_).

test(str_repeat1_compare_string,[true,nondet]):-
  generate_domain("true",Dom1),
  str_in(X,Dom1),
  str_repeat(X,Y),
  %EDom1 = automaton_dom([1,2,3,4,5],[(1,range(116,116),2),(2,range(114,114),3),(3,range(117,117),4),(4,range(101,101),5)],[1],[5]),
  constant_string_domain("true",EDom1),
  repeat(EDom1,Edom2),
  find_chr_constraint(str_in(Y,Edom2)).

test(str_repeat1_compare_automaton,[true,nondet]):-
  EDom1 = automaton_dom([1,2,3,4,5],[(1,range(116,116),2),(2,range(114,114),3),(3,range(117,117),4),(4,range(101,101),5)],[1],[5]),
  str_in(X,EDom1),
  str_repeat(X,Y),
  repeat(EDom1,Edom2),
  find_chr_constraint(str_in(Y,Edom2)).

test(simple_str_repeat2,[true]):-
  generate_domain("true",Dom1),
  str_in(X,Dom1),
  str_repeat(X,5,_).

test(str_repeat2_compare_string,[true,nondet]):-
  generate_domain("true",Dom1),
  str_in(X,Dom1),
  str_repeat(X,2,Y),
  %EDom1 = automaton_dom([1,2,3,4,5],[(1,range(116,116),2),(2,range(114,114),3),(3,range(117,117),4),(4,range(101,101),5)],[1],[5]),
  constant_string_domain("true",EDom1),
  repeat(EDom1,2,Edom2),
  find_chr_constraint(str_in(Y,Edom2)).

test(str_repeat2_compare_string,[true,nondet]):-
  EDom1 = automaton_dom([1,2,3,4,5],[(1,range(116,116),2),(2,range(114,114),3),(3,range(117,117),4),(4,range(101,101),5)],[1],[5]),
  str_in(X,EDom1),
  str_repeat(X,2,Y),
  repeat(EDom1,2,Edom2),
  find_chr_constraint(str_in(Y,Edom2)).

test(simple_str_repeat3,[true]):-
  generate_domain("true",Dom1),
  str_in(X,Dom1),
  str_repeat(X,5,10,_).

test(str_repeat3_compare_string,[true,nondet]):-
  generate_domain("true",Dom1),
  str_in(X,Dom1),
  str_repeat(X,5,10,Y),
  %EDom1 = automaton_dom([1,2,3,4,5],[(1,range(116,116),2),(2,range(114,114),3),(3,range(117,117),4),(4,range(101,101),5)],[1],[5]),
  constant_string_domain("true",EDom1),
  repeat(EDom1,5,10,Edom2),
  find_chr_constraint(str_in(Y,Edom2)).

test(str_repeat3_compare_string,[true,nondet]):-
  EDom1 = automaton_dom([1,2,3,4,5],[(1,range(116,116),2),(2,range(114,114),3),(3,range(117,117),4),(4,range(101,101),5)],[1],[5]),
  str_in(X,EDom1),
  str_repeat(X,5,10,Y),
  repeat(EDom1,5,10,Edom2),
  find_chr_constraint(str_in(Y,Edom2)).

test(simple_str_union,[true]):-
  generate_domain("tr",Dom1),
  str_in(X,Dom1),
  generate_domain("ue",Dom2),
  str_in(Y,Dom2),
  str_union(X,Y,_).

test(str_union_compare_string,[true,nondet]):-
  generate_domain("tr",Dom1),
  str_in(X,Dom1),
  generate_domain("ue",Dom2),
  str_in(Y,Dom2),
  str_union(X,Y,Z),
  % in this case concat will accept strings, because of reg_ex_parser
  %EDom1 = automaton_dom([1,2,3],[(1,range(116,116),2),(2,range(114,114),3)],[1],[3]),
  %EDom2 = automaton_dom([1,2,3],[(1,range(117,117),2),(2,range(101,101),3)],[1],[3]),
  constant_string_domain("tr",EDom1),
  constant_string_domain("ue",EDom2),
  union(EDom1,EDom2,Edom3),
  find_chr_constraint(str_in(Z,Edom3)).

test(str_union_compare_automaton,[true,nondet]):-
  EDom1 = automaton_dom([1,2,3],[(1,range(116,116),2),(2,range(114,114),3)],[1],[3]),
  EDom2 = automaton_dom([1,2,3],[(1,range(117,117),2),(2,range(101,101),3)],[1],[3]),
  str_in(X,EDom1),
  str_in(Y,EDom2),
  str_union(X,Y,Z),
  union(EDom1,EDom2,Edom3),
  find_chr_constraint(str_in(Z,Edom3)).

test(simple_str_intersection_no_res,[fail]):-
  generate_domain("tr",Dom1),
  str_in(X,Dom1),
  generate_domain("ue",Dom2),
  str_in(Y,Dom2),
  str_intersection(X,Y,_).

test(simple_str_intersection_no_res,[true]):-
  generate_domain("true",Dom1),
  str_in(X,Dom1),
  generate_domain("true",Dom2),
  str_in(Y,Dom2),
  str_intersection(X,Y,_).

test(simple_str_intersection_no_res,[true]):-
  generate_domain("true",Dom1),
  str_in(X,Dom1),
  generate_domain("....",Dom2),
  str_in(Y,Dom2),
  str_intersection(X,Y,_).

test(str_intersection_compare_string,[true,nondet]):-
  generate_domain("true",Dom1),
  str_in(X,Dom1),
  generate_domain("....",Dom2),
  str_in(Y,Dom2),
  str_intersection(X,Y,Z),
  % in this case concat will accept strings, because of reg_ex_parser
  %EDom1 = automaton_dom([1,2,3,4,5],[(1,range(116,116),2),(2,range(114,114),3),(3,range(117,117),4),(4,range(101,101),5)],[1],[5]),
  EDom2 = automaton_dom([1,2,3,4,5],[(1,range(32,126),2),(2,range(32,126),3),(3,range(32,126),4),(4,range(32,126),5)],[1],[5]),
  constant_string_domain("true",EDom1),
  intersection(EDom1,EDom2,Edom3),
  find_chr_constraint(str_in(Z,Edom3)).

test(str_intersection_compare_automaton,[true,nondet]):-
  EDom1 = automaton_dom([1,2,3,4,5],[(1,range(116,116),2),(2,range(114,114),3),(3,range(117,117),4),(4,range(101,101),5)],[1],[5]),
  EDom2 = automaton_dom([1,2,3,4,5],[(1,range(32,126),2),(2,range(32,126),3),(3,range(32,126),4),(4,range(32,126),5)],[1],[5]),
  str_in(X,EDom1),
  str_in(Y,EDom2),
  str_intersection(X,Y,Dom3),
  intersection(EDom1,EDom2,Edom3),
  find_chr_constraint(str_in(Dom3,Edom3)).

:- end_tests(operational_constraints).


:- begin_tests(prefix).

test(compare_prefix1,[true,nondet]):-
  generate_domain("true",Dom1),
  str_prefix(X,Dom1),
  str_prefix(X,"true").

test(compare_prefix2,[true,nondet]):-
  generate_domain("true",Dom1),
  str_prefix(X,Dom1),
  str_prefix(X,"true"),
  generate_domain("true.*",Dom2),
  str_in(X,Dom2).

test(compare_prefix3,[true,nondet]):-
  generate_domain("true",Dom1),
  str_prefix(X,Dom1),
  str_prefix(X,"true"),
  generate_domain("....",Dom2),
  str_in(Y,Dom2),
  str_intersection(X,Y,Z),
  str_in(Z,"true").

:- end_tests(prefix).


:- begin_tests(suffix).

test(compare_suffix1,[true,nondet]):-
  generate_domain("true",Dom1),
  str_suffix(X,Dom1),
  str_suffix(X,"true").

test(compare_suffix2,[true,nondet]):-
  generate_domain("true",Dom1),
  str_suffix(X,Dom1),
  str_suffix(X,"true"),
  generate_domain(".*true",Dom2),
  str_suffix(X,Dom2).

test(compare_suffix3,[true,nondet]):-
  generate_domain("true",Dom1),
  str_suffix(X,Dom1),
  str_suffix(X,"true"),
  generate_domain("....",Dom2),
  str_in(Y,Dom2),
  str_intersection(X,Y,Z),
  str_in(Z,"true").

:- end_tests(suffix).


:- begin_tests(infix).

test(compare_infix1,[true,nondet]):-
  generate_domain("true",Dom1),
  str_infix(X,Dom1),
  str_infix(X,"true").

test(compare_infix2,[true,nondet]):-
  generate_domain("true",Dom1),
  str_infix(X,Dom1),
  str_infix(X,"true"),
  generate_domain(".*true.*",Dom2),
  %nl,nl,print(Dom2),nl,nl,
  str_in(X,Dom2).

test(compare_infix3,[true,nondet]):-
  generate_domain("true",Dom1),
  str_infix(X,Dom1),
  str_infix(X,"true"),
  generate_domain("....",Dom2),
  str_in(Y,Dom2),
  str_intersection(X,Y,Z),
  str_in(Z,"true").

:- end_tests(infix).


:- begin_tests(case_sensitive).

test(simple_lower_case,[true]) :-
  generate_domain("true",Dom1),
  str_infix(X,Dom1),
  str_lower_case(X).

test(simple_upper_case,[true]) :-
  generate_domain("TRUE",Dom1),
  str_infix(X,Dom1),
  str_upper_case(X).

test(intersec_lower_case,[true]) :-
  generate_domain("....",Dom1),
  str_in(X1,Dom1),
  str_lower_case(X1),
  generate_domain("true",Dom2),
  str_in(X2,Dom2),
  str_lower_case(X2),
  str_intersection(X1,X2,_).

test(intersec_upper_case,[true]) :-
  generate_domain("....",Dom1),
  str_in(X1,Dom1),
  str_upper_case(X1),
  generate_domain("TRUE",Dom2),
  str_in(X2,Dom2),
  str_upper_case(X2),
  str_intersection(X1,X2,_).

test(label_lower_case,[true(X=="true"),nondet]) :-
  generate_domain("true|TRUE",Dom1),
  str_in(X,Dom1),
  str_lower_case(X),
  %find_chr_constraint(str_in(X,Dom2)),
  %nl,nl,print(Dom2),nl,nl,
  str_label([X]).

test(label_upper_case,[true(X=="TRUE"),nondet]) :-
  generate_domain("true|TRUE",Dom1),
  str_in(X,Dom1),
  str_upper_case(X),
  %find_chr_constraint(str_in(X,Dom2)),
  %nl,nl,print(Dom2),nl,nl,
  str_label([X]).

test(lower_case_fail,[fail]) :-
  generate_domain("TRUE",Dom1),
  str_in(X,Dom1),
  str_lower_case(X).

test(upper_case_fail,[fail]) :-
  generate_domain("true",Dom1),
  str_in(X,Dom1),
  str_upper_case(X).

test(lower_case_fail,[fail]) :-
  generate_domain("TRUE",Dom1),
  str_in(X,Dom1),
  str_lower_case(X).

test(case_sensitive_fail,[fail]) :-
  generate_domain("....",Dom1),
  str_in(X,Dom1),
  str_upper_case(X),
  str_lower_case(X).

:- end_tests(case_sensitive).

equal_string_int_tuple((S,I)) :-
  number_string(I, IStr),
  S == IStr.

:- begin_tests(str_to_int).

% string and fd var should always label the same value
test(str_to_int_equal_string_int1) :-
  str_in(S, "0|-?[1-9]*"),
  I in (-100)..(-1) ,
  str_to_int(S, I),
  findall((S,I), str_labeling([dfs], [I, S]), Tuples),
  maplist(equal_string_int_tuple, Tuples).

test(str_to_int_equal_string_int2) :-
  str_in(S, "0|[1-9]*"),
  I in 0..100 ,
  str_to_int(S, I),
  findall((S,I), str_labeling([dfs], [I, S]), Tuples),
  maplist(equal_string_int_tuple, Tuples).

% TODO: more tests for str_to_int/2

:- end_tests(str_to_int).

:- begin_tests(operators).

test(match, [all(X == ["a", "b"])]) :-
  X match "a|b",
  str_label([X]).

test(implicit_intersection, [true(X == "b")]) :-
  X match "a|b",
  X match "b|y",
  str_label([X]).

test(explicit_intersection, [true(X == "b")]) :-
  X match "a|b" /\ "b|y",
  str_label([X]).

test(union, [all(X == ["a", "b", "c"])]) :-
  X match "a|b" \/ "c",
  str_label([X]).

test(concatenation, [all(X == ["abc"])]) :-
  X match "a" + "b" + "c",
  str_label([X]).

test(combining_ops, [all(X == ["ab"])]) :-
  X match "a" \/ "a" + "b" /\ "ab",
  str_label([X]).

test(nesting, [all(X == ["ab", "ac", "bb", "bc"])]) :-
  X match ("a" \/ "b") + ("b" \/ "c"),
  str_label([X]).

:- end_tests(operators).
