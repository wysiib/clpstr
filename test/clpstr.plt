:- use_module(library(plunit)).

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

test(concat_no_res,[true]):-
  generate_domain("tr",Dom1),
  str_in(X,Dom1),
  generate_domain("ue",Dom2),
  str_in(Y,Dom2),
  str_concatenation(X,Y,_).

/*test(concat_no_res,[true]):-
  generate_domain("tr",Dom),
  str_in(X,Dom),
  generate_domain("ue",Dom),
  str_in(Y,Dom),
  str_concatenation(X,Y,Z).*/


:- end_tests(operational_constraints).
