:- use_module(library(plunit)).

:- use_module('../src/clpstr').
:- use_module('../src/domains/basic_domains').
:- use_module('../src/domains/basic_operations').

:- begin_tests(str_in).

test(simple_in_without_labeling,[nondet]) :-
    constant_string_domain("abc",D),
    str_in(_X,D).
test(simple_in_collision,[fail]) :-
    constant_string_domain("abc",D1),
    str_in(X,D1),
    constant_string_domain("def",D2),
    str_in(X,D2).
test(str_in_twice_fail,[fail]) :-
    any_char_domain(A),
    repeat(A,2,TenTimesAny),
    str_in(X,TenTimesAny),
    repeat(A,3,FiveTimesAny),
    str_in(X,FiveTimesAny).
test(str_in_twice_succeed,[nondet]) :-
    any_char_domain(A),
    repeat(A,5,10,FiveToTenTimesAny),
    str_in(X,FiveToTenTimesAny),
    repeat(A,5,FiveTimesAny),
    str_in(X,FiveTimesAny).

:- end_tests(str_in).
