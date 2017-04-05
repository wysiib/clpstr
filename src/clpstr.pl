 :- module(clpstr,[str_in/2]).

:- use_module(library(chr)).

:- use_module('domains/basic_operations').
:- use_module(labeling).

:- chr_constraint str_in/2, str_labeling/1.

str_in(_,D) ==>  is_empty(D) | fail.

str_in(X,D1), str_in(X,D2)
            <=> intersection(D1,D2,D3), str_in(X,D3).

str_labeling(Vars) \ str_in(Var,Dom)
            <=> var_is_member(Var,Vars) | label(Dom,Var).

var_is_member(X,[C|_]) :- X == C, !.
var_is_member(X,[_|T]) :- var_is_member(X,T).
