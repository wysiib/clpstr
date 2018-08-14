 :- module(clpstr,[str_in/2]).

:- use_module(library(chr)).

:- use_module('domains/basic_operations').
:- use_module('domains/labeling').

:- chr_constraint str_in/2, str_labeling/1.

% chr rule wakes up each time a new or updated str_in is added
% in case the domain is empty, no sulution is possible anymore:
% we have a variable (_) which should be assigned a value,
% but no value is available for said variable.
% In consequence, we fail and backtrack.
str_in(_,D) ==>  is_empty(D) | fail.

% two domains are available for the same string variable X
% this might happen when an updated domain is posted to CHR
% we compute the intersection, i.e. the domain containing all strings
% that are in both domains
% the two old str_in constraints are removed, a single update one is added
% as a result, the string variable X has again an unambiguous domain
str_in(X,D1), str_in(X,D2)
            <=> intersection(D1,D2,D3), str_in(X,D3).

% the variables in the list Vars are suppoed to be labeled.
% the rule iterates over all the domains, picking each domain str_in,
% that is associated which a variable in the list
% (we do not use member to check since that would unify variables!)
% in case the string variable Var is indeed supposed to be labeled,
% we call the domain operation for labeling.
str_labeling(Vars) \ str_in(Var,Dom)
            <=> var_is_member(Var,Vars) | label(Dom,Var).

% just like member, but using variable identity check (==)
% rather than unification (=)
var_is_member(X,[C|_]) :- X == C, !.
var_is_member(X,[_|T]) :- var_is_member(X,T).
