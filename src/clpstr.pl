 :- module(clpstr,[str_in/2,
                   str_size/2,
                   str_labeling/2,
                   str_label/1,
                   str_concatenation/3,
                   str_repeat/2,
                   str_repeat/3,
                   str_repeat/4,
                   str_union/3,
                   str_intersection/3,
                   str_prefix/2,
                   str_suffix/2,
                   str_infix/2,
                   generate_domain/2]).

:- use_module(library(chr)).

:- use_module('domains/basic_domains').
:- use_module('domains/basic_operations').
:- use_module('domains/labeling').
:- use_module('reg_ex_parser').

:- chr_constraint str_in/2, str_labeling/2, str_label/1, str_size/2,
   str_concatenation/3, str_repeat/2, str_repeat/3, str_repeat/4,
   str_union/3, str_intersection/3, str_prefix/2, str_suffix/2, str_infix/2.


% chr rule for generating a str_in directly from a String.
% S should be bound to a str.
str_in(X,S) <=> string(S) | generate_domain(S,D), str_in(X,D).


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

% the variables in the list Vars are supposed to be labeled.
% the rule iterates over all the domains, picking each domain str_in,
% that is associated which a variable in the list
% (we do not use member to check since that would unify variables!)
% in case the string variable Var is indeed supposed to be labeled,
% we call the domain operation for labeling.
str_labeling(Options,Vars) \ str_in(Var,Dom)
            <=> is_list(Options), var_is_member(Var,Vars) | labeling(Options,Dom,Var).

str_label(Vars) <=> str_labeling([],Vars).

% just like member, but using variable identity check (==)
% rather than unification (=)
var_is_member(X,[C|_]) :- X == C, !.
var_is_member(X,[_|T]) :- var_is_member(X,T).


% If you already know Domain, set it up to be in the intersection of the two
% domains. If not set it up to be as a new domain.
% Dismiss old domain and constraint.
/*str_size(X,I), str_in(X,D1)
            <=> integer(I) | generate_any_size(D2,I),
                  intersection(D1,D2,D3), stri_in(X,D3).*/
% Note the first clause is already covered via str_in
str_size(X,I) <=> integer(I) | generate_any_size(I,D), str_in(X,D).


% Take 3 variables and calc the conatenation of the three.
% Dismiss the constraint and keep the str_in of the other var.
str_in(X1,D1), str_in(X2,D2) \ str_concatenation(X1,X2,X3)
            <=> concatenation(D1,D2,D3), str_in(X3,D3).


% Take the variable  and repeat it the specific amount of times.
% Dismiss old domain and constraint and put it into a new varable.
str_repeat(X1,X2), str_in(X1,D1)
            <=> repeat(D1,D2), str_in(X2,D2).
str_repeat(X1,Nmb,X2), str_in(X1,D1)
            <=> integer(Nmb)| repeat(D1,Nmb,D2), str_in(X2,D2).
str_repeat(X1,From,To,X2), str_in(X1,D1)
            <=> integer(From),integer(To) | repeat(D1,From,To,D2), str_in(X2,D2).


% Take 3 variables and calc the union of the three.
% Dismiss the constraint and keep the str_in of the other var.
str_in(X1,D1), str_in(X2,D2) \ str_union(X1,X2,X3)
            <=> union(D1,D2,D3), str_in(X3,D3).


% Take 3 variables and calc the intersection of the three.
% Dismiss the constraint and keep the str_in of the other var.
str_in(X1,D1), str_in(X2,D2) \ str_intersection(X1,X2,X3) 
            <=> intersection(D1,D2,D3), str_in(X3,D3).



str_prefix(X,String) <=>
            string(String) | generate_domain(String,Dom1), any_char_domain(Dom2),
            repeat(Dom2,Dom3), concatenation(Dom1,Dom3,ResDom), str_in(ResDom,X).
str_prefix(X,Dom1) <=>
            any_char_domain(Dom2), repeat(Dom2,Dom3),
            concatenation(Dom1,Dom3,ResDom), str_in(ResDom,X).


str_suffix(X,String) <=>
            string(String) | generate_domain(String,Dom1), any_char_domain(Dom2),
            repeat(Dom2,Dom3), concatenation(Dom3,Dom1,ResDom), str_in(ResDom,X).
str_suffix(X,Dom1) <=>
            any_char_domain(Dom2), repeat(Dom2,Dom3),
            concatenation(Dom3,Dom1,ResDom), str_in(ResDom,X).


str_infix(X,String) <=>
            string(String) | generate_domain(String,Dom1), any_char_domain(Dom2),
            repeat(Dom2,Dom3), concatenation(Dom1,Dom3,Dom4),
            concatenation(Dom3,Dom4,ResDom), str_in(ResDom,X).
str_infix(X,Dom1) <=>
            any_char_domain(Dom2), repeat(Dom2,Dom3),
            concatenation(Dom1,Dom3,Dom4), concatenation(Dom3,Dom4,ResDom),
            str_in(ResDom,X).


generate_domain(String,Dom) :-
  string(String),
  atom_codes(String,Codes),
  generate(Codes, Dom).
