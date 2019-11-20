 :- module(clpstr,[str_in/2,
                   str_size/2,
                   str_max_size/2,
                   str_labeling/2,
                   str_label/1,
                   str_concatenation/3,
                   str_repeat/2,
                   str_repeat/3,
                   str_repeat/4,
                   str_union/3,
                   str_intersection/3,
                   str_to_int/2,
                   str_to_bool/2,
                   str_to_real/2,
                   str_to_intl/2,
                   str_to_booll/2,
                   str_to_reall/2,
                   str_prefix/2,
                   str_suffix/2,
                   str_infix/2,
                   str_upper_case/1,
                   str_lower_case/1,
                   generate_domain/2,
                   str_to_bool/2,
                   str_diff/2,
                   str_all_diff/1,
                   match/2,
                   escape_special_characters/2,
                   remove_escape_special_characters/2,
                   op(700, xfx, match),
                   op(700, xfx, str_in)
                   ]).

:- use_module(library(chr)).
:- use_module(library(clpfd)).

:- use_module('domains/basic_domains').
:- use_module('domains/basic_operations').
:- use_module('domains/reductions').
:- use_module('domains/labeling').
:- use_module('reg_ex_parser').

:- chr_constraint str_in/2, str_labeling/2, str_label/1, str_size/2,
   str_concatenation/3, str_repeat/2, str_repeat/3, str_repeat/4,
   str_union/3, str_intersection/3, str_prefix/2, str_suffix/2, str_infix/2,
   str_upper_case/1, str_lower_case/1, str_max_size/2,
   str_to_int/3, str_to_bool/3, str_to_real/3,
   str_to_int/2, str_to_bool/2, str_to_real/2,
   str_to_intl/2, str_to_booll/2, str_to_reall/2,
   str_diff/2, str_all_diff/1.

clpstr_var(X) :- get_attr(X, clpstr, _).

pairwise_different([]).
pairwise_different([H|T]) :-
  pairwise_different(H, T),
  pairwise_different(T).

pairwise_different(_, []).
pairwise_different(Current, [H|T]) :-
  str_diff(Current, H),
  pairwise_different(Current, T).

% Convenience predicate for defining domains; API similar to CLP(FD)
match(X, D) :- clpstr_var(D), !, X=D.
match(X, D) :- var(D), !, str_in(X, ".*"). % create new string var
match(X, A + B) :- !,
  match(Y, A),
  match(Z, B),
  str_concatenation(Y, Z, X).
match(X, A /\ B) :- !,
  match(Y, A),
  match(Z, B),
  str_intersection(Y, Z, X).
match(X, A \/ B) :- !,
  match(Y, A),
  match(Z, B),
  str_union(Y, Z, X).
match(X, Y) :- str_in(X, Y).

% chr rule for generating a str_in directly from a String.
% S should be bound to a str.
str_in(X,S) <=> string(S) | generate_domain(S,D), str_in(X,D).

% string different: we have to ensure that this propagates during labeling
% as soon as both, S1 and S2, are constant values (better performance)
str_in(S1,D1), str_in(S2,D2), str_diff(S1,S2) ==>
  D1 = string_dom(Cst1), string(Cst1),
  D2 = string_dom(Cst2), string(Cst2) |
  Cst1 \== Cst2.

% string all different
str_all_diff(ListOfCnstrnts) <=> is_list(ListOfCnstrnts) | pairwise_different(ListOfCnstrnts).

% chr rule wakes up each time a new or updated str_in is added
% in case the domain is empty, no sulution is possible anymore:
% we have a variable (_) which should be assigned a value,
% but no value is available for said variable.
% In consequence, we fail and backtrack.
str_in(_,D) ==>  is_empty(D) | fail.
% in case the domain became constant, we propagate to the variable
str_in(Var,Domain) ==> Domain = string_dom(Const) | escape_special_characters(Const, Var).

% two domains are available for the same string variable X
% this might happen when an updated domain is posted to CHR
% we compute the intersection, i.e. the domain containing all strings
% that are in both domains
% the two old str_in constraints are removed, a single update one is added
% as a result, the string variable X has again an unambiguous domain
str_in(X,D1), str_in(X,D2)
            <=> D1 \= D2 | intersection(D1,D2,D3), str_in(X,D3). % see comment below
str_in(X,D1) \ str_in(X,D2)
            <=>  D1 == D2 | true. % sebastians idea 9.11.18: only propagate if change expected, otherwise just drop


str_labeling(Options, Vars)
            <=> is_list(Options), select(Var, Vars, RestVars), fd_var(Var)
            | clpfd:labeling([], [Var]) ,
              str_labeling(Options, RestVars). % TO DO: filter clpfd options
% the variables in the list Vars are supposed to be labeled.
% the rule iterates over all the domains, picking each domain str_in,
% that is associated with a variable in the list
% (we do not use member to check since that would unify variables!)
% in case the string variable Var is indeed supposed to be labeled,
% we call the domain operation for labeling.
str_labeling(Options,Vars), str_in(Var,Dom)
            <=> var(Var), Dom \= string_dom(_), is_list(Options), var_is_member(Var,Vars)
            | labeling(Options,Dom,Label),
              escape_special_characters(Label, Var),
              constant_string_domain(Label,CstDom),
              str_in(Var,CstDom),
              str_labeling(Options,Vars).

% no labeling if string var has a singleton domain
str_labeling(_, Vars) , str_in(Var, Dom)
            ==> var(Var), Dom = string_dom(Label), var_is_member(Var, Vars)
            | escape_special_characters(Label, Var).

str_label(Vars) <=> str_labeling([],Vars).

% just like member, but using exact equality check (==)
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
str_size(X, I) <=> integer(I) | generate_any_size(I, D), str_in(X, D).

str_max_size(X, I) <=> integer(I) | generate_any_up_to_size(I, D), str_in(X, D).

str_concatenation(String, Empty, String) ==>
  str_in(Empty, "").
str_concatenation(Empty, String, String) ==>
  str_in(Empty, "").
% propagate constant values backwards if X3 is constant
str_in(X1,D1), str_in(X2,D2), str_in(X3,D3), str_concatenation(X1,X2,X3)
            ==> D3 = string_dom(CstStr)|
                string_concat(X1Str, X2Str, CstStr),
                label(D1, X1Str),
                label(D2, X2Str),
                str_in(X1, string_dom(X1Str)),
                str_in(X2, string_dom(X2Str)).
str_in(X1,D1), str_in(X2,D2), str_concatenation(X1,X2,X3)
            ==> concatenation(D1,D2,D3),
                str_in(X3,D3).
str_in(X1,D1), str_concatenation(X1,X1,X3)
            ==> concatenation(D1,D1,D3), str_in(X3,D3).

% Take the variable and repeat it the specific amount of times.
% Dismiss the constraint and put result into a new varable.
str_in(X1,D1), str_repeat(X1,X2)
            ==> repeat(D1,D2), str_in(X2,D2).
str_in(X1,D1), str_repeat(X1,Nmb,X2) % see comment below
            ==> integer(Nmb), var(X2) | repeat(D1,Nmb,D2), str_in(X2,D2).
str_in(X1,D1), str_repeat(X1,From,To,X2) % see comment below
            ==> integer(From),integer(To),var(X2) | repeat(D1,From,To,D2), str_in(X2,D2).
            % NOTE Malte's idea 10.11.18:
            %  added var(X2) to make sure repeat does not get
            % calculated after labeling. not sure whether this is smart.
            % theoretically X1 should not change after X2 is labeled and should
            % always be labeled before X2. also done in union.


% Take 3 variables and calc the union of the three.
% Dismiss the constraint and keep the str_in of the other var.
str_in(X1,D1), str_in(X2,D2), str_union(X1,X2,X3)
            ==> var(X3) | union(D1,D2,D3), str_in(X3,D3).
str_in(X1,_) \ str_union(X1,X1,X3) <=> X1 = X3.


% Take 3 variables and calc the intersection of the three.
% Dismiss the constraint and keep the str_in of the other var.
str_in(X1,D1), str_in(X2,D2), str_intersection(X1,X2,X3)
            ==> intersection(D1,D2,D3), str_in(X3,D3).
str_in(X1,_) \ str_intersection(X1,X1,X3) <=> X1 = X3.

% TO DO: unit tests for prefix, suffix and infix
str_prefix(X,String) <=>
            string(String) | generate_domain(String,Dom), str_prefix(X,Dom).
str_in(S,Dom1) \ str_prefix(X,S) <=>
            any_char_domain(Dom2), repeat(Dom2,Dom3),
            concatenation(Dom1,Dom3,ResDom), str_in(X,ResDom),
            str_in(Any, Dom3), str_concatenation(S, Any, X).

str_suffix(X,String) <=>
            string(String) | generate_domain(String,Dom), str_suffix(X,Dom).
str_in(S,Dom1) \ str_suffix(X,S) <=>
            any_char_domain(Dom2), repeat(Dom2,Dom3),
            concatenation(Dom3,Dom1,ResDom), str_in(X,ResDom),
            str_in(Any, Dom3), str_concatenation(Any, S, X).

str_infix(X, String) <=>
            string(String) | generate_domain(String, Dom), str_infix(X, Dom).
str_in(S,Dom1) \ str_infix(X,S) <=>
            any_char_domain(Dom2), repeat(Dom2, Dom3),
            concatenation(Dom1, Dom3, Dom4), concatenation(Dom3, Dom4, ResDom),
            str_in(X,ResDom), str_in(Any1, Dom3), str_in(Any2, Dom3),
            str_concatenation(S, Any1, Suffix), str_concatenation(Any2, Suffix, X).


str_upper_case(X) <=> upper_case_domain(Dom1), repeat(Dom1, Dom2), str_in(X, Dom2).


str_lower_case(X) <=> lower_case_domain(Dom1), repeat(Dom1, Dom2), str_in(X, Dom2).

generate_domain("", Dom) :-
  !,
  constant_string_domain("", Dom).
generate_domain(String, Dom) :-
  string(String),
  atom_codes(String, Codes),
  generate(Codes, Dom),
  !.

%% Wrapper for backend integration which is exposed to the user
%  either allowing leading zeros or converting exact values only.
str_to_int(S, I) <=>
  % terminate exhaustive search if string domain is regex with Kleene star
  % only called once so int domain has to be finite beforehand, not terminating:
  % str_in(X,"[0-9]*"),str_to_int(X,I),I in 0..2,findall(X,str_label([X]),L).
  % would be too much overhead to trigger this on each domain clpfd reduction
  finite_domain_len(I, MaxStrSize) |
  str_max_size(S, MaxStrSize),
  str_to_int(exact, S, I).

str_to_intl(S, I) <=>
  str_to_int(leading_zeros, S, I).

str_to_bool(S, I) <=>
  str_to_bool(exact, S, I).

str_to_booll(S, I) <=>
  str_to_bool(leading_zeros, S, I).

str_to_real(S, I) <=>
  str_to_real(exact, S, I).

str_to_reall(S, I) <=>
  str_to_real(leading_zeros, S, I).

%% String to integer conversion integrating CLP(FD) to the solver.
% detect failure early without computing the intersection of domains
str_to_int(_, S, I) ==>
  string(S), integer(I), number_string(SInt, S), I \== SInt | fail.

% fd var is constant
str_to_int(leading_zeros, S, I) ==>
  integer(I), number_string(I, IString) |
  constant_string_domain(IString, IDom),
  generate_domain("0*", ZDom),
  concatenation(ZDom, IDom, Dom),
  str_in(S, Dom).
str_to_int(exact, S, I) ==>
  integer(I), number_string(I, IString) |
  constant_string_domain(IString, IDom),
  str_in(S, IDom).

% string is constant
str_to_int(_, S, I), str_in(S, D) ==>
  D = string_dom(CstString), number_string(CstInteger, CstString) | I #= CstInteger.

% fail for non numeric string to integer
str_to_int(_, S, _), str_in(S, D) ==>
  D = string_dom(CstString), \+ number_string(_, CstString) | fail.

finite_domain_len(Var, MaxStrSize) :-
  fd_var(Var),
  fd_inf(Var, Infimum),
  fd_sup(Var, Supremum),
  string_size_fd_bound(Infimum, ISize),
  string_size_fd_bound(Supremum, SSize),
  max_string_size_fd_bound(ISize, SSize, MaxStrSize),
  MaxStrSize \== unbounded,!.
finite_domain_len(Cst, MaxStrSize) :-
  integer(Cst),
  number_string(Cst, CstStr),
  string_length(CstStr, MaxStrSize).

string_size_fd_bound(I, StringSize) :-
  integer(I),
  !,
  number_string(I, IString),
  string_length(IString, StringSize).
string_size_fd_bound(_, unbounded).

max_string_size_fd_bound(unbounded, _, unbounded) :-
  !.
max_string_size_fd_bound(_, unbounded, unbounded) :-
  !.
max_string_size_fd_bound(S1, S2, MaxStrSize) :-
  MaxStrSize is max(S1, S2).
%%

%% String to bool conversion allowing leading zeros.
str_to_bool(Type, S, B) ==>
  string(S), ground(B), \+bool_to_string(Type, B, S)| fail.

% bool var is constant
str_to_bool(leading_zeros, S, B) ==>
  ground(B), bool_to_string(leading_zeros, B, BString) |
  constant_string_domain(BString, IDom),
  generate_domain("0*", ZDom),
  concatenation(ZDom, IDom, Dom),
  str_in(S, Dom).
str_to_bool(exact, S, B) ==>
  ground(B), bool_to_string(exact, B, BString) |
  constant_string_domain(BString, BDom),
  str_in(S, BDom).

% string is constant
str_to_bool(Type, S, B), str_in(S, D) ==>
  D = string_dom(CstString), bool_to_string(Type, Bool, CstString) | B = Bool.

% fail for non boolean string to bool
str_to_bool(_, S, B), str_in(S, D) ==>
  D = string_dom(_), (\+ ground(B); \+ bool(B)) | fail.

bool(1).
bool(0).

bool_to_string(_, 1, "1") :-
  !.
bool_to_string(_, 0, "0") :-
  !.
bool_to_string(leading_zeros, Bool, S) :-
  % "00" = 0 etc.
  % Note: number_string(1, "01") holds
  (   Bool = 0
  ;   Bool = 1),
  number_string(Bool, S).
%%

%% String to real conversion allowing leading zeros.
str_to_real(_, S, R) ==>
  string(S), float(R), number_string(SReal, S) , R \== SReal | fail.

% real var is constant
str_to_real(leading_zeros, S, R) ==>
  float(R), number_string(R, RString) |
  constant_string_domain(RString, RDom),
  generate_domain("0*", ZDom),
  concatenation(ZDom, RDom, Dom),
  str_in(S, Dom).
str_to_real(exact, S, R) ==>
  float(R), number_string(R, RString) |
  constant_string_domain(RString, RDom),
  str_in(S, RDom).

% string is constant
str_to_real(_, S, R), str_in(S,D) ==>
  D = string_dom(CstString), number_string(Real, CstString) | R = Real.

% fail for non real string to real
str_to_real(_, S, R), str_in(S,D) ==>
  D = string_dom(_), \+ float(R) | fail.
%%

escape_special_characters(RgbCodeStr1, RgbCodeStr) :-
  re_replace("\\["/g, "\\[", RgbCodeStr1, RgbCodeStr2),
  re_replace("\\]"/g, "\\]", RgbCodeStr2, RgbCodeStr3),
  re_replace("\\{"/g, "\\{", RgbCodeStr3, RgbCodeStr4),
  re_replace("\\}"/g, "\\}", RgbCodeStr4, RgbCodeStr).

remove_escape_special_characters(RgbCodeStr1, RgbCodeStr) :-
  re_replace("\\\\\\["/g, "[", RgbCodeStr1, RgbCodeStr2),
  re_replace("\\\\\\]"/g, "]", RgbCodeStr2, RgbCodeStr3),
  re_replace("\\\\\\{"/g, "{", RgbCodeStr3, RgbCodeStr4),
  re_replace("\\\\\\}"/g, "}", RgbCodeStr4, RgbCodeStr).
