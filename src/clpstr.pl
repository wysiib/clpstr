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
                   str_prefix/2,
                   str_suffix/2,
                   str_infix/2,
                   str_upper_case/1,
                   str_lower_case/1,
                   generate_domain/2,
                   str_to_bool/2,
                   match/2,
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
   str_upper_case/1, str_lower_case/1, str_to_int/2, str_to_int2/2, str_max_size/2,
   str_to_bool/2.

clpstr_var(X) :- get_attr(X, clpstr, _).

% Convenience predicate for defining domains; API similar to CLP(FD)
match(X, D) :- clpstr_var(D), !, X=D.
match(_, D) :- var(D), !. % Yields instantiation error when labelling
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


% chr rule wakes up each time a new or updated str_in is added
% in case the domain is empty, no sulution is possible anymore:
% we have a variable (_) which should be assigned a value,
% but no value is available for said variable.
% In consequence, we fail and backtrack.
str_in(_,D) ==>  is_empty(D) | fail.
% in case the domain became constant, we propagate to the variable
str_in(Var,Domain) ==> Domain = string_dom(Const) | Var = Const.

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
              str_labeling(Options, RestVars). % TODO: filter clpfd options
% the variables in the list Vars are supposed to be labeled.
% the rule iterates over all the domains, picking each domain str_in,
% that is associated with a variable in the list
% (we do not use member to check since that would unify variables!)
% in case the string variable Var is indeed supposed to be labeled,
% we call the domain operation for labeling.
str_labeling(Options,Vars)\ str_in(Var,Dom)
            <=> var(Var), Dom \= string_dom(_), is_list(Options), var_is_member(Var,Vars)
            | labeling(Options,Dom,Label),
              Var = Label,
              constant_string_domain(Label,CstDom),
              str_in(Var,CstDom).

% no labeling if string var has a singleton domain
str_labeling(_, Vars) , str_in(Var, Dom)
            ==> var(Var), Dom = string_dom(Label), var_is_member(Var, Vars)
            | Var = Label.

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
str_size(X, I) <=> integer(I) | generate_any_size(I, D), str_in(X, D).

str_max_size(X, I) <=> integer(I) | generate_any_up_to_size(I, D), str_in(X, D).

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



str_prefix(X,String) <=>
            string(String) | generate_domain(String,Dom), str_prefix(X,Dom).
str_prefix(X,Dom1) <=>
            any_char_domain(Dom2), repeat(Dom2,Dom3),
            concatenation(Dom1,Dom3,ResDom), str_in(X,ResDom).

str_suffix(X,String) <=>
            string(String) | generate_domain(String,Dom), str_suffix(X,Dom).
str_suffix(X,Dom1) <=>
            any_char_domain(Dom2), repeat(Dom2,Dom3),
            concatenation(Dom3,Dom1,ResDom), str_in(X,ResDom).

str_infix(X,String) <=>
            string(String) | generate_domain(String,Dom), str_infix(X,Dom).
str_infix(X,Dom1) <=>
            any_char_domain(Dom2), repeat(Dom2,Dom3),
            concatenation(Dom1,Dom3,Dom4), concatenation(Dom3,Dom4,ResDom),
            str_in(X,ResDom).


str_upper_case(X) <=> upper_case_domain(Dom1), repeat(Dom1,Dom2), str_in(X,Dom2).


str_lower_case(X) <=> lower_case_domain(Dom1), repeat(Dom1,Dom2), str_in(X,Dom2).

generate_domain("", Dom) :-
  !,
  constant_string_domain("", Dom).
generate_domain(String, Dom) :-
  string(String),
  atom_codes(String, Codes),
  generate(Codes, Dom),
  !.

% Note:
%   str_in(X, "[0-9][0-9]"), Y in 0..20, str_to_int(X,Y),str_label([X]).
%   First solution is X = "10" which, of course, is correct.
%   However, we could adapt the implementation to also provide X = "00", X = "01", etc.
%   Not sure if this is useful or disagrees with any conventions.
%% String to integer conversion integrating CLP(FD) to the solver.
%
% detect failure early without computing the intersection of domains
str_to_int2(S,I) ==>
  string(S), integer(I), number_string(I, IString), S \== IString | fail.

% fd var is constant
str_to_int2(S,I) ==>
  integer(I), number_string(I, IString) | constant_string_domain(IString, IDom), str_in(S, IDom).

% string is constant
str_to_int2(S,I), str_in(S,D) ==>
  D = string_dom(CstString), number_string(CstInteger, CstString) | I #= CstInteger.

% fail for non numeric string to integer
str_to_int2(S,_), str_in(S,D) ==>
  D = string_dom(CstString), \+ number_string(_, CstString) | fail.

% terminate exhaustive search if string domain is regex with Kleene star
% only called once so int domain has to be finite beforehand, not terminating:
% str_in(X,"[0-9]*"),str_to_int(X,I),I in 0..2,findall(X,str_label([X]),L).
% would be too much overhead to trigger this on each domain clpfd reduction
str_to_int(S, I) ==>
  finite_domain_len(I, MaxStrSize) |
  str_max_size(S, MaxStrSize),
  str_to_int2(S, I).

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

% Note: We could provide a direct conversion from bool to integer, too. Currently, one can use 'str_to_bool(X,B), str_to_int(X,I)'.
%       Therefore, I think this is not necessary.
%% String to bool conversion integrating CLP(B) to the solver.
str_to_bool(S,B) ==>
  string(S), ground(B), bool_to_string(B, BString) , S \== BString | fail.

% bool var is constant
str_to_bool(S,B) ==>
  ground(B), bool_to_string(B, BString) | constant_string_domain(BString, IDom) , str_in(S, IDom).

% string is constant
str_to_bool(S,B), str_in(S,D) ==>
  D = string_dom(CstString), bool_to_string(Bool, CstString) | B = Bool.

% fail for non boolean string to bool
str_to_bool(S,B), str_in(S,D) ==>
  D = string_dom(_), (\+ ground(B); \+ bool(B)) | fail.

bool(1).
bool(0).

bool_to_string(1, "1").
bool_to_string(0, "0").
%%
