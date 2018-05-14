:- module(reg_ex_parser, [generate/2,
                          parse_2_tree/2]).

:- use_module('domains/basic_domains').
:- use_module('domains/basic_operations').

/* ----- Generating an AST from a given regular expression  ----- */

% ( | * + )
% (a_b)* + (_)

% characters
characters(string(I)) --> characters2(D), {atom_codes(I, D)}.
%characters([any|T]) --> `.`, !, characters(T).
characters2([D|T]) --> char(D), !, characters2(T).
characters2([]) --> [].
char(D) --> [D], {code_type(D, alpha), (D>=65, D=<90); (D>=97, D=<122)}.


% white space
ws --> [D], {code_type(D, white), !}, ws.
ws --> ``.


% regular expressions
expression0([exp(X)]) --> expression(X).
expression0([exp(H)|T]) --> expression(H), expression0(T).


expression(set('|',X,Y)) --> expression2(X), ws, `|`, ws, expression(Y).
expression(X) --> expression2(X).

expression2(quantity(*,X)) --> expression3(X), ws, `*`, !.
expression2(quantity(+,X)) --> expression3(X), ws, `+`, !.
expression2(quantity(?,X)) --> expression3(X), ws, `?`, !.
expression2(X) --> expression3(X).

expression3(X) --> `(`, ws, expression(X), ws, `)`, !.
expression3(X) --> characters(X).

/* ----- Generating the Constraint system from the AST  ----- */

parse_2_tree(RegEx,Tree) :-
  expression0(Tree,RegEx,[]).


generate(RegEx,ResDom) :-
  parse_2_tree(RegEx,Tree),
  build_meta(Tree,ResDom).


build_meta([],ResDom) :-
  constant_string_domain("",ResDom).
build_meta([exp(H)|T],ResDom) :-
  build(H,TempDom1),
  build_meta(T,TempDom2),
  concatenation(TempDom1,TempDom2,ResDom).


build(string(X),ResDom) :-
  constant_string_domain(X,ResDom).
/*build(set('|',X),ResDom) :-
  is_list(X), TODO
  */
build(set('|',X,Y),ResDom) :-
  build(X,TempDom1),
  build(Y,TempDom2),
  union(TempDom1,TempDom2,ResDom).
build(quantity(*,X),ResDom) :-
  build(X,TempDom),
  repeat(TempDom,ResDom).
build(quantity(+,X),ResDom) :-
  build(X,TempDom),
  repeat(TempDom,RepeatDom),
  concatenation(TempDom,RepeatDom,ResDom).
build(quantity(?,X),ResDom) :-
  build(X,TempDom),
  repeat(TempDom,0,1,ResDom).
