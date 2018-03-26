:- module(reg_ex_parser, [parse/2,
                          parse_2_tree/2]).

:- use_module('domains/basic_domains').
:- use_module('domains/basic_operations').

% ( | * + )
% (a_b)* + (_)

% characters
characters(string(I)) --> char(D0), characters(D), {atom_codes(I, [D0|D])}.
%characters([any|T]) --> `.`, !, characters(T).
characters([D|T]) --> char(D), !, characters(T).
characters([]) --> [].
char(D) --> [D], {code_type(D, alpha)}.


% white space
ws --> [D], {code_type(D, white), !}, ws.
ws --> ``.


% regular expressions
expression0(Z) --> expression(X), expression(Y), {Z = (X,Y)}.
expression0(X) --> expression(X).

expression(set('|',X,Y)) --> expression2(X), ws, `|`, ws, expression(Y).
expression(X) --> expression2(X).

expression2(quantity(*,X)) --> expression3(X), ws, `*`.
expression2(quantity(+,X)) --> expression3(X), ws, `+`.
expression2(quantity(?,X)) --> expression3(X), ws, `?`.
expression2(X) --> expression3(X).

expression3(X) --> `(`, ws, expression(X), ws, `)`.
expression3(X) --> characters(X).


parse(_,_) :- fail.


parse_2_tree(RegEx,Tree) :-
  expression(Tree,RegEx,[]).


generate(RegEx,ResDom) :-
  parse_2_tree(RegEx,Tree),
  build(Tree,ResDom).


build(string(X),ResDom) :-
  constant_string_domain(X,ResDom).
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
