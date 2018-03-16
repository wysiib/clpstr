:- module(reg_ex_parser, [parse/2,
                          parse_2_tree/2]).

% ( | * + )
% (a_b)* + (_)

% characters
characters(id(I)) --> char(D0), characters(D), {atom_codes(I, [D0|D])}.
%characters([any|T]) --> `.`, !, characters(T).
characters([D|T]) --> char(D), !, characters(T).
characters([]) --> [].
char(D) --> [D], {code_type(D, alpha)}.


% white space
ws --> [D], {code_type(D, white), !}, ws.
ws --> ``.


% regular expressions
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
