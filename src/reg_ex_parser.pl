:- module(reg_ex_parser, [generate/2,
                          parse_2_tree/2]).

:- use_module('domains/basic_domains').
:- use_module('domains/basic_operations').

/* ----- Generating an AST from a given regular expression  ----- */

% characters
%characters(string(I)) --> characters2(D), {atom_codes(I, D)}.
characters(char(I)) --> char_or_digit(D), !, {atom_codes(I, D)}.
characters(any) --> `.`.
characters(nonliteral(D)) --> nonlit(D), !.
%characters2([D|T]) --> char(D), !, characters2(T).
char_or_digit([D]) --> [D], {code_type(D, alnum)}.
% not only, (D>=65, D=<90); (D>=97, D=<122) alpha also includes special
% letters like ö,ü,a and so on.
% (D>=48, D=<57) for digits
nonlit(any) --> `.`.


% white space
ws --> [D], {code_type(D, white), !}, ws.
ws --> ``.


% regular expressions
%expression0([exp(X)]) --> expression(X).
expression(concat(X,Y)) --> expression2(X), ws, expression(Y), !.
expression(set(X,Y)) --> expression2(X), ws, `|`, !, ws, expression(Y).
expression(X) --> expression2(X).

expression2(quantity(*,X)) --> expression3(X), ws, `*`, !.
expression2(quantity(+,X)) --> expression3(X), ws, `+`, !.
expression2(quantity(?,X)) --> expression3(X), ws, `?`, !.
expression2(X) --> expression3(X).

expression3(X) --> ws, `(`, ws, expression(X), ws, `)`, ws, !.
expression3(X) --> characters(X), !.

/* ----- Generating the Constraint system from the AST  ----- */

parse_2_tree(RegEx,Tree) :-
  expression(Tree,RegEx,[]).


generate(RegEx,ResDom) :-
  parse_2_tree(RegEx,Tree),
  build_meta(Tree,ResDom).


build_meta([],ResDom) :-
  constant_string_domain("",ResDom).
build_meta([exp(H)|T],ResDom) :-
  build(H,TempDom1),
  build_meta(T,TempDom2),
  concatenation(TempDom1,TempDom2,ResDom).


build(char(X),ResDom) :-
  single_char_domain(X,ResDom).
build(any,ResDom) :-
  any_char_domain(ResDom).
/*build(set(X),ResDom) :-
  is_list(X), TODO
  */
build(concat(char(X),Y),ResDom) :-
  string_check(concat(char(X),Y),StringList),
  atomic_list_concat(StringList,String),
  % NOTE String is here an Atom not a string.
  % Not relevant now, but maybe in the future!
  constant_string_domain(String,ResDom).
build(concat(X,Y),ResDom) :-
  build(X,TempDom1),
  build(Y,TempDom2),
  concatenation(TempDom1,TempDom2,ResDom).
build(set(X,Y),ResDom) :-
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

string_check(concat(char(X),char(Y)),[X,Y]).
string_check(concat(char(X),Y),[X|T]) :-
  string_check(Y,T).
