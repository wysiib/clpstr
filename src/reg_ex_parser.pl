:- module(reg_ex_parser, [generate/2,
                          parse_2_tree/2]).

:- use_module(library(dcg/basics), [number//1]).
:- use_module('domains/basic_domains').
:- use_module('domains/basic_operations').

/* ----- How to use the regular expression parser  ----- */
/*
   This prolog module is a parser for regular expressions, in
   the following refered to as regex. The module is seperated into two
   parts. First the parser and secondly the interpreter.
   You can specifically call the parser by using the parse_2_tree/2
   predicate. It will take a regex in backticks in its first argument
   and the resulting AST in its second. By using a variable in its second
   argument the parser will unify the resulting AST of the given
   regex with the variable. Note that the regex must be ground for
   parse_2_tree/2 to work.

   If you do not need or want the AST you can use generate/2 to generate
   an automaton from your regex. Similary to the parser, the first argument
   is again the regex in backticks and the second is the resulting automaton.
   A variable in the second argument will be unified with the correct
   automaton by the interpreter.

   You can use white space in the regex at your own convenience. Thus
   keeping your regex as tidy and readable as you want. It is not
   registered as part of the regex. Use _ underscore instead, if you
   specifically want space as a character.

   Currently the parser supports the following:
   - The following characters:
   - all standard lower and upper case letters, as well some special
   letters like ä, á, â. See SWI prolog char_type alpha for more
   information.
   - digits (0 - 9)
   - The . is used to represent an arbitrary character.
   - The \s represents a whitespace space character (" " or \t, \v, \f, \n, \r) in an regex.
   - All quantity operations:
   Repeats the given regex a set amount of times.
   - * for zero to arbitrary repeats
   - + for one to arbitrary repeats
   - ? for one or zero repeats
   - {n} for n repetitions
   - {m,n} for m to n repetitions
   - {m,+} for m to n repetitions
   - Alternatives:
   Divide multiple regex and add choice points between them by using |.
   - Brackets:
   Use brackets to clarify the use of operations.

   Thereby the operator precedence is:
   1. brackets
   2. quantity operations ( *, +, ?, {n} )
   3. alternatives separated by |

   Thus the regex `ab*` would define the language a, ab, abb, abbb, abbbb, ...

   Example:

   Let's take the regex `(ab)*c*` as an example call. It defines the
   language {ab^n c^m | n,m >= 0}. Thus an arbitrary number of ab's
   followed by an arbitrary numbers of c's.

   Parser:
   The call parse_2_tree(`(ab)*c*`, A) will unify A with the term
   A = concat(quantity(*, concat(char(a), char(b))), quantity(*, char(c))).

   Interpreter:
   The call generate(`(ab)*c*`, A) will TODO
 */


/* ----- Generating an AST from a given regular expression  ----- */

%! special_character(C) is det.
%
% Succeeds if C needs to be escaped to use,
% e.g. as it would otherwise be an operator.
special_character(C) :-
  memberchk(C, `.?+*|()[]{}\\`).

% characters
% characters(string(I)) --> characters2(D), {atom_codes(I, D)}. deprecated!
characters(char(I)) -->
  char_or_digit(D),
  !,
  { atom_codes(I, D) }.
characters(X) -->
  escaped_character(X).
characters(X) -->
  nonlit(X),
  !.
characters(char(I)) --> % remaining special characters
  [C],
  { code_type(C, punct), \+ special_character(C), char_code(I, C) }.
% characters(char(I)) --> visible(D), !, {atom_codes(I, D)}.
char_or_digit([D]) -->
  [D],
  { code_type(D, alnum) }.
% not only, (D>=65, D=<90); (D>=97, D=<122) alpha also includes special
% letters like ö,ü,a and so on.
% (D>=48, D=<57) for digits

escaped_character(char(X)) -->
  `\\`, [C], {special_character(C), char_code(X, C)}.

nonlit(any) --> `.`.
nonlit(char(I)) -->
  [D],
  {   code_type(D, quote),
      atom_codes(I, [D])
  }.                            % ", ', `
nonlit(char(-)) --> `-`. % we need minus for negative integers
nonlit(whitespace) --> `\\s`. % matches space, newline, tab, carriage return

nonlit(char(=)) --> `=`.
% visible([D]) --> [D], {between(32,126,D)}.
% white space
ws -->
  [D],
  {   code_type(D, white),
      !
  },
  ws.
ws --> ``.


% regular expressions
%expression0([exp(X)]) --> expression(X).
expression(set(X,Y)) --> expression1(X), ws, `|`, !, ws, expression(Y).
expression(X) -->
  expression1(X).

expression1(concat(X,Y)) -->
  expression2(X),
  ws,
  expression1(Y),
  !.
expression1(X) -->
  expression2(X).

expression2(quantity(*,X)) --> expression3(X), ws, `*`, !.
expression2(quantity(+,X)) --> expression3(X), ws, `+`, !.
expression2(quantity(?,X)) --> expression3(X), ws, `?`, !.
expression2(repeat(N, Expression)) -->
  expression3(Expression),
  `{`, number(N) , {N >= 0}, `}`,
  !.
expression2(repeat(N, M, Expression)) -->
  expression3(Expression),
  `{`, number(N), {N >= 0},
  ws, `,`, ws,
  (`+`, {M = '+'} ; number(M), {N =< M}), `}`,
  !.
expression2(X) -->
  expression3(X).

expression3(X) --> ws, `(`, ws, expression(X), ws, `)`, ws, !.
expression3(X) --> ws, char_range(X), ws, !.
expression3(X) -->
  characters(X),
  !.

char_range(ranges(Ranges)) -->
  `[`, char_range_tuples(Ranges), `]`.

char_range_tuples([Tuple|Tuples]) -->
  char_range_tuple(Tuple), char_range_tuples(Tuples).
char_range_tuples([Tuple]) -->
  char_range_tuple(Tuple).

char_range_tuple(From-To) -->
  char_or_digit([A]), `-`, char_or_digit([Z]),
  {A =< Z, atom_codes(From, [A]), atom_codes(To, [Z])}.


/* ----- Generating the Constraint system from the AST  ----- */
parse_2_tree(RegEx, Tree) :-
  expression(Tree, RegEx, []).


generate(RegEx, ResDom) :-
  parse_2_tree(RegEx, Tree),
  % nl,print(Tree),nl,
  build(Tree, ResDom).


/* build_meta([],ResDom) :-
   constant_string_domain("",ResDom).
   build_meta([exp(H)|T],ResDom) :-
   build(H,TempDom1),
   build_meta(T,TempDom2),
   concatenation(TempDom1,TempDom2,ResDom). */


build(char(X), ResDom) :-
  !,
  single_char_domain(X, ResDom).
build(ranges(Ranges), ResDom) :-
  !,
  char_range_domain(ranges(Ranges), ResDom).
build(whitespace, ResDom) :-
  !,
  whitespace_domain(ResDom).
build(any, ResDom) :-
  !,
  any_char_domain(ResDom).
build(concat(char(X),Y), ResDom) :-
  string_check(concat(char(X),Y), CharList),
  !,
  string_chars(String, CharList),
  constant_string_domain(String, ResDom).
build(concat(X,Y), ResDom) :-
  build(X, TempDom1),
  build(Y, TempDom2),
  concatenation(TempDom1, TempDom2, ResDom).
build(set(X,Y), ResDom) :-
  (   X = set(_,_)
  ;
      Y = set(_,_),
      !,
      set_collect(set(X,Y), TempDomList),
      union(TempDomList, ResDom)
  ).
build(set(X,Y), ResDom) :-
  build(X, TempDom1),
  build(Y, TempDom2),
  union(TempDom1, TempDom2, ResDom).
build(quantity(*,X), ResDom) :-
  !,
  build(X, TempDom),
  repeat(TempDom, ResDom).
build(quantity(+,X), ResDom) :-
  !,
  build(X, TempDom),
  repeat(TempDom, RepeatDom),
  concatenation(TempDom, RepeatDom, ResDom).
build(quantity(?,X), ResDom) :-
  !,
  build(X, TempDom),
  repeat(TempDom, 0, 1, ResDom).
build(repeat(N, Chars), ResDom) :-
  string_check(Chars, CharList),
  !,
  string_chars(String, CharList),
  constant_string_domain(String, StringDom),
  repeat(StringDom, N, ResDom).
build(repeat(N, X), ResDom) :-
  !,
  build(X, XDom),
  repeat(XDom, N, ResDom).
build(repeat(Min, +, X), ResDom) :-
  !,
  build(concat(repeat(Min, X), quantity(*, X)), ResDom). % s{n,+} = s{n}s*
build(repeat(Min, Max, X), ResDom) :-
  Max \= +,
  !,
  build(X, XDom),
  repeat(XDom, Min, Max, ResDom).


string_check(char(Y), [Y]).
string_check(concat(char(X),Y), [X|T]) :-
  string_check(Y, T).


set_collect(set(set(L1,R1),set(L2,R2)), ResList) :-
  !,
  set_collect(set(L1,R1), List1),
  set_collect(set(L2,R2), List2),
  append(List1, List2, ResList).
set_collect(set(set(L1,R1),X), ResList) :-
  !,
  set_collect(set(L1,R1), TempList),
  build(X, BuildX),
  append([BuildX], TempList, ResList).
set_collect(set(X,set(L2,R2)), ResList) :-
  !,
  build(X, BuildX),
  set_collect(set(L2,R2), TempList),
  append([BuildX], TempList, ResList).
set_collect(set(X,Y), [BuildX,BuildY]) :-
  build(X, BuildX),
  build(Y, BuildY).
