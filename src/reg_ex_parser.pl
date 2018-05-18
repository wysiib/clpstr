:- module(reg_ex_parser, [generate/2,
                          parse_2_tree/2]).

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

      You can use white space in the regex at your own convinience. Thus
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
          - The _ represents the space character, " ", in an regex.
      - All quantity operations:
        Repeats the given regex a set amount of times.
          - * for zero to infinit repeats
          - + for one to infinit repeats
          - ? for one or zero repeats
      - Alternatives:
        Divide multiple regex and add choice points between them by using |.
      - Brackets:
        Use brackets to clarify the use of operations.

      Thereby the operator precedence is:
        1. brackets
        2. quantity operations ( *, +, ? )
        3. alternatives seperated by |

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

% characters
%characters(string(I)) --> characters2(D), {atom_codes(I, D)}.
characters(char(I)) --> char_or_digit(D), !, {atom_codes(I, D)}.
characters(any) --> `.`. % any
characters(char(I)) --> `_`, {atom_codes(I, [32])}. % space
%characters(nonliteral(D)) --> nonlit(D), !. % TODO add some more
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
  %nl,print(Tree),nl,
  build(Tree,ResDom).


build_meta([],ResDom) :-
  constant_string_domain("",ResDom).
build_meta([exp(H)|T],ResDom) :-
  build(H,TempDom1),
  build_meta(T,TempDom2),
  concatenation(TempDom1,TempDom2,ResDom).


build(char(X),ResDom) :-
  !, single_char_domain(X,ResDom).
build(any,ResDom) :-
  !, any_char_domain(ResDom).
build(concat(char(X),Y),ResDom) :-
  string_check(concat(char(X),Y),CharList), !,
  string_chars(String,CharList),
  constant_string_domain(String,ResDom).
build(concat(X,Y),ResDom) :-
  build(X,TempDom1),
  build(Y,TempDom2),
  concatenation(TempDom1,TempDom2,ResDom).
build(set(X,Y),ResDom) :-
  X = set(_,_);
  Y = set(_,_),!,
  set_collect(set(X,Y),TempDomList),
  union(TempDomList,ResDom).
build(set(X,Y),ResDom) :-
  build(X,TempDom1),
  build(Y,TempDom2),
  union(TempDom1,TempDom2,ResDom).
build(quantity(*,X),ResDom) :-
  !,build(X,TempDom),
  repeat(TempDom,ResDom).
build(quantity(+,X),ResDom) :-
  !,build(X,TempDom),
  repeat(TempDom,RepeatDom),
  concatenation(TempDom,RepeatDom,ResDom).
build(quantity(?,X),ResDom) :-
  !,build(X,TempDom),
  repeat(TempDom,0,1,ResDom).


string_check(concat(char(X),char(Y)),[X,Y]).
string_check(concat(char(X),Y),[X|T]) :-
  string_check(Y,T).


set_collect(set(set(L1,R1),set(L2,R2)),ResList) :-
  !, set_collect(set(L1,R1),List1),
  set_collect(set(L2,R2),List2),
  append(List1,List2,ResList).
set_collect(set(set(L1,R1),X),ResList) :-
  !, set_collect(set(L1,R1),TempList),
  build(X,BuildX),
  append([BuildX],TempList,ResList).
set_collect(set(X,set(L2,R2)),ResList) :-
  !, build(X,BuildX),
  set_collect(set(L2,R2),TempList),
  append([BuildX],TempList,ResList).
set_collect(set(X,Y),[BuildX,BuildY]) :-
  build(X,BuildX),
  build(Y,BuildY).
