:- module(basic_operations, [is_empty/1,
                            intersection/3,
                            repeat/3]).

:- use_module(labeling).


%! is_empty(Automaton) is det
% checks whether an automaton is empty, by having no states.
% TODO
is_empty(automaton_dom([],_,_,_)).
is_empty(empty).


%! intersection(String1,String2,ResultingDomain) is det
% Generates a domain from the intersection of two domains.
% @String1 is the first domain.
% @String2 is the second domain.
% @ResultingDomain is the domain containing the Intersection of the two strings.
intersection(S,S,S) :-
  !.
intersection(string_dom(S1),string_dom(S2),empty) :-
  S1 \= S2,
  !.
intersection(Dom1,string_dom(S), string_dom(S)) :-
  label(Dom1,S),
  !.
intersection(string_dom(S),Dom2,string_dom(S)) :-
  label(Dom2,S),
  !.
intersection(_,_,empty).

%! repeat(StringDomain,Counter,RepeatedString) is det
% Generates a new string domain from a string domain using an accumulative
% help predicate.
% The new domain contains the original string times Counter.
% @StringDomain is a string domain containing the original String.
% @Counter is the nuumber of times the string is repeated.
% @RepeatedString is the resulting new string domain.
repeat(string_dom(S),C,string_dom(Label)) :-
  repeat_acc(S,"",C,Label).


%! repeat_acc(String,Accumulator,Counter,NewString) is det
% Generates recursively a repeated String from a String by using
% an accumulator. This predicate should only be called by the repeat predicate
% and not by its own. It should only be called with an Counter > 0.
% @String is the string to repeat.
% @Accumulator collects the new String in the recursion.
% @Counter is the nuumber of times the string shall be repeated.
% NewString is the recursion's solution.
repeat_acc(_,Acc,0,Acc) :- !.
repeat_acc(String,Acc,C,Res) :-
  C > 0,
  C1 is C - 1,
  string_concat(String,Acc,NewAcc),
  repeat_acc(String,NewAcc,C1,Res).
