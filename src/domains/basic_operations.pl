:- module(basic_operations, [is_empty/1,
                            intersection/3,
                            repeat/3,
                            concat_domain/3]).

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





/*
repeat(automaton_dom(States,Delta,Start,End),C,Res) :-
  length(States,L)
  double_state_space(States,L,NewStates),

repeat(automaton_dom(States,Delta,Start,End),C,Res) :-
  fail.




double_state_space([OldH|OldT],L,[NewH|NewT]) :-
  NewH is OldH + L,
  double_state_space(OldT,L,NewT)
double_state_space([],New). */



%! concat_domain(FirstDomain,SecondDomain,NewDomain)
concat_domain(string_dom(S1),string_dom(S2),string_dom(S3)) :-
  string_concat(S1,S2,S3).
concat_domain(automaton_dom(States1,Delta1,Start1,End1),automaton_dom(States2,Delta2,Start2,End2),automaton_dom(States3,Delta3,Start1,End2Star)) :-
  length(States1,L),
  maplist(plus(L),States2,States2Star), % create new state space.
  flatten([States1,States2Star],States3),

  maplist(plus(L),Start2,Start2Star), % create new delta transition.
  findall((S,epsilon,T),(member(S,End1),member(T,Start2Star)),Trans),
  maplist(adjust_transition(L),Delta2,Delta2Star),
  %adjust_transition(Delta2,L,Delta2Star),
  flatten([Delta1,Trans,Delta2Star],Delta3),

  maplist(plus(L),End2,End2Star). % create new Endspaces.

/*connect_by_transition(List1,Liste2,ResList) :-
  maplist((List1,epsilon),List2,ResList).*/

adjust_transition(L,(X,T,Y),(X2,T,Y2)) :-
  plus(X,L,X2),
  plus(Y,L,Y2).
