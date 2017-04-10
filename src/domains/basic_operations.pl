:- module(basic_operations, [is_empty/1,
                            intersection/3,
                            repeat/3]).

:- use_module(labeling).


%! is_empty(Automaton) is det
% checks whether an automaton is empty, by having no states.
% TODO
is_empty(automaton_dom([],_,_,_)).
is_empty(empty).


%! Intersection(String1,String2,ResultingDomain) is det
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


repeat(string_dom(S),C,string_dom(Label)) :-
  repeat_acc(S,"",C,Label).

repeat_acc(_,Acc,0,Acc).
repeat_acc(String,Acc,C,Res) :-
  C1 is C - 1,
  string_concat(String,Acc,NewAcc),
  repeat_acc(String,NewAcc,C1,Res).
