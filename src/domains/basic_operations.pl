:- module(basic_operations, [is_empty/1,
                            intersection/3,
                            repeat/3,
                            concatenation/3]).

:- use_module(labeling).


%! is_empty(Domain) is det
% Checks whether an domain is empty, in case of an automaton_dom by having no
% end states or no states at all and by containing the empty fact in all other
% cases.
% @Domain is the Domain to be testet whether it is empty.
is_empty(automaton_dom([],_,_,_)).
is_empty(automaton_dom(_,_,[],_)).
is_empty(empty).


%! intersection(Domain1,Domain2,ResultingDomain) is det
% Generates a domain from the intersection of two domains.
% @Domain1 is the first domain.
% @Domain2 is the second domain.
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


%! repeat(InputDomain,Counter,RepeatedDomain) is det
% Generates a new domain from a domain using an accumulative
% help predicate.
% The new domain contains the original string or automaton times Counter.
% The resulting domain is an automaton if any of the arguments is an automaton.
% Will fail if counter is =< 0
% @InputDomain is a string domain containing the original String or autoamton.
% @Counter is the number of times the string is repeated.
% @RepeatedDomain is the resulting new domain.
repeat(_,C,_) :- C =< 0, !, fail.
repeat(D,C,DOut) :-
  repeat(C,D,D,DOut).

repeat(1,_,D,D) :- !.
repeat(C,D,Acc,Res) :-
  C1 is C - 1,
  concatenation(D,Acc,NewAcc),
  repeat(C1,D,NewAcc,Res).


%! concatenation(FirstDomain,SecondDomain,ConcatDomain) is det
% Concatenates two domains by converting them into automatons and tying them
% together. Uses SWI Prologs build in string_concat if both domains are strings.
% The resulting domain is an automaton if any of the arguments is an automaton.
% @FirstDomain is one of the domains to be concatenated.
% @SecondDomain is one of the domains to be concatenated.
% @ResultingDomain is the resulting new domain.
concatenation(string_dom(S1),string_dom(S2),string_dom(S3)) :-
  string_concat(S1,S2,S3),
  !.
concatenation(A1,A2,automaton_dom(States3,Delta3,Start1,End2Star)) :-
  constant_string_domain_to_automaton(A1,automaton_dom(States1,Delta1,Start1,End1)),
  constant_string_domain_to_automaton(A2,automaton_dom(States2,Delta2,Start2,End2)),
  length(States1,L),
  maplist(plus(L),States2,States2Star), % create new state space.
  flatten([States1,States2Star],States3),

  maplist(plus(L),Start2,Start2Star), % create new delta transition.
  findall((S,epsilon,T),(member(S,End1),member(T,Start2Star)),Trans),
  maplist(adjust_transition(L),Delta2,Delta2Star),
  flatten([Delta1,Trans,Delta2Star],Delta3),

  maplist(plus(L),End2,End2Star). % create new Endspaces.



%! adjust_transition(LengthTillHere,Transition1,Transition2) is det
% Helper predicate used by concat_domain to adjust the state names in a
% transition to be valid transitions in the new domain. TODO
% Should only be called by concatenation.
% @LengthTillHere is the length of the transition.
% @Transition1
% @Transition2
adjust_transition(L,(X,T,Y),(X2,T,Y2)) :-
  plus(X,L,X2),
  plus(Y,L,Y2).
