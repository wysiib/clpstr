:- module(basic_operations, [is_empty/1,
                            intersection/3]).

%! is_empty(Automaton).
% checks whether an autoamon is empty, by having no states.
% TODO
is_empty(automaton_dom([],_,_,_)).
is_empty(empty).

%! Intersection(String1,String2,ResultingDomain) is det
% Generates a domain from the intersection of two strings.
% @String1 is the first string.
% @String2 is the second string.
% @ResultingDomain is the domain containing the Intersection of the two strings.
intersection(S,S,S) :-
  !.
intersection(string_dom(S1),string_dom(S2),empty) :-
  S1 \= S2.
