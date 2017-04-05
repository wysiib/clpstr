:- module(basic_operations,[is_empty/1,
          repeat/4, intersection/3]).

%! Intersection(String1,String2,ResultingDomain) is det
% Generates a domain from the intersection of two strings.
% @String1 is the first string.
% @String2 is the second string.
% @ResultingDomain is the domain containing the Intersection of the two strings.
intersection(S1,S2,Inter):-
  S1=S2,
  Inter = S1.
