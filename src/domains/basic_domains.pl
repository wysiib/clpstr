:- module(basic_domains,  [constant_string_domain/2,
                           single_char_domain/2,
                           any_char_domain/1]).


%! any_range(Range) is det
% Returns the range of characters represented by '.' in a regex
% @Range the range represented as range(From,To).
any_range(range(32,126)).

%! constant_string_domain(String,ResultingDomain) is det
% Constructs a string domain from a string.
% @String is the String.
% @ResultingDomain is the domain only containing S.
constant_string_domain(S,string_dom(S)) :-
  string(S).

%! single_char_domain(ResultingDomain) is det
% Constructs an automaton domain containing only a single character.
% @CharAsString is a string of length one containing the character in question
% @ResultingDomain is the domain containing only a single character.
single_char_domain(CharAsString,automaton_dom(States,Delta,Start,End)):-
  string_codes(CharAsString,[Char]),
  States = [1,2], % List of states
  Delta = [(1,range(Char,Char),2)], % List of statetransitions
  Start = [1], % List of start states
  End = [2]. % List of end states

%! any_char_domain(ResultingDomain) is det
% TODO
% Constructs an automaton domain containing any character.
% @ResultingDomain is the domain containing any character.
any_char_domain(automaton_dom(States,Delta,Start,End)):-
  any_range(Range),
  States = [1,2], % List of states
  Delta = [(1,Range,2)], % List of statetransitions
  Start = [1], % List of start states
  End = [2]. % List of end states
