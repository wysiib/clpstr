:- module(basic_domains,  [constant_string_domain/2,
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

%! any_char_domain(ResultingDomain) is det
% TODO
% Constructs an automaton domain containing any character.
% @ResultingDomain is the domain containing any character.
any_char_domain(automaton_dom(States,Delta,Start,End)):-
  any_range(Range),
  States = [start,end], % List of states
  Delta = [(start,Range,end)], % List of statetransitions
  Start = [start], % List of start states
  End = [end]. % List of end states
