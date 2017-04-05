:- module(basic_domains,[constant_string_domain/2,
                         any_char_domain/1]).

%! automaton(Automaton) is det
% The representation of an automaton.
% TODO
% @Automaton is the resulting Automaton
automaton(A).


%! constant_string_domain(String,ResultingDomain) is det
% Constructs a string domain from a string.
% @String is the String.
% @ResultingDomain is the domain only containing S.
constant_string_domain(S,string_dom(S)) :-
  string(S).

%! any_char_domain(ResultingDomain) is det
% Constructs an automaton domain containing any character.
% TODO
% @ResultingDomain is the domain containing any character.
any_char_domain(D).
