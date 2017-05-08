:- module(basic_domains,  [constant_string_domain/2,
                           single_char_domain/2,
                           any_char_domain/1,
                           get_all_states/2,
                           get_transition/2,
                           get_start_states/2,
                           get_end_states/2,
                           set_end_states/3]).


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


%! get_all_states(AutomatonDom,ReturnStates) is det
% Returns the List of all states of a given automaton_dom.
% AutomatonDom must be initiialized to an automaton.
% @AutomatonDom is the automaton_dom to return her states.
% @ReturnStates is the List of states of AutomatonDom.
get_all_states(automaton_dom(States,_,_,_),States).


%! get_transition(AutomatonDom,ReturnTransition) is det
% Returns the List of state transitions of a given automaton_dom.
% AutomatonDom must be initiialized to an automaton.
% @AutomatonDom is the automaton_dom to return her states.
% @ReturnTransition is the List of transitions of AutomatonDom.
get_transition(automaton_dom(_,Delta,_,_),Delta).


%! get_start_states(AutomatonDom,ReturnStates) is det
% Returns the List of all start states of a given automaton_dom.
% AutomatonDom must be initiialized to an automaton.
% @AutomatonDom is the automaton_dom to return her states.
% @ReturnStates is the List of start states of AutomatonDom.
get_start_states(automaton_dom(_,_,Start,_),Start).


%! get_end_states(AutomatonDom,ReturnStates) is det
% Returns the List of all end states of a given automaton_dom.
% AutomatonDom must be initiialized to an automaton.
% @AutomatonDom is the automaton_dom to return her states.
% @ReturnStates is the List of end states of AutomatonDom.
get_end_states(automaton_dom(_,_,_,End),End).

%! set_end_states(InputDomain,NewEndStates,OutputDomain) is det
% Returns the InputDomain with new end states.
% TODO
set_end_states(automaton_dom(States,Delta,Start,_),NewEnd,automaton_dom(States,Delta,Start,NewEnd)).
% NOTE if this ever breaks, test whether NewEnd are part of States.
% FOr example by using maplist(member()).
