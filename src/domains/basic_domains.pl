:- module(basic_domains,  [constant_string_domain/2,
                           single_char_domain/2,
                           any_char_domain/1,
                           get_all_states/2,
                           get_transition/2,
                           get_start_states/2,
                           get_end_states/2,
                           set_end_states/3,
                           add_end_states/3,
                           add_several_end_states/3,
                           adjust_transition/3,
                           adjust_domain/3,
                           combine_domain/3]).


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
single_char_domain(CharAsString,automaton_dom(States,Delta,Start,End)) :-
  string_codes(CharAsString,[Char]),
  States = [1,2], % List of states
  Delta = [(1,range(Char,Char),2)], % List of statetransitions
  Start = [1], % List of start states
  End = [2]. % List of end states


%! any_char_domain(ResultingDomain) is det
% Constructs an automaton domain containing any character.
% @ResultingDomain is the domain containing any character.
any_char_domain(automaton_dom(States,Delta,Start,End)) :-
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
% Takes an automaton_dom and a list of States and  makes them Endstates.
% Only use states that are part of original automatons state list!
% Returns the InputDomain with NewEndStates as endstates.
% InputDomain must be initiialized to an automaton_dom.
% @InputDomain is an automaton_dom.
% @NewEndStates is a list of states, that must be part of InputDomain's states.
% @OutputDomain is the resulting automaton_dom.
set_end_states(automaton_dom(States,Delta,Start,_),NewEnd,automaton_dom(States,Delta,Start,NewEnd)).
% NOTE if this ever breaks, test whether NewEnd are part of States.
% FOr example by using maplist(member()).


%! adjust_transition(Length,OldTransitionlist,ResultingTransitionlist) is det
% Takes a List of Transitions, usually used by an automaton_dom.
% Addjusts the state names in the given transition list by shifting all
% state names in OldTransition by LengthTillHere.
% @Length is the length of the transition shift.
% @OldTransition is the list of transitions to be changed.
% @ResultingTransition is the new list of transitions.
adjust_transition(L,Delta,Res) :-
  maplist(adjust_single_transition(L),Delta,Res).


%! adjust_single_transition(Length,OldTransition,ResultingTransition) is det
% Addjusts the state names of a single transition, by shifting all state names
% in OldTransition by LengthTillHere.
% This Predicate is usually only used by adjust_transition and not usable
% outside of basic_domains.pl.
% @Length is the length of the transition shift.
% @OldTransition is the transition to be changed.
% @ResultingTransition is the new transition.
adjust_single_transition(L,(X,T,Y),(X2,T,Y2)) :-
  plus(X,L,X2),
  plus(Y,L,Y2).

%! adjust_domain(Length,InputDomain,ResultingDomain) is det
% Takes a domain and adjusts all state names in the domain's definition, i.e.
% in States, Delta, Start, and End.
% The length of the shift is Length.
% @Length is the length of the domain shift.
% @InputDomain is the domain to be changed.
% @ResultingDomain is the new shifted domain.
adjust_domain(L,string_dom(String),automaton_dom(ResStates,ResDelta,ResStart,ResEnd)) :-
  constant_string_domain_to_automaton(string_dom(String),automaton_dom(States,Delta,Start,End)),
  maplist(plus(L),States,ResStates),
  adjust_transition(L,Delta,ResDelta),
  maplist(plus(L),Start,ResStart),
  maplist(plus(L),End,ResEnd),
  !.
adjust_domain(L,automaton_dom(States,Delta,Start,End),automaton_dom(ResStates,ResDelta,ResStart,ResEnd)) :-
  maplist(plus(L),States,ResStates),
  adjust_transition(L,Delta,ResDelta),
  maplist(plus(L),Start,ResStart),
  maplist(plus(L),End,ResEnd).

%! combine_domain(Domain1,Domain2,ResultingDomain) is det
% Takes two domains and returns a domain containing both of them.
% Note that the States will not be remained.
% Thus ResultingDomain can contain multiple States with the same Name.
% Use adjust_domain to rename states.
% @Domain1 is the first domain to be combined.
% @Domain2 is the second domain to be combined.
% @ResultingDomain is the combined domain.
combine_domain(automaton_dom(States1,Delta1,Start1,End1),automaton_dom(States2,Delta2,Start2,End2),automaton_dom(ResStates,ResDelta,ResStart,ResEnd)) :-
  append(States1,States2,ResStates),
  append(Delta1,Delta2,ResDelta),
  append(Start1,Start2,ResStart),
  append(End1,End2,ResEnd).


add_end_states(automaton_dom(States,Delta,Start,Ends),AdditionalEnds,automaton_dom(States,Delta,Start,NewEnds)) :-
    ord_union(Ends,AdditionalEnds,NewEnds).


add_several_end_states(automaton_dom(States,Delta,Start,Ends),ListOfAdditionalEnds,automaton_dom(States,Delta,Start,NewEnds)) :-
    ord_union([Ends|ListOfAdditionalEnds],NewEnds).
