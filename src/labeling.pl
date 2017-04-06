:- module(labeling, [label/2]).



%! label(Domain,Label) is nondet
% Labels a domain to get exactly one value.
% Domain needs to be instantiated.
% Expects either a string_dom or an automaton_dom.
% Calls unfold_tailrec to construct a label.
% @Domain is the domain that is to be labeled.
% @Label is the resulting label as a list of characters.
label(string_dom(S),S).
label(Automaton,Label) :-
  unfold_tailrec(Automaton,CharList),
  make_string(Label,CharList).


%! unfold_tailrec(AutomatonDomain,ListOfCharacterCodes) is nondet
% Tailrecursively constructs a list of character codes from an automaton.
% AutomatonDomain needs to be instantiated.
% Uses member to find viable states in the statespace and relies on all_transition
% to gain viable transitions.
% @AutomatonDomain is the incoming automaton.
% @ListOfCharacterCodes is the generated list of charactercodes.
unfold_tailrec(automaton_dom(_,_,End,End),[]).
unfold_tailrec(automaton_dom(States,Trans,Start,End),[ResH|ResT]) :-
  member(StartState,Start),
  all_transition(Trans,StartState,Next,ResH),
  unfold_tailrec(automaton_dom(States,Trans,[Next],End),ResT).

%! all_transition(ListOfTransitions,StartState,EndState,ResultingCharacter) is nondet
% Seaches the whole list of transitions for a viable transition.
% @ListOfTransitions is the list of transitions and covers the whole statespace.
% @StartState is the current state of the automaton and
% thus the state to start the search with.
% @EndState is the next state after a transition was found.
% @ResultingCharacter is the character enabling the transition from StartState
% to EndState.
all_transition(Trans,Start,End,Char):-
  member((Start,Char,End),Trans).


%! make_string(String,ListOfCharacters) is nondet
% Constructs a string from a list containing charaktercodes.
% can also process special codes such as:
% - any
% - more may be added in the future.
% @String is the generaed string.
% @ListOfCharacters is a List containing the corresponding charactercodes.
make_string(Label,[any]) :-
  numlist(32,126,ListOfAllCharacters),
  member(Charakter,ListOfAllCharacters),
  string_codes(Label,[Charakter]).
make_string(Label,CharList):-
  string_codes(Label,CharList).
