:- module(labeling, [label/2]).



%! label(Domain,Label) is nondet
% Labels a domain to get exactly one value.
% Domain needs to be instantiated.
% Expects either a string_dom or an automaton_dom.
% Calls unfold_tailrec to construct a label.
% @Domain is the domain that is to be labeled.
% @Label is the resulting label as a list of characters.
label(string_dom(S),S).
label(automaton_dom(_,Trans,Starts,Ends),Label) :-
  ground(Label), !, % string is ground: verify if in language
  string_codes(Label,CharList),
  member(StartState,Starts),
  unfold_tailrec(StartState,Trans,Ends,CharList).
label(automaton_dom(_,Trans,Starts,Ends),Label) :-
  % string is var: enumerate all solutions
  member(StartState,Starts),
  unfold_tailrec(StartState,Trans,Ends,CharList),
  string_codes(Label,CharList).

%! unfold_tailrec(CurrentState,Trans,EndStates,ListOfCharacterCodes) is nondet
% Tailrecursively constructs a list of character codes from an automaton.
% From CurrentState a transition in Trans is searched to a new state.
% If an EndStates is reached the recursion stops and returns the found
% character codes to ListOfCharacterCodes
% @CurrentState is the current state of the automaton.
% @Trans is the labeled automaton's transition list.
% @EndStates is the list of the automaton's accepting states.
% @ListOfCharacterCodes is the generated list of charactercodes.
unfold_tailrec(CurrentState,_,FinalStates,[]) :- member(CurrentState,FinalStates).
unfold_tailrec(CurrentState,Transitions,FinalStates,[C|Cs]) :-
  member((CurrentState,range(From,To),NextState),Transitions),
  between(From,To,C),
  unfold_tailrec(NextState,Transitions,FinalStates,Cs).
unfold_tailrec(CurrentState,Transitions,FinalStates,Cs) :-
  member((CurrentState,epsilon,NextState),Transitions),
  unfold_tailrec(NextState,Transitions,FinalStates,Cs).
