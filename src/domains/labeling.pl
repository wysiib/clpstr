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
  member(StartState,Starts),
  unfold_tailrec(StartState,Trans,Ends,CharList),
  string_codes(Label,CharList).


%! unfold_tailrec(AutomatonDomain,ListOfCharacterCodes) is nondet
% Tailrecursively constructs a list of character codes from an automaton.
% AutomatonDomain needs to be instantiated.
% Uses member to find viable states in the statespace and relies on all_transition
% to gain viable transitions.
% @AutomatonDomain is the incoming automaton.
% @ListOfCharacterCodes is the generated list of charactercodes.
unfold_tailrec(CurrentState,_,FinalStates,[]) :- member(CurrentState,FinalStates).
unfold_tailrec(CurrentState,Transitions,FinalStates,[C|Cs]) :-
  member((CurrentState,range(From,To),NextState),Transitions),
  between(From,To,C),
  unfold_tailrec(NextState,Transitions,FinalStates,Cs).
