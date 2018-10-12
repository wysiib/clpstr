:- module(labeling, [label/2,label/3]).

label(Dom,Label) :-
  label_dfs(Dom,Label).
label([dfs],Dom,Label) :-
  label_dfs(Dom,Label).
label([id_dfs],_,_) :-
  fail.
label([bfs],Dom,Label) :-
  label_bfs(Dom,Label).

%! label(Domain,Label) is nondet
% Labels a domain to get exactly one value.
% Domain needs to be instantiated.
% Expects either a string_dom or an automaton_dom.
% Calls unfold_tailrec to construct a label.
% @Domain is the domain that is to be labeled.
% @Label is the resulting label as a list of characters.
label_dfs(string_dom(S),S).
label_dfs(Dom,Label) :-
  ground(Label), !, % string is ground: verify if in language
  string_codes(Label,CharList),
  get_start_states(Dom,Starts),
  member(StartState,Starts),
  History = history{},
  put_dict(StartState,History,visited,NewHistory),
  get_end_states(Dom,Ends),
  get_transition(Dom,Trans),
  unfold_tailrec(StartState,Trans,Ends,NewHistory,CharList).
label_dfs(Dom,Label) :-
  % string is var: enumerate all solutions
  get_start_states(Dom,Starts),
  get_end_states(Dom,Ends),
  get_transition(Dom,Trans),
  member(StartState,Starts),
  History = history{},
  put_dict(StartState,History,visited,NewHistory),
  unfold_tailrec(StartState,Trans,Ends,NewHistory,CharList),
  string_codes(Label,CharList).

%! unfold_tailrec(CurrentState,Trans,EndStates,ListOfCharacterCodes) is nondet
% Tailrecursively constructs a list of character codes from an automaton.
% From CurrentState a transition in Trans is searched to a new state.
% If an EndStates is reached the recursion stops and returns the found
% character codes to ListOfCharacterCodes.
% Helper predicate of labeling.
% @CurrentState is the current state of the automaton.
% @Trans is the labeled automaton's transition list.
% @EndStates is the list of the automaton's accepting states.
% @ListOfCharacterCodes is the generated list of charactercodes.
unfold_tailrec(CurrentState,_,FinalStates,_,[]) :-
  member(CurrentState,FinalStates).
unfold_tailrec(CurrentState,Transitions,FinalStates,History,CodeList) :-
  find_next_transition(CurrentState,Transitions,History,NewHistory,(CurrentState,Char,NextState)),
  (Char == epsilon
  -> CodeList = Cs
  ;  Char = range(From,To), between(From,To,C), CodeList = [C|Cs]),
  unfold_tailrec(NextState,Transitions,FinalStates,NewHistory,Cs).

%!find_next_transition(CurrentState,ListOfTransitions,HistoryDict,NewHistoryDict,ResTrans)
% Helper predicate of unfold_tailrec/5.
% Takes a state, a list of transitions and a dict of visited states
% and finds the next transition that is acceptable with respect to loops.
% Transitions outside of a visited loop get prefered.
% @CurrentState is the state the automaton is currently in.
% @ListOfTransitions is the list of transitions from the automaton.
% @HistoryDict is the dict containing the already visited states.
% @NewHistoryDict is the dict after visiting the newly found transition.
% @ResTrans is the found transition.
find_next_transition(CurrentState,Transitions,History,NewHistory,Alternative) :-
  member((CurrentState,C,NextState),Transitions),
  (get_dict(NextState,History,visited)
  ->  alternative_transitions((CurrentState,C,NextState),Transitions,History,NewHistory,Alternative)
  ;   put_dict(NextState,History,visited,NewHistory),
      Alternative = (CurrentState,C,NextState)).

%!alternative_transitions(CurrentTrans,ListOfTransitions,HistoryDict,NewHistoryDict,ResTrans)
% Helper predicate of find_next_transition/5.
% Takes a transition, a list of transitions and a dict of visited states
% and if there is an alternative transition, such that the state it leads to
% is unvisited, the transition is returned instead.
% If no such transition exists, CurrentTrans is returned.
% @CurrentTrans is the transition currently used in find_next_transition/5.
% @ListOfTransitions is the list of transitions from the automaton.
% @HistoryDict is the dict containing the already visited states.
% @NewHistoryDict is the dict after visiting the newly found transition.
% @ResTrans is the found transition.
alternative_transitions((CS,_,NextState),Trans,History,NewHistory,Alt) :-
  member((CS,CNew,NewNextState),Trans),
  NewNextState \= NextState,
  \+ get_dict(NewNextState,History,visited),
  put_dict(NewNextState,History,visited,NewHistory),
  Alt = (CS,CNew,NewNextState).
alternative_transitions(Found,_,History,History,Found).


label_bfs(string_dom(S),S).
label_bfs(_,_) :-
  fail.

label_id_dfs(string_dom(S),S).
label_id_dfs(_,_) :-
  fail.
