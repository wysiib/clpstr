:- module(labeling, [label/2,labeling/3]).

:- use_module(basic_domains).

label(string_dom(S), S) :-
  !.
label(Dom, Label) :-
  ground(Label),
  !,
  label_dfs(Dom, Label).
label(Dom, Label) :-
  labeling([id_dfs], Dom, Label).

/* labeling(L,Dom,Label) :-
   is_list(L),
   get_time_out(L,TO),
   get_search(L,Search),
   on_exception(X,call_with_time_limit(TO,call_with_time_limit(TO,labeling_with_opt([Search],Dom,Label)),time_out_handler(TO)).
   NOTE: unfortuneately this can't be used, since it does not allow
   for backtracking. Future work: implement timeout!

   get_time_out(L,TO) :-
   member(time_out=TO,L),!.
   get_time_out(_,900).

   get_search(L,dfs) :-
   member(dfs,L),!.
   get_search(L,bfs) :-
   member(bfs,L),!.
   get_search(L,id_dfs) :-
   member(id_dfs,L),!.
   get_search(_,id_dfs).

   time_out_handler(X) :-
   string_concat("The Given Timeout has been reachen. Time Out was: ",X, Err),
   writeln(Err). */
labeling(_, string_dom(S), S) :-
  !.
labeling([], Dom, Label) :-
  labeling([id_dfs], Dom, Label).
labeling([dfs], Dom, Label) :-
  label_dfs(Dom, Label).
labeling([id_dfs], Dom, Label) :-
  ground(Label),
  !,
  label_dfs(Dom, Label).
labeling([id_dfs], Dom, Label) :-
  label_id_dfs(Dom, Label).
labeling([bfs], Dom, Label) :-
  ground(Label),
  !,
  label_dfs(Dom, Label).
labeling([bfs], Dom, Label) :-
  label_bfs(Dom, Label).
labeling([any], Dom, Label) :-
  label_any_dfs(Dom, Label).

%! label(Domain,Label) is nondet
% Labels a domain to get exactly one value.
% Domain needs to be instantiated.
% Expects either a string_dom or an automaton_dom.
% Calls unfold_tailrec to construct a label.
% @Domain is the domain that is to be labeled.
% @Label is the resulting label as a list of characters.
label_dfs(Dom,Label) :-
  ground(Label), !, % string is ground: verify if in language
  string_codes(Label,CharList),
  get_start_states(Dom,Starts),
  member(StartState,Starts),
  History = history{},
  put_dict(StartState,History,visited,NewHistory),
  get_end_states(Dom,Ends),
  get_transition(Dom,Trans),
  unfold_tailrec_ground(StartState,Trans,Ends,NewHistory,CharList).
label_dfs(Dom,Label) :-
  % string is var: enumerate all solutions
  get_start_states(Dom,Starts),
  get_end_states(Dom,Ends),
  get_transition(Dom,Trans),
  member(StartState,Starts),
  History = history{},
  put_dict(StartState,History,visited,NewHistory),
  unfold_tailrec(StartState,Trans,Ends,NewHistory,RangeList),
  translate_ranges(RangeList,Label).

% ! unfold_tailrec(CurrentState,Trans,EndStates,ListOfCharacterCodes) is nondet
% Tailrecursively constructs a list of character codes from an automaton.
% From CurrentState a transition in Trans is searched to a new state.
% If an EndStates is reached the recursion stops and returns the found
% character codes to ListOfCharacterCodes.
% Helper predicate of labeling.
% @CurrentState is the current state of the automaton.
% @Trans is the labeled automaton's transition list.
% @EndStates is the list of the automaton's accepting states.
% @ListOfCharacterCodes is the generated list of charactercodes.
unfold_tailrec(CurrentState, _, FinalStates, _, []) :-
  member(CurrentState, FinalStates).
unfold_tailrec(CurrentState, Transitions, FinalStates, History, RangeList) :-
  find_next_transition(CurrentState, Transitions, History, NewHistory, (CurrentState,Char,NextState)),
  (   Char == epsilon
  ->
      RangeList = Chars
  ;   Char = range(_,_),
      RangeList = [Char|Chars]
  ),
  unfold_tailrec(NextState, Transitions, FinalStates, NewHistory, Chars).

unfold_tailrec_ground(CurrentState, _, FinalStates, _, []) :-
  member(CurrentState, FinalStates).
unfold_tailrec_ground(CurrentState, Transitions, FinalStates, History, CodeList) :-
  find_next_transition(CurrentState, Transitions, History, NewHistory, (CurrentState,Char,NextState)),
  (   Char == epsilon
  ->
      CodeList = Cs
  ;   Char = range(From,To),
      CodeList = [C|Cs],
      between(From, To, C)
  ),
  unfold_tailrec_ground(NextState, Transitions, FinalStates, NewHistory, Cs).


% !find_next_transition(CurrentState,ListOfTransitions,HistoryDict,NewHistoryDict,ResTrans)
% Helper predicate of unfold_tailrec/5.
% Takes a state, a list of transitions and a dict of visited states
% and finds the next transition that is acceptable with respect to loops.
% Transitions outside of a visited loop get prefered.
% @CurrentState is the state the automaton is currently in.
% @ListOfTransitions is the list of transitions from the automaton.
% @HistoryDict is the dict containing the already visited states.
% @NewHistoryDict is the dict after visiting the newly found transition.
% @ResTrans is the found transition.
find_next_transition(CurrentState, Transitions, History, NewHistory, Alternative) :-
  member((CurrentState,C,NextState), Transitions),
  (   get_dict(NextState, History, visited)
  ->
      alternative_transitions((CurrentState,C,NextState), Transitions, History, NewHistory, Alternative)
  ;   put_dict(NextState, History, visited, NewHistory),
      Alternative = (CurrentState,C,NextState)
  ).

% !alternative_transitions(CurrentTrans,ListOfTransitions,HistoryDict,NewHistoryDict,ResTrans)
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
alternative_transitions((CS,_,NextState), Trans, History, NewHistory, Alt) :-
  member((CS,CNew,NewNextState), Trans),
  NewNextState \= NextState,
  \+ get_dict(NewNextState, History, visited),
  put_dict(NewNextState, History, visited, NewHistory),
  Alt = (CS,CNew,NewNextState).
alternative_transitions(Found, _, History, History, Found).


translate_ranges(RangeList, Label) :-
  gen_charlist(RangeList, CharList),
  string_codes(Label, CharList).

% ! gen_charlist(RangeList,CharList)
gen_charlist([], []).
gen_charlist([range(From,To)|RangeT], [C|ResT]) :-
  between(From, To, C),
  gen_charlist(RangeT, ResT).

label_id_dfs(Dom,Label) :-
  % string is var: enumerate all solutions
  get_start_states(Dom,Starts),
  get_end_states(Dom,Ends),
  get_transition(Dom,Trans),
  lst(List),
  member(StartState,Starts),
  History = history{},
  put_dict(StartState,History,visited,NewHistory),
  length(List,L), % some arbitrary termination condition.
  (L >= 10000 -> !, fail;
  unfold_tailrec(StartState,Trans,Ends,NewHistory,List),
  translate_ranges(List,Label)).

%%%% List generation %%%%
% generates a continuesly longer list for id_dfs
lst([]).
lst([_|T]) :-
  lst(T).


%%%%%%%%%%%%%%%%%%%%%%%%%
label_bfs(Dom, Label) :-
  get_start_states(Dom, Starts),
  get_end_states(Dom, Ends),
  get_transition(Dom, Trans),
  member(StartState, Starts),
  queue_new(Queue),
  unfold_tailrec_bfs(StartState, Trans, Ends, [], Queue, RangeList),
  translate_ranges(RangeList, Label).

unfold_tailrec_bfs(CurrentState, _, FinalStates, History, _, RangeList) :-
  member(CurrentState, FinalStates),
  reverse(History, RangeList).
unfold_tailrec_bfs(CurrentState, Transitions, FinalStates, History, Queue, RangeList) :-
  findall(((CurrentState,R,NextState),History), member((CurrentState,R,NextState), Transitions), NextTransitions),
  queue_push_all(NextTransitions, Queue, NewQueue),
  queue_pop(NewQueue, ((_,Char,NextState),OldHistory), PopedQueue),
  (   Char == epsilon
  ->
      NewHistory = OldHistory
  ;   Char = range(_,_),
      NewHistory = [Char|OldHistory]
  ),
  unfold_tailrec_bfs(NextState, Transitions, FinalStates, NewHistory, PopedQueue, RangeList).
unfold_tailrec_bfs(CurrentState, _, FinalStates, _, Queue, []) :-
  queue_is_empty(Queue),
  \+ member(CurrentState, FinalStates),
  !,
  fail.


%%%% Queue handling %%%%
% ! queue_new(Empty_Queue).
queue_new(Q-Q).

% ! queue_push(Old_Queue, NewElement, NewQueue).
queue_push(E, Q-[E|T], Q-T).

% ! queue_push_all(Old_Queue, ListOfNewElements, New_queue).
queue_push_all([], Q, Q).
queue_push_all([H|T], Q, NewQ) :-
  queue_push(H, Q, TempQ),
  queue_push_all(T, TempQ, NewQ).

% ! queue_pop(Old_queue, Next_element, Remaining_queue).
% Fails on empty queue.
queue_pop(Q, _, _) :-
  queue_is_empty(Q),
  !,
  fail.
queue_pop([X|Xs]-T, X, Xs-T).

queue_is_empty(Q-_) :-
  var(Q).

%%%%%%%%%%%%%%%%%%%%%%%%

label_any_dfs(Dom,Label) :-
  % string is var: enumerate all solutions
  get_start_states(Dom,Starts),
  get_end_states(Dom,Ends),
  get_transition(Dom,Trans),
  member(StartState,Starts),
  History = history{},
  put_dict(StartState,History,visited,NewHistory),
  unfold_tailrec_any(StartState,Trans,Ends,NewHistory,CharList),
  string_codes(Label,CharList).

unfold_tailrec_any(CurrentState, _, FinalStates, _, []) :-
  member(CurrentState, FinalStates).
unfold_tailrec_any(CurrentState, Transitions, FinalStates, History, CodeList) :-
  find_next_transition_any(CurrentState, Transitions, History, NewHistory, (CurrentState,Char,NextState)),
  (   Char == epsilon
  ->
      CodeList = Cs
  ;   Char = range(From,To),
      From =< To,
      CodeList = [From|Cs]
  ),
  unfold_tailrec_any(NextState, Transitions, FinalStates, NewHistory, Cs).

find_next_transition_any(CurrentState, Transitions, History, NewHistory, Next) :-
  member((CurrentState,C,NextState), Transitions),
  \+ get_dict(NextState, History, visited),
  put_dict(NextState, History, visited, NewHistory),
  Next = (CurrentState,C,NextState).
