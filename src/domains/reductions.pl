:- module(reductions, [epsilon_reduce/2,
                      dfa_reduce/2,
                      epsilon_closure/3,
                      ordered_eps_closure/3,
                      bin_2_new_state/2,
                      gen_bin_states/2,
                      remove_unused/2,
                      breadth_first_state_search/3,
                      clean_automaton/2]).

:- use_module(library(clpfd)).
:- use_module(basic_domains).
:- use_module(basic_operations).

%! epsilon_reduce(OldDomain,ResultingDomain)
% Takes a Domain and applies an epsilon reduction.
% The resulting domain may contain unneccessary not reachable states and
% trainsitions from these states.
% NOTE: this is very expensive because it calls epsilon_reduce_recursive.
% @OldDomain the automaton domain to be reduced.
% @ResultingDomain the domain after the reduction.
epsilon_reduce(Dom,Res) :-
  get_all_states(Dom,States),
  get_transition(Dom,Delta),
  get_end_states(Dom,End),
  epsilon_reduce_recursive(States,States,Delta,End,TempDelta,ResEnd),
  delete_epsilon_transitions(TempDelta, ResDelta),
  set_end_states(Dom,ResEnd,Temp),
  set_transitions(Temp,ResDelta,Res).

%! epsilon_reduce_recursive(States,AllStates,Delta,EndStates,ResDelta,ResEnd)
% Takes the list of states from an automaton domain, its delta and its
% end states and returns new end states and list of transitions according
% to an epsilon reduction.
% Helper predicate for epsilon_reduce.
% NOTE: this is very expensive because it recursivly calls make_transitions.
% @States The list of states the reduction did not consider yet.
% @AllStates the list of all states of the automaton to reduce.
% @Delta the list of transitions of the automaton to reduce.
% @EndStates the list of accepting states of the automaton to reduce.
% @ResDelta the list of transitions of the reduced automaton, still containing
% epsilon trainsitions.
% @ResEnd the list of accepting states of the reduced automaton.
epsilon_reduce_recursive([],_,TempDelta,TempEnd,TempDelta,TempEnd).
epsilon_reduce_recursive([SH|ST],AllStates,Delta,End,ResDelta,ResEndT) :-
  ordered_eps_closure(SH,Delta,EpClo),
  make_accept_states(SH,EpClo,End,TempEnd),!,
  make_transitions(AllStates,SH,EpClo,Delta,TempDelta),
  epsilon_reduce_recursive(ST,AllStates,TempDelta,TempEnd,ResDelta,ResEndT).



%! make_accept_states(StartState,EpsilonClosure,EndStates,ResultingStates)
% Takes a state, its ordered epsilon closure and the list of end states
% and returns the new list of end states.
% Helper predicate for epsilon_reduce_recursive.
% StartState is included in ResultingStates, iff there is a state in
% EpsilonClosure that is also contained in EndStates.
% @StartState is the state for which the status is checked.
% @EpsilonClosure is the epsilon closure of StartState.
% @EndStates is the incoming list of already existing accepting states.
% @ResultingStates is the outgoing list of accepting states.
% Case 1: State is already an end State.
make_accept_states(State,_,End,End) :-
  ord_memberchk(State,End),!.
% Case 2:  No End States in Epsilon closure.
make_accept_states(_,EpClo,End,End) :-
  ord_intersect(EpClo,End,[]),!.
% Case 3: An End State is in Epsilon closure.
make_accept_states(State,_,End,ResEnd) :-
  ord_add_element(End,State,ResEnd).


%! make_transitions(AllStates,CurrentState,EpsilonClosure,CurrentDelta,ResDelta)
% Takes a state, its EpsilonClosure and the list of all transitions and
% returns a new list of transitions that bypass epsilon transitions.
% Helper predicate for epsilon_reduce_recursive.
% The new transitions are created by considering all pairs of states from the
% CurrentState and the elements of AllStates. There is added a new Transition,
% iff there is a state in EpsilonClosure containing a transition to the element
% of AllStates.
% New tranisitons are added at the beginning of ResDelta for efficiency.
% NOTE: This is a very expensive operation since it recursivly calls findall/3
% to make the transitions.
% @AllStates the list of all states from the automaton domain.
% @CurrentState the state from which  new transitions are added.
% @EpsilonClosure the epsilon closure of CurrentState.
% @CurrentDelta the list of transitions during recursion.
% NOTE: this could add unneccessary tranisitions since, unlike in the
% original algorithm, the originial list of transitions is not kept between
% recursion steps. This did never happen in test cases.
% @ResDelta the resulting list of transitions.
make_transitions([],_,_,Delta,Delta).
make_transitions([S|T],S,EpClo,Delta,ResDelta) :-
  !, make_transitions(T,S,EpClo,Delta,ResDelta).
make_transitions([Tar|T],S,EpClo,Delta,ResDelta) :-
  findall((S,range(From,To),Tar),(member(C,EpClo),member((C,range(From,To),Tar),Delta)),SomeOtherDelta),
  append(SomeOtherDelta,Delta,TempDelta),
  make_transitions(T,S,EpClo,TempDelta,ResDelta).


%! delete_epsilon_transitions(CurrentDelta,NewDelta)
% Takes a list of atomaton transitions and deletes all epsilon transitions.
% @CurrentDelta is the list containing epsilon transitions.
% @NewDelta is the original list without epsilon transitions.
delete_epsilon_transitions([],[]).
delete_epsilon_transitions([(_,epsilon,_)|T],Res) :-
  !,delete_epsilon_transitions(T,Res).
delete_epsilon_transitions([(A,Char,B)|T],[(A,Char,B)|Res]) :-
  delete_epsilon_transitions(T,Res).



% TODO Reduce NFA to DFA
dfa_reduce(_,_) :- !, fail.
dfa_reduce(Dom,Res) :-
  get_all_states(Dom,States),
  gen_pow_set(States,PowStates),
  get_transition(Dom,Trans),
  /*length(States,L),
  Pow is 2**L,
  findall(X,between(1,Pow,X),NewStates),

  This code does generate a Power set of States.
  Whether it is really needed depends on the
  implementation of the reduction.*/
  % generate new transitions:
  generate_dfa_transitions(PowStates,Trans,[],NewDelta),
  set_transitions(Dom,NewDelta,Res).


generate_dfa_transitions([],Acc,Acc).
generate_dfa_transitions([Start|PowT],Trans,Acc,Res) :-
  findall(
    (Start,Char,End),
    (member(S,Start),findall(E,member((S,Char,E),Trans),End)),
    NewDelta),
  append(NewDelta,Acc,NewAcc),
  generate_dfa_transitions(PowT,NewAcc,Res).


gen_bin_states(OldStates,ResStates) :-
  length(OldStates,L),
  findall(X,(length(X,L),X ins 0..1,labeling([],X)),ResStates).

gen_pow_set(Set,Res) :-
  findall(Pow,pow_set(Set,Pow),Res).

pow_set([],[]).
pow_set([H|T], [H|P]) :- pow_set(T,P).
pow_set([_|T], P) :- pow_set(T,P).

bin_2_new_state(binstate([]),0) :- !.
bin_2_new_state(binstate(L),ResState) :-
  is_list(L),
  length(L,Temp),
  N is Temp - 1,
  bin_2_new_state_recursive(L,0,N,ResState).

bin_2_new_state_recursive([],Acc,_,Acc).
bin_2_new_state_recursive([0|T],Acc,N,Res) :-
  !, NewN is N - 1,
  bin_2_new_state_recursive(T,Acc,NewN,Res).
bin_2_new_state_recursive([1|T],Acc,N,Res) :-
  NewN is N - 1,
  NewAcc is 2 ** N + Acc,
  bin_2_new_state_recursive(T,NewAcc,NewN,Res).




%! epsilon_closure(StartState,TransitionList,EpsilonClosure) is det
% Takes a state and a list of transitions and returns all states reachable
% by only using epsilon transitions, starting in StartState. It does not
% include StartState in the closure, when there is no explicit epsilon
% transition in the list.
% This does not return an ordered set. Use ordered_eps_closure for the epsilon
% closure as an orderered set.
% NOTE that this is not a formaly correct epsilon closure, since it does not
% include the StartState itself, although it is obviously always reachable via an epsilon
% transition from the initial StartState.
% Also if this ever breaks, its propably because it does not test whether
% S is actually included in T.
% @StartState is the state from which the epsilon closure shall be calculated.
% @TransitionList shall be initiialized by the List of reachable transitions
% by StartState.
% @EpsilonClosure is the resulting list of states representing the epsilon
% closure of StartState.
epsilon_closure(S,Trans,ResClo) :-
  eps_clo_acc(Trans,[],S,[],ResClo).

eps_clo_acc([(S,epsilon,Next)|TT],RemainTrans,S,Stack,[Next|Res]) :-
  !, eps_clo_acc(TT,RemainTrans,S,[Next|Stack],Res).
eps_clo_acc([(R,epsilon,Next)|TT],RemainTrans,S,Stack,Res) :-
  !, eps_clo_acc(TT,[(R,epsilon,Next)|RemainTrans],S,Stack,Res).
eps_clo_acc([_|TT],RemainTrans,S,Stack,Res) :-
  eps_clo_acc(TT,RemainTrans,S,Stack,Res).
eps_clo_acc([],[],_,_,[]):- !.
eps_clo_acc([],RemainTrans,_,[Next|ST],Res) :-
  !,eps_clo_acc(RemainTrans,[],Next,ST,Res).
eps_clo_acc([],_,_,[],[]).


%!ordered_eps_closure(StartState,TransitionList,EpsilonClosure) is det
% Computes the epsilon closure and transforms it into an ordered set.
% This is the same as epsilon_closure, list_to_ord_set.
% @StartState is the state from which the epsilon closure shall be calculated.
% @TransitionList shall be initiialized by the List of reachable transitions
% by StartState.
% @EpsilonClosure is the resulting list of states representing the epsilon
% closure of StartState.
ordered_eps_closure(State,Trans,Res) :-
  epsilon_closure(State,Trans,Clo),
  list_to_ord_set(Clo,Res).


remove_unused(string_dom(X),string_dom(X)) :- !.
remove_unused(Dom,Res) :-
  get_start_states(Dom,Starts),\+ Starts == [], !,
  findall(SingleState,(member(X,Starts),
    breadth_first_state_search(Dom,X,ReachableStates),
    member(SingleState,ReachableStates)),NewStates),
  list_to_ord_set(NewStates,NewOrdStates),
  set_all_states(Dom,NewOrdStates,NewStateDom),
  get_end_states(Dom,Ends),
  ord_intersect(Ends,NewOrdStates,NewEnds),
  set_end_states(NewStateDom,NewEnds,NewEndsDom),
  get_transition(Dom,Trans),
  findall((X,T,Y),(member((X,T,Y),Trans),member(X,NewStates),member(Y,NewStates)),NewTrans),
  set_transitions(NewEndsDom,NewTrans,Res).
/*remove_unused(_,_)
  (Starts == []) -> (nl,print("Can not remove unused States. Automaton has no start states"),
  fail). NOTE consider adding log
*/


breadth_first_state_search(Dom,Start,Res) :-
  get_all_states(Dom,States),
  ord_memberchk(Start,States),
  breadth_first_state_search_acc(Dom,[Start],[Start],[Start],Res).


breadth_first_state_search_acc(_,[],_,Acc,Acc) :- !.
breadth_first_state_search_acc(Dom,Remain,Seen,Acc,Res) :-
  get_transition(Dom,Trans),
  findall(To,(member(R,Remain),member((R,_,To),Trans),\+member(To,Seen)),Destination),!, % Is there a transition?
  ord_add_element(Remain,Seen,NewSeen), % Destination is now visited
  ord_union(Acc,Destination,NewAcc), % collect Destination in Accumulator
  breadth_first_state_search_acc(Dom,Destination,NewSeen,NewAcc,Res).



%! clean_automaton(Domain, CleanDomain)
% Takes a domain and clears it of unused states and transitions,
% as well as espilon self loops, for example resulting from intersection.
% NOTE does not check for unaccepting pathes.
% Returns Domain in case of empty or string domain.
% @Domain is the domain that is about to be cleaned.
% @CleanDomain is the cleaned domain.
clean_automaton(empty,empty) :- !.
clean_automaton(string_dom(String),string_dom(String)) :- !.
clean_automaton(Dom,CleanDom) :-
  % For each initial state DFS to find reachable states and transitions.
  reachable(Dom,(ReachState,ReachTrans)),
  % map new states
  sort(ReachState,OrdStates),
  % generate matches and new states
  MatchDict = matchedStates{},
  match_states(OrdStates,MatchDict,MatchedStates,ResStates),
  % generate new start and end states
  get_start_states(Dom,Starts),
  gen_matched_states(Starts,MatchedStates,ResStarts),
  get_end_states(Dom,Ends),
  gen_matched_states(Ends,MatchedStates,ResEnds),
  % generate new transitions
  % remve epsilon self loops and dummy transitions from reachable call
  gen_matched_trans(ReachTrans,MatchedStates,ResTrans),
  reverse(ResTrans,RevResTrans), % TODO! Do this directly in gen_matched_trans
  CleanDom = automaton_dom(ResStates,RevResTrans,ResStarts,ResEnds).


%! reachable(Domain,(ReachableStates,ReachableTransitions))
% Helper predicate of clean_automaton.
% Takes a domain and returns a domain containing all reachable states and
% transitions from a new start state 0
% Uses a dummy domain and deepth first search to find pathes.
% Dummy state 0 and dummy transitions (0,epsilon,_) remain in automaton
% and are returned in ReachableStates and ReachableTransitions.
% use gen_matched_trans to remove dummy trans.
% @Domain is the domain to be searched.
% @ReachableStates are the states reachable from all starting states of Domain.
% @ReachableTransitions are the Transitions reachable from all starting states
%  of Domain.
reachable(Dom,(ReachState,ReachTrans)):-
  % dummy domain used as a for each initial state
  Dummy = automaton_dom([0],[],[0],[0]),
  concatenation(Dummy,Dom,ConcatDom),
  get_transition(ConcatDom,ConTrans),
  % Use DFS to find transitions and states.
  reachable_acc([0],ConTrans,[],[],(ReachState,ReachTrans)).
reachable_acc([],_,AccTo,AccTrans,(AccTo,AccTrans)).
reachable_acc([S|T],Trans,AccTo,AccTrans,Res) :-
  member((S,R,To),Trans),!,
  ord_add_element(AccTo,To,NewToAcc),
  delete(Trans,(S,R,To),NewTrans),
  reachable_acc([To,S|T],NewTrans,NewToAcc,[(S,R,To)|AccTrans],Res).
reachable_acc([_|T],Trans,AccTo,AccTrans,Res) :-
  reachable_acc(T,Trans,AccTo,AccTrans,Res).


% !match_states(States,Dict,ResDict,ResStates)
% Helper predicate of clean_automaton.
% Takes a list of states and returns a dictionary containing matches of states
% and an ordered list of states.
% The dictionary contains the member of States and matches them to a new
% state number, where missing states are removed.
% ResStates is the complete List of new States.
% States should be ordered and Dict must be ground.
% @States is the list of old States.
% @Dict is the dictionary to be filled.
% @ResDict is the filled Dict.
% @ResStates is the list of new states.
match_states(OrdStates,MatchDict,ResMatchDict,ResStates) :-
  match_states_acc(OrdStates,1,MatchDict,ResMatchDict,ResStates).
match_states_acc([],_,MatchDict,MatchDict,[]).
match_states_acc([HStates|TStates],X,Dict,MatchDict,[X|TX]) :-
  NewX is X + 1,
  put_dict(HStates,Dict,X,NewDict),
  match_states_acc(TStates,NewX,NewDict,MatchDict,TX).


% !gen_matched_states(States,Dict,NewStates)
% Helper predicate of clean_automaton.
% Takes a list of states and a dictionary of state matches and converts the
% states to new states via the dictionary.
% States from States not found in Dict  are not added to NewStates.
% Conserves order of States.
% @States is the list of states to be converted.
% @Dict is the dictionary containing the matches.
% @NewStates is the new list of states.
gen_matched_states([],_,[]).
gen_matched_states([HState|TState],Matches,[HNewState|TNewState]):-
  get_dict(HState,Matches,HNewState),!,
  gen_matched_states(TState,Matches,TNewState).
gen_matched_states([_|TState],Matches,TNewState):-
  gen_matched_states(TState,Matches,TNewState).


% !gen_matched_states(Transitions,Dict,NewTransitions)
% Helper predicate of clean_automaton.
% Takes a list of transtions and a dictionary of state matches and converts the
% transtions to contain the new states.
% Removes dummy transitions and epsilon selfloops.
% @Transitions is the list of transtions to be converted.
% @Dict is the dictionary containing the matches.
% @NewTransitions is the new list of transtions.
gen_matched_trans([],_,[]).
gen_matched_trans([(0,_,_)|TTrans],Matches,NewTrans) :-
  !, gen_matched_trans(TTrans,Matches,NewTrans).
gen_matched_trans([(In,epsilon,In)|TTrans],Matches,NewTrans) :-
  !, gen_matched_trans(TTrans,Matches,NewTrans).
gen_matched_trans([(StateIn,R,StateOut)|TTrans],Matches,[(NewStateIn,R,NewStateOut)|TNewTrans]) :-
  get_dict(StateIn,Matches,NewStateIn),
  get_dict(StateOut,Matches,NewStateOut),
  gen_matched_trans(TTrans,Matches,TNewTrans).
