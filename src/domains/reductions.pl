:- module(reductions, [epsilon_reduce/2,
                      dfa_reduce/2,
                      epsilon_closure/3,
                      ordered_eps_closure/3]).

% TODO DOC
epsilon_reduce(Dom,Res) :-
  get_all_states(Dom,States),
  get_transition(Dom,Delta),
  get_end_states(Dom,End),
  epsilon_reduce_recursive(States,States,Delta,End,TempDelta,ResEnd),
  delete_epsilon_transitions(TempDelta, ResDelta),
  set_end_states(Dom,ResEnd,Temp),
  set_transitions(Temp,ResDelta,Res).

% TODO DOC
epsilon_reduce_recursive([],_,TempDelta,TempEnd,TempDelta,TempEnd).
epsilon_reduce_recursive([SH|ST],AllStates,Delta,End,ResDelta,ResEndT) :-
  ordered_eps_closure(SH,Delta,EpClo),
  make_accept_states(SH,EpClo,End,TempEnd),!,
  make_transitions(AllStates,SH,EpClo,Delta,TempDelta),
  epsilon_reduce_recursive(ST,AllStates,TempDelta,TempEnd,ResDelta,ResEndT).



%! make_accept_states(StartState,EpsilonClosure,EndStates,ResultingStates)
% Takes a state, its ordered epsilon closure and the list of end states
% and returns the new list of end states.
% Helper predicate for the epsilon_reduce_recursive
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


make_transitions([],_,_,Delta,Delta).
make_transitions([S|T],S,EpClo,Delta,ResDelta) :-
  !, make_transitions(T,S,EpClo,Delta,ResDelta).
make_transitions([Tar|T],S,EpClo,Delta,ResDelta) :-
  findall((S,Char,Tar),(member(C,EpClo),member((C,Char,Tar),Delta)),SomeOtherDelta),
  append(SomeOtherDelta,Delta,TempDelta),
  make_transitions(T,S,EpClo,TempDelta,ResDelta).


delete_epsilon_transitions([],[]).
delete_epsilon_transitions([(_,epsilon,_)|T],Res) :-
  !,delete_epsilon_transitions(T,Res).
delete_epsilon_transitions([(A,Char,B)|T],[(A,Char,B)|Res]) :-
  delete_epsilon_transitions(T,Res).



% TODO Reduce NFA to DFA
dfa_reduce(_,_) :- fail.


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
% S is actually included in T or because it expects the transition in an
% ordered state.
% @StartState is the state from which the epsilon closure shall be calculated.
% @TransitionList shall be initiialized by the List of reachable transitions
% by StartState.
% @EpsilonClosure is the resulting list of states representing the epsilon
% closure of StartState.
epsilon_closure(S,[TH|TT],[Next|ResT]) :-
  TH = (S,epsilon,Next),
  !,
  epsilon_closure(Next,TT,Res1),
  epsilon_closure(S,TT,Res2),
  append(Res1,Res2,ResT).
epsilon_closure(S,[_|TT],Res) :-
  epsilon_closure(S,TT,Res),!.
epsilon_closure(_,[],[]).

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
