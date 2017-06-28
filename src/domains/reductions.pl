:- module(reductions, [epsilon_reduce/2,
                      dfa_reduce/2,
                      epsilon_closure/3,
                      ordered_eps_closure/3]).


 epsilon_reduce(Dom,Res) :-
   get_all_states(Dom,States),
   get_transition(Dom,Delta),
   get_start_states(Dom,Start),
   get_end_states(Dom,End),
   epsilon_reduce_recursive(States,Delta,End,ResDelta,ResEnd),
   Res = automaton_dom(States,ResDelta,Start,ResEnd).

epsilon_reduce_recursive(States,Trans,End,[ResTH|ResTT],[ResEndH|ResEndT]) :-
  States = [SH|ST],
  ordered_eps_closure(SH,Trans,EpClo),
  make_accept_states([SH|EpClo],End,ResEndH), % TODO
  make_transitions(SH,EpClo,Trans,ResTH), % TODO
  wonderfunction(ST,Trans,End,ResTT,ResEndT).


dfa_reduce(_,_) :- fail.


%! epsilon_closure(StartState,TransitionList,EpsilonClosure) is det
% Takes a state and a list of transitions and returns all states reachable
% by only using epsilon transitions, starting in StartState. It does not
% include StartState in the closure, when there is no explicit epsilon
% transition in the list.
% This does not return an ordered set. Use ordered_eps_closure for the epsilon
% closure as an orderered set.
% NOTE that this is not a formaly correct epsilon closure, since it does not
% include, although it is obviously always reachable via an epsilon
% transition from the initial state.
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
