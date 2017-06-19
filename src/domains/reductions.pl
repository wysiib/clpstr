:- module(reductions, [epsilon_reduce/2,
                      dfa_reduce/2,
                      epsilon_closure/3]).


epsilon_reduce(_,_) :- fail.

dfa_reduce(_,_) :- fail.


%! epsilon_closure(StartState,TransitionList,EpsilonClosure) is det
% Takes a state and a list of transitions and returns all states reachable
% by only using epsilon transitions, starting in StartState. It does not
% include StartState in the closure, when there is no explicit epsilon
% transition in the list.
% NOTE that this is not a formaly correct epsilon closure, since it does not
% include, although it is obviously always reachable via an epsilon
% transition from the initial state.
% Also if this ever breaks, its propably because it does not test whether
% S is actually included in T.
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
