:- module(reductions, [epsilon_reduce/2,
                      dfa_reduce/2,
                      epsilon_closure/3]).


epsilon_reduce(_,_) :- fail.

dfa_reduce(_,_) :- fail.



epsilon_closure(S,[TH|TT],[Next|ResT]) :-
  TH = (S,epsilon,Next),
  !,
  epsilon_closure(Next,TT,Res1),
  epsilon_closure(S,TT,Res2),
  append(Res1,Res2,ResT).
epsilon_closure(S,[_|TT],Res) :-
  epsilon_closure(S,TT,Res),!.
epsilon_closure(_,[],[]).
