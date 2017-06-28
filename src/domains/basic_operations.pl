:- module(basic_operations, [is_empty/1,
                            intersection/3,
                            repeat/2,
                            repeat/3,
                            repeat/4,
                            concatenation/3,
                            union/3,
                            calcse/4]).

:- use_module(labeling).
:- use_module(domain_conversion).

%! is_empty(Domain) is det
% Checks whether an domain is empty, in case of an automaton_dom by having no
% end states or no states at all and by containing the empty fact in all other
% cases.
% @Domain is the Domain to be testet whether it is empty.
is_empty(automaton_dom([],_,_,_)).
is_empty(automaton_dom(_,_,[],_)).
is_empty(empty).


%! intersection(Domain1,Domain2,ResultingDomain) is det
% Generates a domain from the intersection of two domains.
% @Domain1 is the first domain.
% @Domain2 is the second domain.
% @ResultingDomain is the domain containing the Intersection of the two strings.
intersection(S,S,S) :-
  !.
intersection(string_dom(S1),string_dom(S2),empty) :-
  S1 \= S2,
  !.
intersection(Dom1,string_dom(S), string_dom(S)) :-
  label(Dom1,S),
  !.
intersection(string_dom(S),Dom2,string_dom(S)) :-
  label(Dom2,S),
  !.
intersection(Dom1,Dom2,Res) :-
  get_all_states(Dom1,L1),
  get_all_states(Dom2,L2),
  L is L1 * L2,
  numlist(1,L,ResStates),
  % State product:
  % X : Dom1.States, Y : Dom2.States
  % (X,Y) = (X-1)*L2 + Y
  % e.g.
  % (1x1) = (1-1)*L2 + 1
  % (2x3) = (2-1)*L2 + 3
  get_transition(Dom1,Delta1),
  get_transition(Dom2,Delta2),
  %specialfunctionfortransitions(Delta1,Delta2,ResDelta),
  findall((Start,Char,End),(((member((S1,Char,E1),Delta1),member((S2,Char,E2),Delta2));
                            (member((S1,epsilon,E1),Delta1),S2=E2);
                            (member((S2,epsilon,E2),Delta2),S1=E1)),
                            calcse(S1,S2,L,Start),calcse(E1,E2,L,End)),ResDelta),
  get_start_states(Dom1,Start1),
  get_start_states(Dom2,Start2),
  findall(E,(member(X,Start1),member(Y,Start2),calcse(X,Y,L,E)),ResStart),
  get_end_states(Dom1,End1),
  get_end_states(Dom2,End2),
  findall(E,(member(X,End1),member(Y,End2),calcse(X,Y,L,E)),ResEnd),
  Res = automaton_dom(ResStates,ResDelta,ResStart,ResEnd).
intersection(_,_,empty).


% TODO
calcse(State1,State2,L,ResState) :-
  ResState is (State1 - 1) * L + State2.


%! repeat(InputDomain,Counter,RepeatedDomain) is det
% Generates a new domain from a domain using an accumulative
% help predicate repeat_acc/4.
% The new domain contains the original string or automaton times Counter.
% The resulting domain is an automaton if any of the arguments is an automaton.
% Will fail if counter is =< 0
% @InputDomain is a string domain containing the original String or autoamton.
% @Counter is the number of times the string is repeated.
% @RepeatedDomain is the resulting new domain.
repeat(_,C,_) :- C =< 0, !, fail.
repeat(D,C,DOut) :-
  repeat_acc(C,D,D,DOut).


%! repeat(InputDomain,OutputDomain) is det
% Takes a domain and generates an infitie loop automaton representing the
% regular expression InputDomain*.
% The resulting automaton accepts zero to infinite times the original domain's
% accepted words.
% String domains are automatically converted to automatons.
% @InputDomain is the Domain that shall be repeated.
% @OutputDomain is the resulting Domain as an automaton.
repeat(Dom,automaton_dom(States,DeltaRes,Start,EndRes)) :-
  constant_string_domain_to_automaton(Dom,automaton_dom(States,Delta,Start,End)),
  flatten([Start,End],EndRes),
  findall((E,epsilon,S),(member(E,End),member(S,Start)),Trans),
  flatten([Delta,Trans],DeltaRes).


%! repeat(InputDomain,From,To,RepeatedDomain) is det
% Generates a new domain from a domain using an accumulative
% help predicate repeat_acc_with_end/4.
% The new domain contains the original string or automaton a number of times
% between From and To.
% The resulting domain is an automaton and has fianl states that may end any
% labeling in between From and To.
% Will fail if From < 0, To < 0 or  To < From.
% @InputDomain is a string domain containing the original String or autoamton.
% @From is the number of times the string is at least repeated.
% @To is the number of times the string is at most repeated.
% @RepeatedDomain is the resulting new domain.
% The resulting domain is always an automaton.
repeat(_,From,To,_) :- From < 0; To < 0; To < From, !, fail.
repeat(Dom,FromTo,FromTo,Res) :- repeat(Dom,FromTo,Res).
repeat(Dom,0,To,automaton_dom(States,Delta,Start,End)) :-
  repeat(Dom,1,To,automaton_dom(States,Delta,Start,TempEnd)),
  ord_union(Start,TempEnd,End),
  !.
repeat(Dom,From,To,Res) :-
  constant_string_domain_to_automaton(Dom,TempDom),
  repeat(TempDom,From,FromDom),
  Len is To - From,
  repeat_acc_with_end(Len,TempDom,ToDom),
  concatenation(FromDom,ToDom,ConcatDom),
  get_end_states(FromDom,EndsFrom),
  add_end_states(ConcatDom,EndsFrom,Res).


%! repeat_acc(Counter,Input,Accumulator,Output)
% helper predicate for repeat/3
% Recursively creates an automaton Domain from InputDomain by repeating it
% Counter times.
% The resulting automaton will have epsilon transitions inbetween two
% repetitions.
% @Counter is the number of times input domain will be repeated.
% @Input is the domain that is repeated.
% @Accumulator is used for the recursion. Ususally instantiated with Input.
% @Output contains the repeated Domain.
repeat_acc(1,_,D,D) :- !.
repeat_acc(C,D,Acc,Res) :-
  C1 is C - 1,
  concatenation(D,Acc,NewAcc),
  repeat_acc(C1,D,NewAcc,Res).


%! repeat_acc_with_end(Counter,InputDomain,Accumulator,ResultingDomain) is det
% Helper Predicate for repeat/4.
% Recursively creates an automaton Domain from InputDomain by repeating it
% Counter times. Thereby the end states between the repeated domains remain
% and are not deleted as they are in repeat acc.
% The resulting automaton will have epsilon transitions inbetween two
% repetitions.
% @Counter is the number of times input domain will be repeated.
% @InputDomain is the domain that is repeated.
% @ResultingDomain contains the repeated Domain.
repeat_acc_with_end(C,Dom,DomOut) :-
  repeat_acc_with_end(C,Dom,Dom,[[]],DomOut).
repeat_acc_with_end(1,_,Dom,Ends,DomOut) :- !,
  add_several_end_states(Dom,Ends,DomOut).
repeat_acc_with_end(C,Dom,Acc,AdditionalEnds,Res) :-
  C1 is C - 1,
  concatenation(Acc,Dom,TempAcc),
  get_end_states(Acc,Ends),
  repeat_acc_with_end(C1,Dom,TempAcc,[Ends|AdditionalEnds],Res).


%! concatenation(FirstDomain,SecondDomain,ConcatDomain) is det
% Concatenates two domains by converting them into automatons and tying them
% together. Uses SWI Prologs build in string_concat if both domains are strings.
% The resulting domain is an automaton if any of the arguments is an automaton.
% @FirstDomain is one of the domains to be concatenated.
% @SecondDomain is one of the domains to be concatenated.
% @ResultingDomain is the resulting new domain.
concatenation(string_dom(S1),string_dom(S2),string_dom(S3)) :-
  string_concat(S1,S2,S3),
  !.
concatenation(A1,A2,automaton_dom(States3,Delta3,Start1,End2Star)) :-
  constant_string_domain_to_automaton(A1,automaton_dom(States1,Delta1,Start1,End1)),
  constant_string_domain_to_automaton(A2,automaton_dom(States2,Delta2,Start2,End2)),
  length(States1,L),
  maplist(plus(L),States2,States2Star), % create new state space.
  flatten([States1,States2Star],States3),
  maplist(plus(L),Start2,Start2Star), % create new delta transition.
  findall((S,epsilon,T),(member(S,End1),member(T,Start2Star)),Trans),
  adjust_transition(L,Delta2,Delta2Star),
  flatten([Delta1,Trans,Delta2Star],Delta3),
  maplist(plus(L),End2,End2Star). % create new Endspaces.


%! union(InputDomain1,InputDomain2,ResultingDomain) is det
% Builds the union of InputDomain1 and InputDomain2.
% Thereby a new start state is created, but the old final states are kept.
% The input domains can either be strings or automatons.
% @InputDomain1 is the first domain of the union.
% @InputDomain2 is the second domain of the union.
% @ResultingDomain is the union of InputDomain1 and InputDomain2.
union(Dom1,Dom2,Res) :-
  constant_string_domain_to_automaton(Dom1,AutomDom1),
  constant_string_domain_to_automaton(Dom2,AutomDom2),
  get_all_states(AutomDom1,D1States),
  length(D1States,L),
  adjust_domain(L,AutomDom2,AdjDom2),
  combine_domain(AutomDom1,AdjDom2,CombiDom),
  UniDom = automaton_dom([1],[],[1],[1]),
  concatenation(UniDom,CombiDom,Res).
