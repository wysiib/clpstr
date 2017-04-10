:- module(domain_conversion, [constant_string_domain_to_automaton/2]).

constant_string_domain_to_automaton(string_dom(Str),automaton_dom(States,Trans,[1],[FinalState])) :-
    string_codes(Str,Codes),
    string_codes_to_automaton(Codes,1,States,Trans,FinalState).
    
string_codes_to_automaton([],Final,[Final],[],Final).
string_codes_to_automaton([C|Cs],CurStateId,[CurStateId|States],[(CurStateId,range(C,C),NextStateId)|Trans],FinalState) :-
    NextStateId is CurStateId + 1,
    string_codes_to_automaton(Cs,NextStateId,States,Trans,FinalState).