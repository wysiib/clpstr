:- use_module('../src/clpstr').
:- use_module(library(clpfd)).
:- use_module('../src/domains/basic_domains').

:- dynamic runtime/1, runs/1.
:- volatile runtime/1, runs/1.


benchmarks(Amount) :-
    retractall(runtime(_)),
    retractall(runs(_)),
    assert(runs(0)),
    !,
    statistics(walltime, _),
    bibid(I),
    statistics(walltime, B),
    write(I),nl,
    B = [_, SinceLast],
    assert(runtime(SinceLast)),
    runs(Ran),
    retractall(runs(_)),
    Ran1 is Ran + 1,
    assert(runs(Ran1)),
    (   Ran1 == Amount
    ->  findall(R, runtime(R), AllR),
        sumlist(AllR, TotalR),
        format("Total walltime for ~w Bib-ID's: ~w ms", [Amount, TotalR])
    ;   fail).

bibid(Bid) :-
    str_in(Bid, "[A-Z]{2}[1-9][0-9]{6}"),
    str_label([Bid]).

% Should get the first character of PreName and SurName and concat them.
% But does not work however.
bibid2(FirstLetterPreName) :-
    str_in(PreName, "Max"),
    %str_in(SurName, "Mustermann"),
    any_char_domain(I),
    str_in(FirstLetterPreName, I),
    str_prefix(PreName, FirstLetterPreName),
    str_label([FirstLetterPreName]).

