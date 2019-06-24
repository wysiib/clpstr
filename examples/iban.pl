:- use_module('../src/clpstr').
:- use_module(library(clpfd)).

:- dynamic runtime/1, runs/1.
:- volatile runtime/1, runs/1.

benchmarks(Amount) :-
    retractall(runtime(_)),
    retractall(runs(_)),
    assert(runs(0)),
    !,
    statistics(walltime, _),
    iban(I),
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
        format("Total walltime for ~w IBANs: ~w ms", [Amount, TotalR])
    ;   fail).

% fastest
iban(IBAN) :-
    Rest in 0..96,
    TICalc in 100000000000000000..999999999999999999,
    ICalc #= TICalc * 1000000 + 131400, % ICalc has the constant suffix 131400
    ICalc mod 97 #= Rest,
    str_label([ICalc, Rest]),
    BBAN is ICalc // 1000000,
    str_to_int(BBANStr, BBAN),
    CheckSum #= 98 - Rest,
    str_to_intl(CheckSumStr, CheckSum),
    str_size(CheckSumStr, 2), % TODO: without size constraint we possibly find a solution several times
    str_in(DE, "DE"),
    str_concatenation(DE, CheckSumStr, IBANPrefix),
    str_concatenation(IBANPrefix, BBANStr, IBAN),
    str_label([IBAN]).

iban2(IBAN) :-
    Rest in 0..96,
    TICalc in 100000000000000000..999999999999999999,
    ICalc #= TICalc * 1000000 + 131400, % ICalc has the constant suffix 131400
    ICalc mod 97 #= Rest,
    str_label([ICalc, Rest]),
    str_in(BBANStr, "[0-9]{18}"),
    str_in(DECodeCheckSumStr, "[0-9]{6}"),
    str_to_int(StrCalc, ICalc),
    str_concatenation(BBANStr, DECodeCheckSumStr, StrCalc),
    str_in(IBAN, "DE[0-9]{20}"),
    str_in(DE, "DE"),
    CheckSum #= 98 - Rest,
    str_to_intl(CheckSumStr, CheckSum),
    str_size(CheckSumStr, 2), % TODO: without size constraint we possibly find a solution several times
    str_concatenation(DE, CheckSumStr, IBANPrefix),
    str_concatenation(IBANPrefix, BBANStr, IBAN),
    str_label([IBAN]). % [ICalc, Rest, IBAN] clpfd variables can also be labeled here which, however, is a bit slower

iban3(IBAN) :-
    Rest in 0..96,
    TICalc in 100000000000000000..999999999999999999,
    ICalc #= TICalc * 1000000 + 131400, % ICalc has the constant suffix 131400
    ICalc mod 97 #= Rest,
    str_label([ICalc, Rest]),
    str_in(BBANStr, "[0-9]{18}"),
    str_to_int(StrCalc, ICalc),
    StrCalc match BBANStr + "[0-9]{6}",
    CheckSum #= 98 - Rest,
    str_to_int(CheckSumStr, CheckSum),
    str_in(IBAN, "DE[0-9]{20}"),
    IBAN match "DE" + CheckSumStr + BBANStr,
    str_label([IBAN]).
