:- use_module('../src/clpstr').
:- use_module(library(clpfd)).

iban :-
    Rest in 10..96,
    ICalc in 100000000000000000131400..999999999999999999131499,
    ICalc mod 97 #= Rest,
    clpfd:labeling([], [ICalc, Rest]),
    str_in(BBANStr, "[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]"),
    str_in(DECode, "1314"),
    str_in(CheckSumStr, "[0-9][0-9]"),
    str_to_int(CheckSumStr, Rest),
    str_to_int(StrCalc, ICalc),
    str_in(DECodeCheckSum, "[0-9][0-9][0-9][0-9][0-9][0-9]"),
    str_concatenation(BBANStr, DECodeCheckSum, StrCalc),
    str_labeling([],[BBANStr, DECodeCheckSum, StrCalc]),
    str_concatenation(DECode, CheckSumStr, DECodeCheckSum),
    str_in(IBAN, "[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]"),
    str_in(DE, "DE"),
    str_in(Prefix, "DE[0-9][0-9]"),
    str_concatenation(DE, CheckSumStr, Prefix),
    str_concatenation(Prefix, BBANStr, IBAN),
    str_labeling([], [CheckSumStr, Prefix, IBAN]).
