:- use_module('../src/clpstr').
:- use_module(library(clpfd)).

iban(IBAN) :-
    Rest in 0..96,
    ICalc in 100000000000000000..999999999999999999,
    ICalc2 #= ICalc * 1000000 + 131400, % ICalc2 has the constant suffix 131400
    ICalc2 mod 97 #= Rest,
    clpfd:labeling([],[ICalc2, Rest]),
    str_in(BBANStr, "[0-9]{18}"),
    str_in(DECodeCheckSumStr, "[0-9]{6}"),
    str_to_int(StrCalc, ICalc2),
    str_concatenation(BBANStr, DECodeCheckSumStr, StrCalc),
    str_in(IBAN, "DE[0-9]{20}"),
    str_in(DE, "DE"),
    CheckSum #= 98 - Rest,
    str_to_int(CheckSumStr, CheckSum),
    str_concatenation(DE, CheckSumStr, IBANPrefix),
    str_concatenation(IBANPrefix, BBANStr, IBAN),
    str_labeling([], [IBAN]). % [ICalc2, Rest, IBAN] clpfd variables can also be labeled here which, however, is a bit slower
