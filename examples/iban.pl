:- use_module('../src/clpstr').
:- use_module(library(clpfd)).


iban :-
    str_in(DECode,"1314"),
    BBAN in 000000000000000000 ..999999999999999999,
    str_to_int(BBANStr,BBAN),
    % for the "00" to have a domain, we need to set it up independently
    str_in(DoubleZero,"00"),
    str_concatenation(DECode,DoubleZero,ICalcTail),
    str_concatenation(BBANStr,ICalcTail,ICalc),
    ICalcInt in 000000000000000000000000..999999999999999999999999,
    str_to_int(ICalc,ICalcInt),
    Rest in 0..96,
    ICalcInt mod 97 #= Rest,
    str_in(CheckSumStr,"[0-9][0-9]"),
    98 - Rest #= CheckSumInt,
    %str_to_int(CheckSumStr, CheckSumInt),
    str_in(DEConst,"DE"), % ensure this has a domain
    str_concatenation(DEConst,CheckSumStr,IBANPrefix),
    %str_in(IBAN, "DE[0-9]*"),
    %str_size(IBAN, 22),
    str_concatenation(IBANPrefix,BBANStr,IBAN),
    str_labeling([],[BBAN,ICalc,CheckSumInt,BBANStr,DE,ICalcTail,IBANPrefix,IBAN]),
    format('iban found: ~w~n',[IBAN]).