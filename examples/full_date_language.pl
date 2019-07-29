:- use_module('../src/clpstr').

% This code captures the domain of valid date notations
% from January 1 1 to December 31 9999.
%
% Possible notations are of the forms
% - August 28
% - August 28, 1996
% - Wednesday
% - Wednesday, August 28
% - Wednesday, August 28, 1996
%
% Regex taken and adapted from "Regular expressions for language engineering"
% @article{karttunen1996regular,
%   title={Regular expressions for language engineering},
%   author={Karttunen, Lauri and Chanod, Jean-Pierre and Grefenstette, Gregory and Schille, Anne},
%   journal={Natural Language Engineering},
%   volume={2},
%   number={4},
%   pages={305--328},
%   year={1996},
%   publisher={Cambridge University Press}
% }

:- dynamic runtime/1, runs/1.

benchmarks(Amount, TotalR) :-
    retractall(runtime(_)),
    retractall(runs(_)),
    assert(runs(0)),
    !,
    statistics(walltime, _),
    date2(I),
    write(I),nl,
    statistics(walltime, B),
    % write(I),nl,
    B = [_, SinceLast],
    assert(runtime(SinceLast)),
    runs(Ran),
    retractall(runs(_)),
    Ran1 is Ran + 1,
    assert(runs(Ran1)),
    (   Ran1 == Amount
    ->  findall(R, runtime(R), AllR),
        sumlist(AllR, TotalR),
        format("Total walltime for ~w Date Expressions: ~w ms\n", [Amount, TotalR])
    ;   fail).


date(DateDom) :-
  % Basics
  str_in(DayNames,
    "Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|Sunday"),
  str_in(MonthNames,
    "January|February|March|April|June|July|August|September|October|November|December"),
  str_in(DayNum, "[1-9]|[1-2][0-9]|3[0-1]"),
  str_in(Year,"[1-9][0-9]{0,3}"),
  str_in(Sep, "_"),
  str_in(CSep, ",_"),
  % Construction
  str_concatenation(DayNames, CSep, DaySep),
  str_concatenation(MonthNames, Sep, MonthSep),
  str_concatenation(MonthSep, DayNum, MonthDay),
  str_concatenation(MonthDay, CSep, MonthDaySep),
  str_concatenation(MonthDaySep, Year, MonthDayYear),
  str_union(MonthDayYear, MonthDay, MonthDayOptYear),
  str_concatenation(DaySep, MonthDayOptYear, FullDate),
  str_union(MonthDayOptYear, FullDate, FullDateOptDay),
  str_union(FullDateOptDay, DayNames, DateDom),
  % Labeling
  str_labeling([], [DateDom]).

date2(DateDom) :-
  % Basics
  DayNames str_in "Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|Sunday",
  MonthNames str_in "January|February|March|April|June|July|August|September|October|November|December",
  DayNum str_in "[1-9]|[1-2][0-9]|3[0-1]",
  Year str_in "[1-9][0-9]{0,3}",
  % Construction
  MonthDay match MonthNames + "_" + DayNum,
  MonthDayOptYear match MonthDay + ",_" + Year \/ MonthDay,
  FullDate match DayNames + ",_" + MonthDayOptYear,
  DateDom match MonthDayOptYear \/ FullDate \/ DayNames,
  % Labeling
  str_labeling([], [DateDom]).
