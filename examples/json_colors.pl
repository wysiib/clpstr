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
    json_colors(Json),
    statistics(walltime, B),
    write(Json),nl,
    B = [_, SinceLast],
    assert(runtime(SinceLast)),
    runs(Ran),
    retractall(runs(_)),
    Ran1 is Ran + 1,
    assert(runs(Ran1)),
    (   Ran1 == Amount
    ->  findall(R, runtime(R), AllR),
        sumlist(AllR, TotalR),
        format("Total walltime for ~w JSON Datasets describing colors: ~w ms", [Amount, TotalR])
    ;   fail).

/*
{ "colors": [
    { "color": "black",
      "code": {
        "rgba": [255,255,255,1],
        "hex": "#000"
      }
    },
    { "color": "white",
      "code": {
        "rgba": [0,0,0,1],
        "hex": "#FFF"
      }
    },
    { "color": "red",
      "code": {
        "rgba": [255,0,0,1],
        "hex": "#FF0"
      }
    },
  ]
}

*/

join_to_concat([], "").
join_to_concat([H], H).
join_to_concat([H1,H2], '+'(H1, H2)).
join_to_concat([H|T], Concat) :-
    join_to_concat(T, H, Concat).

join_to_concat([], Acc, Acc).
join_to_concat([H|T], Acc, Concat) :-
    join_to_concat(T, '+'(H,Acc), Concat).

list_of_colors(AmountOfColors, ColorsList) :-
    list_of_colors(AmountOfColors, [], ColorsList).

list_of_colors(0, Acc, Acc).
list_of_colors(AmountOfColors, Acc, ColorsList) :-
    AmountOfColors \== 0,
    single_color_json(ColorJson),
    AmountOfColors1 is AmountOfColors - 1,
    (   AmountOfColors1 \== 0
    ->  NewAcc = [",",ColorJson|Acc]
    ;   NewAcc = [ColorJson|Acc]
    ),
    list_of_colors(AmountOfColors1, NewAcc, ColorsList).

single_color_code(ClrCode) :-
    str_in(HexCode, "([A-F] | [0-9]){6}"),
    str_label([HexCode]),
    hex_bytes(HexCode, HexBytesTerm),
    term_string(HexBytesTerm, HexBytes1),
    re_replace("\\[", "\\[", HexBytes1, HexBytes2),
    re_replace("\\]", "\\]", HexBytes2, HexBytes),
    ClrCode match "\"code\": \\{ \"rgb\": " + HexBytes + ", \"hex\": " + HexCode + " \\}".

single_color_json(Color) :-
    single_color_code(ClrCode),
    Color match "\\{\"color\": \"" + "[a-z]{1,10}" + "\"," + ClrCode + " \\}".

% just a first draft; currently only works for a single ColorJson
json_colors(Json) :-
    random(0, 10, AmountOfColors),
    %single_color_json(ColorJson1),
    list_of_colors(AmountOfColors, ColorsList),
    join_to_concat(ColorsList, ColorsStr),
    InnerData match ColorsStr,
    Json match "\\{\"colors\": \\[" + InnerData + "\\]\\}",
    str_label([Json]).
