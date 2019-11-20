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
Generate data in JSON describing colors:

{ "colors": [
    { "color": "black",
      "code": {
        "rgba": [255,255,255,1],
        "hex": "#000000"
      }
    },
    { "color": "white",
      "code": {
        "rgba": [0,0,0,1],
        "hex": "#FFFFFF"
      }
    }
    ...
  ]
}

*/

list_of_hex_codes(0, []) :- !.
list_of_hex_codes(C, [HexCode|T]) :-
    str_in(HexCode, "([A-F] | [0-9]){6}"),
    C1 is C-1,
    list_of_hex_codes(C1, T).

get_color_entry(Hex, RgbList, Color) :-
    term_string(RgbList, Rgb1),
    escape_special_characters(Rgb1, Rgb),
    Color match "\\{\"color\": \"test\",\"code\":\\{ \"rgb\": " + Rgb + ", \"hex\": #" + Hex + " \\}\\}".

list_of_colors_concat([], [], "").
list_of_colors_concat([Hex|HT], [Rgb|RT], Concat) :-
    get_color_entry(Hex, Rgb, Color),
    list_of_colors_concat_acc(HT, RT, Color, Concat).

list_of_colors_concat_acc([], [], Acc, Acc).
list_of_colors_concat_acc([Hex|HT], [Rgb|RT], Acc, Concat) :-
    get_color_entry(Hex, Rgb, Color),
    list_of_colors_concat_acc(HT, RT, '+'(Acc, '+'(",", Color)), Concat).

json_colors(Out) :-
    random(0, 10, Amount),
    format("~nAmount of colors: ~d~n", [Amount]),
    list_of_hex_codes(Amount, LHex),
    str_all_diff(LHex),
    str_label(LHex),
    maplist(hex_bytes, LHex, RgbList),
    list_of_colors_concat(LHex, RgbList, ColorsConcat),
    Json match "\\{\"colors\": \\[" + ColorsConcat + "\\]\\}",
    str_label([Json]),
    remove_escape_special_characters(Json, Out).
