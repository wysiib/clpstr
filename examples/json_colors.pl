:- use_module('../src/clpstr').
:- use_module(library(clpfd)).
:- use_module('../src/domains/basic_domains').

:- dynamic runtime/1.
:- volatile runtime/1.

benchmark_goal(json_colors(1, _)).
benchmark_goal(json_colors(2, _)).
benchmark_goal(json_colors(3, _)).
benchmark_goal(json_colors(4, _)).
benchmark_goal(json_colors(5, _)).
benchmark_goal(json_colors(10, _)).
benchmark_goal(json_colors(50, _)).
benchmark_goal(json_colors(100, _)).
%benchmark_goal(json_colors(1000, _)). % does not to scale in under 20 minutes

benchmarks :-
    retractall(runtime(_)),
    benchmark_goal(Goal),
    statistics(walltime, _),
    Goal,
    statistics(walltime, B),
    B = [_, SinceLast],
    assertz(runtime(SinceLast)),
    fail.
benchmarks :-
    findall(Runtime, runtime(Runtime), Runtimes),
    format("~n~nRuntimes (ms) in order of goals: ~w", [Runtimes]).

/*
Generate data describing colors in JSON:

{ "colors": [
    { "color": "white",
      "code": {
        "rgb": [255,255,255],
        "hex": "#FFFFFF"
      }
    },
    { "color": "black",
      "code": {
        "rgb": [0,0,0],
        "hex": "#000000"
      }
    }
    ...
  ]
}
*/

list_of_hex_codes(0, []) :- !.
list_of_hex_codes(C, [HexCode|T]) :-
    str_in(HexCode, "([A-F] | [0-9]){6}"),
    C1 is C-1, list_of_hex_codes(C1, T).

get_color_entry(Hex, RgbList, Color) :-
    term_string(RgbList, Rgb1),
    escape_special_characters(Rgb1, Rgb),
    Prefix = "\\{\"color\": \"test\",\"code\":\\{ \"rgb\": ",
    Color match Prefix + Rgb + ", \"hex\": #" + Hex + " \\}\\}".

list_of_colors_concat([], [], "").
list_of_colors_concat([Hex|HT], [Rgb|RT], Concat) :-
    get_color_entry(Hex, Rgb, Color),
    list_of_colors_concat_acc(HT, RT, Color, Concat).

list_of_colors_concat_acc([], [], Acc, Acc).
list_of_colors_concat_acc([Hex|HT], [Rgb|RT], Acc, Concat) :-
    get_color_entry(Hex, Rgb, Color),
    NewAcc = '+'(Acc, '+'(",", Color)),
    list_of_colors_concat_acc(HT, RT, NewAcc, Concat).

json_colors(Amount, JSON) :-
    list_of_hex_codes(Amount, LHex),
    str_all_diff(LHex),
    str_label(LHex),
    maplist(hex_bytes, LHex, RgbList),
    list_of_colors_concat(LHex, RgbList, ColorsConcat),
    JSON match "\\{\"colors\": \\[" + ColorsConcat + "\\]\\}",
    str_label([JSON]),
    !.
