:- use_module('../src/clpstr').
:- dynamic benchmark/1.

make_all_benchmarks(Directory,Name) :-
  make_all_benchmarks(Directory,Name,[]).

make_all_benchmarks(Directory,Name,Options) :-
  directory_files(Directory,Files),
  writeln(Files),
  maplist(benchmark_test(Name,Directory,Options),Files).

benchmark_test(_,_,_,.) :- !.
benchmark_test(_,_,_,..) :- !.
benchmark_test(ResultName,DirectoryName,Options,FileName) :-
  writeln(FileName),
  atom_concat(DirectoryName,'/',Directory),
  atom_concat(Directory,FileName,FileLocation),
  consult(FileLocation),
  on_exception(time_limit_exceeded,call_with_time_limit(900,run_benchmark(Options,Time,Inferences)),time_out_handler(900)),
  retractall(benchmark(_)),
  write_data(FileName,ResultName,Time,Inferences).

write_data(Name,ResName,Time,Inferences) :-
  atom_concat(result_,ResName,Temp),
  atom_concat(Temp,'.txt',FileName),
  open(FileName,append,Stream),
  write(Stream,benchmarks(Name,Time,Inferences)),
  nl(Stream),
  close(Stream).

gen_file_name(Name,FileName) :-
  atom_concat(Name,'.txt',FileName).

run_benchmark(Options,Time,Inferences) :-
  statistics(walltime,_),
  statistics(inferences,X),
  benchmark(Options),
  statistics(inferences,Y),
  statistics(walltime,[_,Time]),
  Inferences is Y-X.


time_out_handler(X) :-
  string_concat("The Given Timeout has been reachen. Time Out was: ",X, Err),
  writeln(Err).
