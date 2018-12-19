%:- dynamic benchmarks/4.

order([str_in,concat,repeat1,repeat2,repeat3,union,intersec,size,str_prefix,str_suffix,str_infix,str_upper_case,str_lower_case]).
sizes(['10','100','1000']).

:- [full].

gen_table :-
  gen_ordered_entries(dfs,OrdEntnix), print(OrdEntnix),
  gen_ordered_entries(dfs,OrdEntdfs),
  gen_ordered_entries(bfs,OrdEntbfs),
  gen_ordered_entries(dfs,OrdEntiddfs),
  gen_table_rec(OrdEntnix,OrdEntdfs,OrdEntbfs,OrdEntiddfs).

gen_table_rec([HN|TN],[HD|TD],[HB|TB],[HI|TI]) :-
  gen_string_from_entry(HN,HD,HB,HI,S),
  write_data(S),
  gen_table_rec(TN,TD,TB,TI).

write_data(String) :-
  open('result.txt',append,Stream),
  write(Stream,String),
  nl(Stream),
  close(Stream).


gen_string_from_entry(Entry1,Entry2,Entry3,Entry4,String) :-
  make_strings(Entry1,(N,nix,TN1,TN2,TN3)),
  make_strings(Entry2,(N,dfs,TD1,TD2,TD3)),
  make_strings(Entry3,(N,bfs,TB1,TB2,TB3)),
  make_strings(Entry4,(N,id_dfs,TI1,TI2,TI3)),
  entry_concat([N,TN1,TD1,TB1,TI1,TN2,TD2,TB2,TI2,TN3,TD3,TB3,TI3],"",String).

entry_concat([],Res,Res).
entry_concat([H|T],S,Res) :-
  string_und_concat(H,S,NewS),
  entry_concat(T,NewS,Res).

string_und_concat(S1,S2, S3) :-
  string_concat(S1," & ",T),
  string_concat(T,S2,S3).

make_strings((Name,X,Time1,Time2,Time3),(NameS,X,Time1Z,Time2Z,Time3Z)) :-
  atom_string(Name, NameS),
  atom_string(Time1,Time1S),
  change_zero(Time1S,Time1Z),
  atom_string(Time2,Time2S),
  change_zero(Time2S,Time2Z),
  atom_string(Time3,Time3S),
  change_zero(Time3S,Time3Z).

change_zero("0","< 1") :- !.
change_zero(X,"t/o") :- var(X).
change_zero(T,T).

gen_ordered_entries(Search,OrdEnt) :-
  gen_entries(Search,Entries),
  order(X),
  gen_order(X,Entries,[],OrdEnt).

gen_order([],_,Res,Res).
gen_order([OH|OT],Entries,Acc,Res) :-
  member((OH,S,T1,T2,T3),Entries),
  append(Acc,[(OH,S,T1,T2,T3)],NewAcc),
  gen_order(OT,Entries,NewAcc,Res).

gen_entries(Search,Entries) :-
  findall((Name,Search,Time1,Time2,Time3),(
    get_meta(Search,Name,Time1,10),
    get_meta(Search,Name,Time2,100),
    get_meta(Search,Name,Time3,1000)
  ),Entries).


get_meta(Search,Name,Time,Size) :-
  order(X),
  sizes(Y),
  benchmarks(FullName,Search,Time,_),
  member(Name,X),
  atom_concat(_,Name,FullName),
  member(ASize,Y),
  atom_number(ASize,Size),
  atom_concat(_,ASize,FullName).
