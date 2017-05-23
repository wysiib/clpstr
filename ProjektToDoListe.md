# Projekt TODO-list:

## general
- [ ] [Prolog Doc conventionen][1] in gesamtem project durchsetzen
**MÖGLICHST BALD!**
- [x] make "any" a charakter in all transition.
    - Aktuell kann `any` nur als allein stehendes Zeichen eingelesen werden im
    Prädikat labeling/make_string. Ziel ist es, dass es letztendlich während
    des errechnens eines Labels gemacht wird. Z.B. in label/unfold_tailrec.

## basic operations and domains
- [x] intersection.plt tests wieder einschalten
- [ ] intersection fertig schreiben

- [ ] Create more labeling tests.
    - Sobald mehr automaton_dom verfügbar sind als nur any_char_domain werden
    mehr Tests benötigt.
- [x] repeat for automaton_dom
  - [ ] turn on str_in.plt tests using repeat, after fixing automaton repeat.
- [ ] add more tests for repeat/2, testing whether it correctly accepts (word)* , i.e. wordwordword and not something like wordwowordwword or such things.
- [ ] add tests for adjust_transition and adjust_automaton_dom.


## read me
- [ ] add usage part
- [ ] add FAQ part
- [ ] add bug part when at least beta ready

[1]: http://www.swi-prolog.org/pldoc/doc_for?object=section('packages/pldoc.html') "pldoc"
