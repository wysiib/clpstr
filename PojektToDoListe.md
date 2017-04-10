# Projekt TODO-list:

- [x] intersection.plt tests wieder einschalten
- [ ] intersection fertig schreiben
- [x] make "any" a charakter in all transition.
    - Aktuell kann `any` nur als allein stehendes Zeichen eingelesen werden im
    Prädikat labeling/make_string. Ziel ist es, dass es letztendlich während
    des errechnens eines Labels gemacht wird. Z.B. in label/unfold_tailrec.

- [ ] Create more labeling tests.
    - Sobald mehr automaton_dom verfügbar sind als nur any_char_domain werden
    mehr Tests benötigt.
- [ ] repeat for automaton_dom
- [ ] turn on str_in.plt tests using repeat, after fixing automaton repeat.
- [ ] [Prolog Doc conventionen][1] in gesamtem project durchsetzen
**MÖGLICHST BALD!**

[1]: http://www.swi-prolog.org/pldoc/doc_for?object=section('packages/pldoc.html') "pldoc"
