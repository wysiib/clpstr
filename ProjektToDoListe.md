# Projekt TODO-list:

## Refactoring

* [ ] Change internal representation of automata
  * [ ] Change naming convention of states to `id-num` format,
    `id` being a unique identifier for the automaton,
    `num` being the name of the state
    * [ ] Change list of states to list of tuples of `id` and number of states
    * [ ] Add `id` to automaton term so each carries its own id
  * [ ] Change lists to difference lists
* [X] Increase performance of several predicates
  * See todos in code
* [ ] Change and update grammar
  * [ ] Do not ignore whitespace
  * [ ] Increase performance
    * `expression2` for instance can be made more efficient when the choice
      point is set before the operator instead of before `expression3`
* [ ] Check documentation for consistency and topicality
  * [ ] Make entries more relevant
    * Documenting an argument called `Domain1` as "the first domain" carries
      no further information (cf. `combine_domain/3`)

## general
- [ ] [Prolog Doc conventionen][1] in gesamtem project durchsetzen
**MÖGLICHST BALD!**
- [x] make "any" a charakter in all transition.
    - Aktuell kann `any` nur als allein stehendes Zeichen eingelesen werden im
    Prädikat labeling/make_string. Ziel ist es, dass es letztendlich während
    des errechnens eines Labels gemacht wird. Z.B. in label/unfold_tailrec.

## basic operations and domains
- [x] intersection.plt tests wieder einschalten
- [x] intersection fertig schreiben
- [x] Epsilon Reduktion für automaton_dom hinzufügen
    - [x] accepting states
    - [x] add new transitions
    - [x] delete epsilon transitions
- [x] Create more labeling tests.
    - Sobald mehr automaton_dom verfügbar sind als nur any_char_domain werden
    mehr Tests benötigt.
- [x] repeat for automaton_dom
  - [x] turn on str_in.plt tests using repeat, after fixing automaton repeat.
- [ ] add more tests for repeat/2, testing whether it correctly accepts (word)* , i.e. wordwordword and not something like wordwowordwword or such things.
- [ ] add tests for adjust_transition and adjust_automaton_dom.
- [ ] add DFA reduction.
- [ ] Fix epsilon_closure, see 2. note in documentary.
- [x] add n-union. Union with a list.
- [ ] add empty automaton domain.
- [ ] adjust_domain nochmal angucken wegen doppeltem choicepoint.

## Tests
- [ ] add "Actual == Expected" to all tests. (?)

## read me
- [ ] add usage part
- [ ] add FAQ part
- [ ] add bug part when at least beta ready

[1]: http://www.swi-prolog.org/pldoc/doc_for?object=section('packages/pldoc.html') "pldoc"
