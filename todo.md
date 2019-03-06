# TODO

## Refactoring

* [ ] Change internal representation of automata
  * [ ] Change naming convention of states to `id-num` format,
    `id` being a unique identifier for the automaton,
    `num` being the name of the state
    * [ ] Change list of states to list of tuples of `id` and number of states
    * [ ] Add `id` to automaton term so each carries its own id
  * [ ] Change lists to difference lists
* [ ] Increase performance of several predicates
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
* [ ] Merge [project todo list](ProjektToDoListe.md) into this one
