# TODO

## Refactoring

* [ ] Change internal representation of automata
  * [ ] Reduce list of states to single integer
    * State names are 1..n. Instead of having the whole list to traverse,
      simply use n as value.
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
