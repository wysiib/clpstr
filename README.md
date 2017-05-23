# clpstr

## Usage
- Get SWI Prolog
- ???
- Profit


## Domain Representation
clpstr works on regular expressions. But as there is no direct prolog
representation for these and the direct work on them  can become cumbersome.
Therefore the program works internally with domains representing regular
expressions.

There are two types of domains implemented in clpstr. These are unified to
simplify program intern processes.

The first one represents strings, i.e. fixed
regular expressions that only can produce themselves.
`string_dom(STRING)` is a simple data structure encapsulating a standard Prolog
string.
Thereby is `STRING` the contained string.

The other domain in clpstr represents automata, i.e. general regular
expressions.
``automaton_dom(STATES,DELTA,START,END)`` is the representation of an automaton.
Thereby `STATES` is the list of states. cplstr states are a list of ordered
integers, e.g. `[1,2,3]` would be a valid list of states.
`DELTA` is the transition function of the automaton. In clpstr it is implemented
as a prolog list containing a three tuples of the state transitions.
A valid transition would be `(1,epsilon,2)`
`START` is the list of valid start states and `END` a list of valid final
states. Both are subsets of `STATES`.


It is not recommended to construct these domains oneself, but instead use one
of the constructors provided by clpstr:
- `constant_string_domain(STRING,RESULT)` takes a string and constructs a
  string_dom.
- `single_char_domain(CHAR,AUTOMATON)` takes a single character and constructs
  an automata which language is only the chosen character.
- `any_char_domain(DOMAIN)` constructs an automata that language is any single
  character.
- eventually more in the future.



## Coding Guidelines
These are guidelines that shall keep the code tidy and clear, as well as ensure
a certain degree of readability.
These guidelines should be kept in mind by contributors and followed as much as
possible.
- between every predicate 2 lines of whitespace for readability.
- indentation is made by two spaces.
- only commit documented code using [Prolog Doc conventionen][1].
- make test cases for the desired behavior for at least every exported predicate.


[1]: http://www.swi-prolog.org/pldoc/doc_for?object=section('packages/pldoc.html') "Prolog Doc conventionen"


## FAQ
coming soonish(TM)
