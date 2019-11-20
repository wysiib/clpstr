# ConString
A string constraint solver for SWI-Prolog having interfaces to CLP(FD), CLP(B) and CLP(R).

# User Information

## Regular Expression Dialect
The employed alphabet consists of ASCII characters and some special characters like umlauts and accented characters.
Additionally, dynamic character matching is possible by specifying ranges inside square brackets, e.g., `[0-9a-f]`,  or by using the dot operator `.` which matches with any character.
Parentheses can be used to enforce operator precedence.
We represent a whitespace character in regular expressions by `\s` while actual whitespace characters can be used to structure regular expressions without being part of the accepted language.
Further, we support the usual regular expression operators on characters, i.e., quantity operators (`*`, `+` and `?`) and the alternative choice operator (`|`).
On top of those, our regular expressions offer more strict repetition definitions for convenience, noted by
`{n}` (exactly `n` times), `{m,n}` (`m` to `n` times) and `{m,+}` (at least `m` times).

## Supported Constraints

* `str_in/2`: membership to a string domain, e.g., `str_in(S, "[0-9]*"`

* `str_size/2`: exact string length

* `str_max_size/2`: upper bound string length

* `str_concatenation/3`: string concatenation

* `str_repeat/[2, 3, 4]`: finite string repetition

* `str_union/3`: string union, i.e., third argument string variable accept strings that are in one of both domains

* `str_intersection/3`: string intersection

* `str_prefix/2`, `str_suffix/2`, `str_infix/2`: e.g., `str_prefix(S, Prefix)` where Prefix can be a constant string or a string variable itself

* `str_upper_case/1`, `str_lower_case/1`: set the string to accept upper/lower case characters only

* `str_diff/2`: inequality between two string constraints

* `str_all_diff/1`: pairwise inequality for a list of string constraints

* `str_to_int/2`: bidirectional string to integer conversion integrating SWI-Prologs CLP(FD) library

    * any constraint of library(clpfd) can be used on finite domain integer variables
    * does not accept leading zeros, e.g., `str_to_int("01", 1)` does not hold
* `str_to_intl/2`: same as `str_to_int/2` but accepting leading zeros

* `str_to_real/2`: bidirectional string to real conversion integrating SWI-Prologs CLP(R) library

    * any constraint of library(clpr) can be used on floating point numbers
    * does not accept leading zeros
* `str_to_reall/2`: same as `str_to_real/2` but accepting leading zeros

* `str_to_bool/2`: bidirectional string to boolean conversion integrating SWI-Prolog's CLP(B) library

    * any constraint of library(clpb) can be used on boolean variables
    * does not accept leading zeros
* `str_to_booll/2`: same as `str_to_bool/2` but accepting leading zeros

* `str_labeling/2`: labeling with options (CLP(FD) options can be used, too)
    * supported uninformed search strategies are depth-first search (`dfs`), breadth-first search (`bfs`) and iterative deepening depth-first search (`idfs`)

* `str_label/1`: labeling with default options, i.e., `dfs`

# Developer Information

## Domain Representation
ConString works on regular expressions.

There are two types of domains implemented in ConString:
`string_dom(STRING)` represents a *constant string*.

*Regular expressions* are represented as finite automata.

``automaton_dom(STATES,DELTA,START,END)`` is the representation of an automaton.
`STATES` is the list of states. cplstr states are a list of ordered integers, e.g. `[1,2,3]` would be a valid list of states.
`DELTA` is the transition function of the automaton. In ConString it is implemented as a Prolog list containing a triple of the state transitions.
A valid transition would be `(1,epsilon,2)`
`START` is the list of valid start states and `END` a list of valid final states. Both are subsets of `STATES`.

It is not recommended to construct these domains oneself but instead use one of the constructors provided by ConString:
- `constant_string_domain(STRING,RESULT)` takes a string and constructs a string_dom.
- `single_char_domain(CHAR,AUTOMATON)` takes a single character and constructs
  an automata which language is only the chosen character.
- `any_char_domain(DOMAIN)` constructs an automata that language is any single character.
- `generate_domain(REGEX,AUTOMATON)` constructs an automata for a regular expression or a constant string.

## Coding Guidelines
These are guidelines that shall keep the code tidy and clear, as well as ensure a certain degree of readability.
These guidelines should be kept in mind by contributors and followed as much as possible.
- between every predicate 2 lines of whitespace for readability.
- indentation is made by two spaces.
- only commit documented code using [Prolog Doc conventionen][1].
- make test cases for the desired behavior for at least every exported predicate.


[1]: http://www.swi-prolog.org/pldoc/doc_for?object=section('packages/pldoc.html') "Prolog Doc conventionen"
