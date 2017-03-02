# Notes from Week 1

## Data Types

* Numbers
* Atoms
* Booleans
* Tuples and Lists
* Functions

### Numbers

* integers and floats
* integers are "bignums"
* Using different bases: `base#number`
* Operators: `+ - * / div rem`

### Atoms

* Examples:

```erlang
foo
'I am an atom too'
```

* Can be compared for equality, ordering ...
* Two special atoms, the booleans: `true`, `false`

### Tuples

* Putting values together, typically heterogenous.
* Erlang idiom: use the first field to indicate what sort of data in the
  tuple.

```erlang
{rectangle, {1.2, 1.5}, {2.4, 5.0}}
{cicle, {1.1, 2.1}, 3.0}
```

### Lists

* Collections of values, typically homogenous.

```erlang
["Jose", "Corcuera"]
[1,2,3,4]
[]
```

### Strings

* Are just lists.

```erlang
"abc".
[97,98,99].
[$a, $b, $c].
```

### Functions

* Can be data themselves.
* Can be arguments of other functions.

## Guards

* Can contain arithmetical operations and comparisons.
* Can be combined with `and` (`,`) and `or` (`;`).
* Can't use user-defined function.

```erlang
greater_than_16(N) when N>16 -> true;
greater_than_16(_) -> false.
```


