# Type Inference Algorithm in Prolog

## Testing
```prolog
?- [typeInf].
?- consult('typeInf.plt'), run_tests().
```
Note: these tests are based off the ones written in typeInf.plt

### Supported Function Types
* (int + int) -> int
* (float + float) -> float
* ((int) float) -> int
* ((float) int) -> float
* lessThan(int, int) -> bool
* greaterThan(int, int) -> bool
* and(bool, bool) -> bool
* or(bool, bool) -> bool
* not(bool) -> bool
* print(any) -> unit

### Supported Basic Types
* int
* float
* string
* bool
* unit
* a list containing any of the above