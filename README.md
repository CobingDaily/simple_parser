### Functions
```
>>> let add = x -> y -> x + y in
    let square = x -> x*x in
    let add_squares = x -> y -> (add (square x) (square y)) in
    add_squares 3 4
25

>>> ((x -> y -> ((x -> x*x) x + (x -> x*x) y)) 3 4)
25

>>> ((x -> y -> x*x + y*y) 3 4)
25

>>> ((x -> y -> x*x + y*y) 3)
y -> ((3 * 3) + (y * y)
```
### Let and If Expressions:
```
>>> let a = "abc" in (a * 15)
"abcabcabcabcabcabcabcabcabcabcabcabcabcabcabc"

>>> 10 * (if true then 10 else 5)
100
```
### Boolean Logic:
```
>>> 1 == 1
true

>>> false == false
true

>>> "abc" == "a" + "b" + "c"
true
```
### Typechecking
```
>>> if (3 > 2) then 4 else 5.2
Error: Failure("Types in both branches of `if` must match")
```
