# λynx
> Functional programming language inspired by OCaml and Racket
### Functions
###### factorial
```
let rec factorial = x ->
  if x <= 1 then 1
  else x * factorial (x-1)
in

factorial 5
```
> This will evaluate to: `120`
###### fibonacci
```
let rec fib = n ->
  if n <= 0 then 0
  else if n == 1 then 1
  else if n == 2 then 1
  else fib (n-1) + fib (n-2)
in

fib 10
```
> This will evaluate to: `55`
### Higher Order Functions
```
let apply_thrice = func -> arg ->
    func o func o func arg
in

apply_thrice (x -> x + 1) 2
```
> This will evaluate to: `5`
### Partial Applications
```
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
