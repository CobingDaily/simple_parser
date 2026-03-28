### Let and If Expressions:
```
>>> let a = "abc" in (a * 15)
"abcabcabcabcabcabcabcabcabcabcabcabcabcabcabc"

>>> 10 * (if true then 10 else 5)
100

>>> "abc" == "a" + "b" + "c"
true
```
### Strictly Typed:
```
>>> 3/2
1

>>> 3.0/2.0
1.500000

>>> 3.0 / 2
Error: Failure("cannot infer types")

>>> let pi = 3.14 in pi / 4.0
0.785000
```
### Boolean Logic:
```
>>> 1 == true
false
>>> 1 == false
false
>>> 1 == 1
true
>>> false == false
true
```
