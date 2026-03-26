```
Input: "3.5 / 4.3"
Result: (3.500000f / 4.300000f)

Input: "3 - 2.1"
Result: (3 - 2.100000f)

Input: "3.2 - 3 / 1"
Result: (3.200000f - (3 / 1))

Input: "let x = 5 in let y = 3 in x + y * 3"
Result: (let x = 5 in (let y = 3 in (x + (y * 3))))
```
### If Else Expressions
```
------------------------------                                                                   
Input: "if (5*5 == 25) then (42 * 3.14) else 0.0"                                                
Result: (if ((5 * 5) == 25) then (42 * 3.140000f) else 0.000000f)                                
131.880000                                                                                       
------------------------------                                                                   
Input: "if (5*5 == 24) then (42 * 3.14) else 0.0"                                                
Result: (if ((5 * 5) == 24) then (42 * 3.140000f) else 0.000000f)                                
0.000000                                                                                         
------------------------------                                                                   
```
