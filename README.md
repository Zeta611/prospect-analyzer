# Ln
## Usage
To run, execute
```sh
./l <test.l
```
after `make`.

## Sample code
```
# lang 0
# 1 1
# 2 0
# 3 3
let v (x, 3) case v (y, z) if v.1 y v.2 0
```
should output
```
Interpreter version: L0
Input 2 should output 0, but got 2
```
