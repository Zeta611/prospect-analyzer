# Ln
See the [documentation](ln.pdf) for syntax and semantics.

## Usage
To run, execute
```sh
./l <test.l
```
after `make`.

## Sample code
```
# lang 1
# 1 1
let v (x, 3) case [] (y, z) if v.1 y v.2 0
```
should output
```
Type checker version: L1
| []: (ι, τ6), O: ι
| []: (τ5, τ6), O: ι
| []: ι, O: ι
```
