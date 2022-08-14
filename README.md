# Ln
See the [documentation](ln.pdf) for the target language syntax and semantics.

The shape analyzer determines which paths are "shape-safe", and the value analyzer checks if each path can indeed yield the provided output value.
The shape analysis uses a non-deterministic Damas-Milner type inference, since the target language is not statically typed.
The value analysis utilizes the Z3 theorem prover to prune branches as early as possible, as well as to unify with the output value.

## Usage
To run, execute
```sh
./_build/default/bin/main.exe -all test.l
```
after `dune build`.

When the flag `-all` is omitted, only shape analysis is done.

## Sample code
```
# lang 1
# (0, 0) 1
# (1, 1) 0
let h []
let a h.1
let b h.2
if (a + b)
  if (a + b) (a + a + 1) b
  if (a + b) b (a + a + 1)
```
should output
```
Shape & value analyzer. tests/test_7.l (L1)
Sample: ((0, 0), 1)
[let x = [([0 : ℓ3], [0 : ℓ4]) : ℓ2] in [let h = [[] : ℓ6] in [let a = [[h : ℓ9].1 : ℓ8] in [let b = [[h : ℓ12].2 : ℓ11] in [if [[a : ℓ15] + [b : ℓ16] : ℓ14] [if [[a : ℓ19] + [b : ℓ20] : ℓ18] [[[a : ℓ23] + [a : ℓ24] : ℓ22] + [1 : ℓ25] : ℓ21] [b : ℓ26] : ℓ17] [if [[a : ℓ29] + [b : ℓ30] : ℓ28] [b : ℓ31] [[[a : ℓ34] + [a : ℓ35] : ℓ33] + [1 : ℓ36] : ℓ32] : ℓ27] : ℓ13] : ℓ10] : ℓ7] : ℓ5] : ℓ1]
| []: (ι, ι), O: ι, Trace: let . = (.) in (let . = (.) in (let . = (.) in (if (. + .) ≠ 0 if (. + .) ≠ 0 . + . + .))); ℓ1-ℓ2-ℓ3-ℓ4-ℓ5-ℓ6-ℓ7-ℓ8-ℓ9-ℓ10-ℓ11-ℓ12-ℓ13-ℓ14-ℓ15-ℓ16-ℓ17-ℓ18-ℓ19-ℓ20-ℓ21-ℓ22-ℓ23-ℓ24-ℓ25
| Eval: 1 + 2[2]
[let x = [([0 : ℓ3], [0 : ℓ4]) : ℓ2] in [let h = [[] : ℓ6] in [let a = [[h : ℓ9].1 : ℓ8] in [let b = [[h : ℓ12].2 : ℓ11] in [if [[a : ℓ15] + [b : ℓ16] : ℓ14] [if [[a : ℓ19] + [b : ℓ20] : ℓ18] [[[a : ℓ23] + [a : ℓ24] : ℓ22] + [1 : ℓ25] : ℓ21] [b : ℓ26] : ℓ17] [if [[a : ℓ29] + [b : ℓ30] : ℓ28] [b : ℓ31] [[[a : ℓ34] + [a : ℓ35] : ℓ33] + [1 : ℓ36] : ℓ32] : ℓ27] : ℓ13] : ℓ10] : ℓ7] : ℓ5] : ℓ1]
| []: (ι, ι), O: ι, Trace: let . = (.) in (let . = (.) in (let . = (.) in (if (. + .) ≠ 0 if (. + .) = 0 .))); ℓ1-ℓ2-ℓ3-ℓ4-ℓ5-ℓ6-ℓ7-ℓ8-ℓ9-ℓ10-ℓ11-ℓ12-ℓ13-ℓ14-ℓ15-ℓ16-ℓ17-ℓ18-ℓ19-ℓ20-ℓ26
| Falsy value found when guide_path expected a truthy value
[let x = [([0 : ℓ3], [0 : ℓ4]) : ℓ2] in [let h = [[] : ℓ6] in [let a = [[h : ℓ9].1 : ℓ8] in [let b = [[h : ℓ12].2 : ℓ11] in [if [[a : ℓ15] + [b : ℓ16] : ℓ14] [if [[a : ℓ19] + [b : ℓ20] : ℓ18] [[[a : ℓ23] + [a : ℓ24] : ℓ22] + [1 : ℓ25] : ℓ21] [b : ℓ26] : ℓ17] [if [[a : ℓ29] + [b : ℓ30] : ℓ28] [b : ℓ31] [[[a : ℓ34] + [a : ℓ35] : ℓ33] + [1 : ℓ36] : ℓ32] : ℓ27] : ℓ13] : ℓ10] : ℓ7] : ℓ5] : ℓ1]
| []: (ι, ι), O: ι, Trace: let . = (.) in (let . = (.) in (let . = (.) in (if (. + .) = 0 if (. + .) ≠ 0 .))); ℓ1-ℓ2-ℓ3-ℓ4-ℓ5-ℓ6-ℓ7-ℓ8-ℓ9-ℓ10-ℓ11-ℓ12-ℓ13-ℓ14-ℓ15-ℓ16-ℓ27-ℓ28-ℓ29-ℓ30-ℓ31
| Falsy value found when guide_path expected a truthy value
[let x = [([0 : ℓ3], [0 : ℓ4]) : ℓ2] in [let h = [[] : ℓ6] in [let a = [[h : ℓ9].1 : ℓ8] in [let b = [[h : ℓ12].2 : ℓ11] in [if [[a : ℓ15] + [b : ℓ16] : ℓ14] [if [[a : ℓ19] + [b : ℓ20] : ℓ18] [[[a : ℓ23] + [a : ℓ24] : ℓ22] + [1 : ℓ25] : ℓ21] [b : ℓ26] : ℓ17] [if [[a : ℓ29] + [b : ℓ30] : ℓ28] [b : ℓ31] [[[a : ℓ34] + [a : ℓ35] : ℓ33] + [1 : ℓ36] : ℓ32] : ℓ27] : ℓ13] : ℓ10] : ℓ7] : ℓ5] : ℓ1]
| []: (ι, ι), O: ι, Trace: let . = (.) in (let . = (.) in (let . = (.) in (if (. + .) = 0 if (. + .) = 0 . + . + .))); ℓ1-ℓ2-ℓ3-ℓ4-ℓ5-ℓ6-ℓ7-ℓ8-ℓ9-ℓ10-ℓ11-ℓ12-ℓ13-ℓ14-ℓ15-ℓ16-ℓ27-ℓ28-ℓ29-ℓ30-ℓ32-ℓ33-ℓ34-ℓ35-ℓ36
| Eval: 1 + 2[2]
Sample: ((1, 1), 0)
[let x = [([1 : ℓ3], [1 : ℓ4]) : ℓ2] in [let h = [[] : ℓ6] in [let a = [[h : ℓ9].1 : ℓ8] in [let b = [[h : ℓ12].2 : ℓ11] in [if [[a : ℓ15] + [b : ℓ16] : ℓ14] [if [[a : ℓ19] + [b : ℓ20] : ℓ18] [[[a : ℓ23] + [a : ℓ24] : ℓ22] + [1 : ℓ25] : ℓ21] [b : ℓ26] : ℓ17] [if [[a : ℓ29] + [b : ℓ30] : ℓ28] [b : ℓ31] [[[a : ℓ34] + [a : ℓ35] : ℓ33] + [1 : ℓ36] : ℓ32] : ℓ27] : ℓ13] : ℓ10] : ℓ7] : ℓ5] : ℓ1]
| []: (ι, ι), O: ι, Trace: let . = (.) in (let . = (.) in (let . = (.) in (if (. + .) ≠ 0 if (. + .) ≠ 0 . + . + .))); ℓ1-ℓ2-ℓ3-ℓ4-ℓ5-ℓ6-ℓ7-ℓ8-ℓ9-ℓ10-ℓ11-ℓ12-ℓ13-ℓ14-ℓ15-ℓ16-ℓ17-ℓ18-ℓ19-ℓ20-ℓ21-ℓ22-ℓ23-ℓ24-ℓ25
| Eval: 1 + 2[2] failed to unify with the output
[let x = [([1 : ℓ3], [1 : ℓ4]) : ℓ2] in [let h = [[] : ℓ6] in [let a = [[h : ℓ9].1 : ℓ8] in [let b = [[h : ℓ12].2 : ℓ11] in [if [[a : ℓ15] + [b : ℓ16] : ℓ14] [if [[a : ℓ19] + [b : ℓ20] : ℓ18] [[[a : ℓ23] + [a : ℓ24] : ℓ22] + [1 : ℓ25] : ℓ21] [b : ℓ26] : ℓ17] [if [[a : ℓ29] + [b : ℓ30] : ℓ28] [b : ℓ31] [[[a : ℓ34] + [a : ℓ35] : ℓ33] + [1 : ℓ36] : ℓ32] : ℓ27] : ℓ13] : ℓ10] : ℓ7] : ℓ5] : ℓ1]
| []: (ι, ι), O: ι, Trace: let . = (.) in (let . = (.) in (let . = (.) in (if (. + .) ≠ 0 if (. + .) = 0 .))); ℓ1-ℓ2-ℓ3-ℓ4-ℓ5-ℓ6-ℓ7-ℓ8-ℓ9-ℓ10-ℓ11-ℓ12-ℓ13-ℓ14-ℓ15-ℓ16-ℓ17-ℓ18-ℓ19-ℓ20-ℓ26
| Falsy value found when guide_path expected a truthy value
[let x = [([1 : ℓ3], [1 : ℓ4]) : ℓ2] in [let h = [[] : ℓ6] in [let a = [[h : ℓ9].1 : ℓ8] in [let b = [[h : ℓ12].2 : ℓ11] in [if [[a : ℓ15] + [b : ℓ16] : ℓ14] [if [[a : ℓ19] + [b : ℓ20] : ℓ18] [[[a : ℓ23] + [a : ℓ24] : ℓ22] + [1 : ℓ25] : ℓ21] [b : ℓ26] : ℓ17] [if [[a : ℓ29] + [b : ℓ30] : ℓ28] [b : ℓ31] [[[a : ℓ34] + [a : ℓ35] : ℓ33] + [1 : ℓ36] : ℓ32] : ℓ27] : ℓ13] : ℓ10] : ℓ7] : ℓ5] : ℓ1]
| []: (ι, ι), O: ι, Trace: let . = (.) in (let . = (.) in (let . = (.) in (if (. + .) = 0 if (. + .) ≠ 0 .))); ℓ1-ℓ2-ℓ3-ℓ4-ℓ5-ℓ6-ℓ7-ℓ8-ℓ9-ℓ10-ℓ11-ℓ12-ℓ13-ℓ14-ℓ15-ℓ16-ℓ27-ℓ28-ℓ29-ℓ30-ℓ31
| Falsy value found when guide_path expected a truthy value
[let x = [([1 : ℓ3], [1 : ℓ4]) : ℓ2] in [let h = [[] : ℓ6] in [let a = [[h : ℓ9].1 : ℓ8] in [let b = [[h : ℓ12].2 : ℓ11] in [if [[a : ℓ15] + [b : ℓ16] : ℓ14] [if [[a : ℓ19] + [b : ℓ20] : ℓ18] [[[a : ℓ23] + [a : ℓ24] : ℓ22] + [1 : ℓ25] : ℓ21] [b : ℓ26] : ℓ17] [if [[a : ℓ29] + [b : ℓ30] : ℓ28] [b : ℓ31] [[[a : ℓ34] + [a : ℓ35] : ℓ33] + [1 : ℓ36] : ℓ32] : ℓ27] : ℓ13] : ℓ10] : ℓ7] : ℓ5] : ℓ1]
| []: (ι, ι), O: ι, Trace: let . = (.) in (let . = (.) in (let . = (.) in (if (. + .) = 0 if (. + .) = 0 . + . + .))); ℓ1-ℓ2-ℓ3-ℓ4-ℓ5-ℓ6-ℓ7-ℓ8-ℓ9-ℓ10-ℓ11-ℓ12-ℓ13-ℓ14-ℓ15-ℓ16-ℓ27-ℓ28-ℓ29-ℓ30-ℓ32-ℓ33-ℓ34-ℓ35-ℓ36
| Eval: 1 + 2[2] failed to unify with the output
```
The output is xterm-256 colorized:
![Capture](./capture.png)
