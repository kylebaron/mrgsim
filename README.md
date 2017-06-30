
```yaml
name: Simulation 1
endpoints: [CP, RESP]
output_file: saved.RDS
model: irm1
project: /Users/kyleb/Rlibs/mrgsolve/models
covariates:
  WT:   {distribution: rlnorm,  meanlog: 4.3 , sdlog: 0.2, by: ID, lb: 40, ub: 140}
  SEX:  {distribution: rbinomial, p: 0.5}
  AGE:  {distribution: runif, min: 21, max: 42 , by: arm}
  EGFR: {call: "runif(22,34)"}
  B:    {call: "runif(22,22)"}
  A:    {formula: "a+b~rmvnorm(log(c(2,22)),Sigma)"}
covset:
  cov1: [WT,SEX,AGE,A]
  cov2: [SEX,AGE,WT]
period:
  a: {amt: 100, ii: 12, addl: 7}
  b: {amt: 200, ii: 12, addl: 3}
  c: {amt: 500, ii: 24, addl: 1}
sequence:
  s1: [a, b, c]
  s2: [b, a, a]
arm:
  a1: {sequence: s1, nid: 300, covset: cov1}
  a2: {sequence: s2, nid: 200, covset: cov2}
  a3: {nid: 500, sequence: a}
```
