Scripted simulation runs with `mrgsolve`
========================================

``` r
library(mrgsim)
library(mrgsolve)
library(dplyr)
```

Load a model
------------

``` r
mod <- mread("popex", modlib())

param(mod)
```

    . 
    .  Model parameters (N=4):
    .  name value . name value
    .  TVCL 1     | TVV  24   
    .  TVKA 0.5   | WT   70

Load a scripted simulation run
------------------------------

``` r
run <- load_run("inst/yaml/foo.yaml")
```

Simulate
--------

``` r
out <- sim_run(mod,run)
```

``` r
runtime
```

    .    user  system elapsed 
    .   0.499   0.014   0.515

``` r
dim(out)
```

    . [1] 109850      5

``` r
out %>%
  group_by(arm) %>% 
  filter(time==4032) %>%
  summarise(Mean = mean(DV), Min = min(DV), Max = max(DV), N=n())
```

    . # A tibble: 3 x 5
    .     arm      Mean        Min       Max     N
    .   <chr>     <dbl>      <dbl>     <dbl> <int>
    . 1  arm1  7.014079 0.11433253  78.11614   250
    . 2  arm2  3.492472 0.09303598  22.02542   250
    . 3  arm3 17.942659 0.65358606 112.13541   150

The simulation run specification
--------------------------------

      name: Simulation 1
      
      endpoints: [DV,CP,RESP]
      
      output_file: saved.RDS
      
      model: irm1
      
      project: /Users/kyleb/Rlibs/mrgsolve/models
      
      covariate:
        WT:   {distribution: rlnorm,  meanlog: 4.3 , sdlog: 0.5, by: ID, lb: 40, ub: 140}
        SEX:  {distribution: rbinomial, p: 0.5}
        AGE1: {distribution: runif, min: 21, max: 42, as: AGE}
        AGE2: {distribution: runif, min: 12, max: 27, as: AGE}
      
      covset:
        cov1: [WT,SEX,AGE1]
        cov2: [WT,SEX,AGE2]
      
      period:
        a: {amt: 100, ii: 24, addl: 83}
        b: {amt: 200, ii: 24, addl: 83}
        c: {amt: 500, ii: 24, addl: 168}
      
      sequence:
        s1: [a, b]
        s2: [a, a]
        s3: [c]
      
      sample:
        des1: {end: 4032, delta: 24, add: [0]}
      
      arm:
        arm1: {nid: 250, sequence: s1, covset: cov1}
        arm2: {nid: 250, sequence: s2, covset: cov2}
        arm3: {nid: 150, sequence: c,  covset: cov1}
      
      objects:
        - "Sigma <- diag(c(1,1))"
        - "mu <- log(c(2,20))"

Run specification for pediatric / adult pk
------------------------------------------

      name: Pediatrics and adults, same dose
      
      endpoints: [DV]
      
      output_file: saved.RDS
      
      model: popex
      
      covariate:
        WT1:   {distribution: rlnorm,  meanlog: 4.3 , sdlog: 0.5, by: ID, lb: 50, ub: 120, as: WT}
        WT2:   {distribution: rlnorm,  meanlog: 3.6 , sdlog: 0.5, by: ID, lb: 25, ub: 80,  as: WT}
      
      covset:
        cov1: [WT1]
        cov2: [WT2]
      
      period:
        dose100: {amt: 100, ii: 24, addl: 168}
      
      sample:
        des1: {end: 4032, delta: 24, add: [0]}
      
      arm:
        adult:     {nid: 250, sequence: dose100, covset: cov1}
        pediatric: {nid: 250, sequence: dose100, covset: cov2}

Run specification details
-------------------------

The run has arms

``` r
arms(run)
```

    . # A tibble: 3 x 9
    .     nid sequence covset sample   arm covsetn sequencen samplen  armn
    .   <int>    <chr>  <chr>  <chr> <chr>   <int>     <int>   <int> <int>
    . 1   250       s1   cov1   des1  arm1       1         1       1     1
    . 2   250       s2   cov2   des1  arm2       2         2       1     2
    . 3   150        c   cov1   des1  arm3       1         6       1     3

treatment sequences

``` r
sequences(run)
```

    . # A tibble: 6 x 2
    .   sequence periods
    .      <chr>   <chr>
    . 1       s1     a,b
    . 2       s2     a,a
    . 3       s3       c
    . 4        a       a
    . 5        b       b
    . 6        c       c

treatment periods

``` r
run$periods
```

    . $s1
    . Events:
    .   time cmt amt ii addl evid
    . 1    0   1 100 24   83    1
    . 2 2016   1 200 24   83    1
    . 
    . $s2
    . Events:
    .   time cmt amt ii addl evid
    . 1    0   1 100 24   83    1
    . 2 2016   1 100 24   83    1
    . 
    . $s3
    . Events:
    .   time cmt amt ii addl evid
    . 1    0   1 500 24  168    1
    . 
    . $a
    . Events:
    .   time cmt amt ii addl evid
    . 1    0   1 100 24   83    1
    . 
    . $b
    . Events:
    .   time cmt amt ii addl evid
    . 1    0   1 200 24   83    1
    . 
    . $c
    . Events:
    .   time cmt amt ii addl evid
    . 1    0   1 500 24  168    1

covariate sets

``` r
covsets(run)
```

    . $cov1
    .  Formulae                             
    .    WT~rlnorm(meanlog=4.3,sdlog=0.5)|ID
    .    SEX~rbinomial(p=0.5)               
    .    AGE~runif(min=21,max=42)           
    . 
    . $cov2
    .  Formulae                             
    .    WT~rlnorm(meanlog=4.3,sdlog=0.5)|ID
    .    SEX~rbinomial(p=0.5)               
    .    AGE~runif(min=12,max=27)

sampling times

``` r
run$designs
```

    . $des1
    . start:  0  end:    4032  delta:  24  offset: 0  min:    0   max:    4032
