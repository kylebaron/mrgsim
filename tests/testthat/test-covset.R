# Copyright (C) 2013 - 2017  Metrum Research Group, LLC
#
# This file is part of mrgsim
#
# mrgsim is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# mrgsim is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with mrgsim.  If not, see <http://www.gnu.org/licenses/>.

library(testthat)
library(mrgsim)
library(dplyr)
Sys.setenv(R_TESTS="")


context("Testing covset functions")

test_that("covariate formula is properly assembled", {
  x <- list(distribution="rnorm", mean=2, sd=1,var="y",by="FOO")
  y <- do.call(mrgsim:::assemble_formula,x)
  expect_identical(y,"y~rnorm(mean=2,sd=1)|FOO")
  
})

test_that("covariate call is properly handled", {
  x <- list(call="runif(0,1)", var="FOO")
  y <- do.call(mrgsim:::assemble_formula,x)
  expect_identical(y,"FOO~runif(0,1)")
  
})

test_that("covariate formula is properly handled", {
  x <- list(formula="z~rbinomial(0.2)")
  y <- do.call(mrgsim:::assemble_formula,x)
  expect_identical(y,"z~rbinomial(0.2)")
  
})


