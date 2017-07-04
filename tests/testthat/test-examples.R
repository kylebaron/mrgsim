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

context("Testing example specifications")

exdir <- function() {
  where <- system.file(package="mrgsim")
  file.path(where,"example")
}

files <- list.files(exdir(),full.names=TRUE)

mod <- mrgsolve:::house()

test_that("test example 1", {
  run <- load_run(files[1])
  out <- sim_run(mod,run)
  expect_is(out,"data.frame")
  
})

test_that("test example 2", {
  run <- load_run(files[1])
  out <- sim_run(mod,run)
  expect_is(out,"data.frame")
  
})
