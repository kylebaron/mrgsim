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




#' @importFrom dmutate mutate_random covset rbinomial rmvnorm rlmvnorm
#' @importFrom dplyr data_frame mutate bind_rows left_join as_data_frame 
#' @importFrom dplyr slice everything filter
#' @importMethodsFrom mrgsolve as.data.frame
#' @importFrom mrgsolve assign_ev ev tgrid as.ev mrgsim
#' @importFrom yaml yaml.load_file yaml.load
#' @importFrom stats setNames
#' @importFrom mrgsolvetk ev_seq
NULL

.yaml1 <- list()
.yaml1$reps <- 1
.yaml1$name <- "mrgscript simulation"
.yaml1[c("endpoints", "output_file")] <- as.character(NA)
.yaml1[c("model","project")] <- NA
.yaml1[c("period","sequence","arm")] <- as.character(NA)




globalVariables(c("time","ID","arm", "armn","x","evid"))

