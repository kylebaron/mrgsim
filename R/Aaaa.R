
#' @importFrom dmutate mutate_random
#' @importFrom dplyr data_frame mutate bind_rows left_join as_data_frame
#' @importMethodsFrom mrgsolve as.data.frame
#' @importFrom mrgsolve assign_ev ev tgrid
#' @importFrom yaml yaml.load_file yaml.load
NULL

.yaml1 <- list()
.yaml1$reps <- 1
.yaml1$name <- "mrgscript simulation"
.yaml1[c("endpoints", "output_file")] <- as.character(NA)
.yaml1[c("model","project")] <- NA
.yaml1[c("period","sequence","arm")] <- as.character(NA)
.yaml1$sample <- NA
.yaml1$covariate <- NA
.yaml1$covset <- NA
