
#' @importFrom dplyr data_frame mutate bind_rows
#' @importMethodsFrom mrgsolve as.data.frame
NULL

prototype <- list()
prototype$reps <- 1
prototype$name <- "mrgscript simulation"
prototype[c("endpoints", "output_file")] <- NA
prototype[c("model","project")] <- NA
prototype[c("covariates","covset")] <- NA
prototype[c("period","sequence","arm")] <- NA


