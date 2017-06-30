library(mrgsolve)
#
# foo <- parseTOML("foo.toml")
#
# foo$period
#
library(dplyr)
library(yaml)
x <- yaml.load_file("foo.yaml")

get_subjects <- function(x) {
  lapply(x$arm, FUN = function(y) y$nid)
}
total_subjects <- function(x) sum(unlist(x))

subjects <- get_subjects(x)
total_subjects(subjects)

handle_period <- function(x) do.call("ev", x)
handle_periods <- function(x) {
  x$period_ev <- lapply(x$period, handle_period)
  x
}

x <- handle_periods(x)

then_ev <- function(df,x) {
  if(nrow(df) <=1) return(df)
  tim <- sapply(x$period,FUN = function(y) exists("time", y))
  for(i in seq_along(rownames(df))[-1]) {
    j <- i-1
    if(df[j,"addl"]==0) next
    df[i,"time"] <- df[j,"time"] + df[j,"ii"] * df[j,"addl"] + df[j,"ii"]
  }
  df
}

expand_events <- function(x, id) {
  x <- .Call("mrgsolve_EXPAND_EVENTS", 0, data.matrix(x), id,
             PACKAGE="mrgsolve")
  as.data.frame(x)
}


ev <- vector(mode="list", length=length(x$sequence))
names(ev) <- names(x$sequence)
for(i in seq_along(x$sequence)) {
  sn <- x$sequence[[i]]
  ss <- x$period_ev[sn]
  ss <- lapply(ss,as.data.frame)
  ss <- bind_rows(ss)
  ev[[i]] <- then_ev(ss,x)
}


expand_events(ev[[2]], 1:3)



df$ID <- 1
mod <- mread("pk1cmt", modlib())

mod %>% data_set(df) %>% mrgsim(end=240) %>% plot



