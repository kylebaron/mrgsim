
##' @export
handle_period <- function(x) do.call("ev", x)

##' @export
handle_periods <- function(x) {
  if(is.na(x$sequence[1])) {
    x$sequence <- names(x$period)
    names(x$sequence) <- x$sequence
  } else {
    to_aug <- setdiff(names(x$period),names(x$sequence))
    x$sequence <- c(x$sequence,setNames(as.list(to_aug),to_aug))
  }
  x$period_ev <- lapply(x$period, handle_period)
  x
}

##' @export
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


# Take in the workflow object and return events
# named by sequence name
##' @export
make_ev_list <- function(x) {
  ev <- vector(mode="list", length=length(x$sequence))
  names(ev) <- names(x$sequence)
  for(i in seq_along(x$sequence)) {
    sn <- x$sequence[[i]]
    ss <- x$period_ev[sn]
    ss <- lapply(ss,as.data.frame)
    ss <- bind_rows(ss)
    ev[[i]] <- then_ev(ss,x)
  }
  lapply(ev,as.ev)
}

##' @export
assemble_formula <- function(...,distribution,call=NULL,formula=NULL,
                             lb=NULL,ub=NULL,by=NULL,var) {
  if(!is.null(formula)) return(formula)
  if(is.null(call)) {
    args <- list(...)
    arg_names <- names(args)
    arg_values <- unlist(args,use.names=FALSE)
    args <- paste0(arg_names,"=",arg_values)
    args <- paste(args,collapse=",")
    call <- paste0(distribution,"(",args,")")
    if(is.character(by)) call <- paste0(call,"|",by)
  }
  if(!all(is.null(c(lb,ub)))) {
    var <- paste0(var,"[",lb,",",ub,"]")
  }
  paste0(var,"~",call)
}

##' @export
make_cov_formulae <- function(x) {
  y <- add_name_list(x$covariates,where="var")
  lapply(y,FUN=do.call,what=assemble_formula)
}

##' @export
make_covsets <- function(x,cov_form=NULL) {
  if(is.null(cov_form)) cov_form <- make_cov_formulae(x)
  lapply(x$covset, function(y) {
    do.call("covset",cov_form[y])
  })
}


##' @export
get_subjects <- function(x) {
  x <- add_name_list(x$arm,where="arm")
  ans <- lapply(x, function(y) {
    period <- y$period
    if(is.null(period)) period <- y$sequence
    data_frame(arm=y$arm,ID=seq_len(y$nid),sequence=y$sequence,period=period)
  })
  ans <- bind_rows(ans)
  ans <- dplyr::mutate(ans,armn  = match(arm,unique(arm)),ID=seq_len(n()))
  dplyr::select(ans,armn,arm,ID,everything())
}


used_sequences <- function(x) {
  sapply(x$arm,function(y) y$sequence)
}
