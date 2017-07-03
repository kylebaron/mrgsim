
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
make_periods <- function(x) {
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
                             lb=NULL,ub=NULL,by=NULL,as=NULL,var) {
  if(!is.null(formula)) return(formula)
  if(is.null(as)) as <- var
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
    var <- paste0(as,"[",lb,",",ub,"]")
  }
  paste0(as,"~",call)
}

##' @export
make_cov_formulae <- function(x) {
  y <- add_name_list(x$covariate,where="var")
  lapply(y,FUN=do.call,what=assemble_formula)
}

##' @export
make_covsets <- function(x,cov_form=NULL) {
  if(is.null(cov_form)) cov_form <- make_cov_formulae(x)
  lapply(x$covset, function(y) {
    do.call("covset",cov_form[y])
  })
}

make_designs <- function(x) {
  lapply(x, do.call, what=mrgsolve::tgrid)
}


##' @export
get_subjects <- function(x) {

  x$arm <- add_name_list(x$arm, where="arm")
  ans <- lapply(x$arm, function(y) {
    data_frame(arm=y$arm,ID=seq_len(y$nid),
               sequence=y$sequence,covset=y$covset,sample=y$sample)
  })
  ans <- bind_rows(ans)
  ans <- dplyr::mutate(ans,
                       .covsetn = match(covset,names(x$covset)),
                       .armn  = match(arm,names(x$arm)),
                       .sequencen = match(sequence,names(x$sequence)),
                       .sample = match(sample,names(x$sample)),
                       ID=seq_len(n()))
  dplyr::select(ans,ID,.armn,.covsetn,.sequencen)
}


used_sequences <- function(x) {
  sapply(x$arm,function(y) y$sequence)
}


do_covariates <- function(x) length(x$covset) > 0

##' @export
get_arms <- function(x) {
  ans <- bind_rows(lapply(x$arm, dplyr::as_data_frame))
  ans$arm <- names(x$arm)
  return(ans)
}

##' @export
get_sequences <- function(x) {
  sequence <- names(x$sequence)
  periods <- sapply(x$sequence,FUN=paste, collapse=",")
  data_frame(sequence=sequence,periods=periods)
}

##' @export
load_run <- function(file) {

  if(!file.exists(file)) {
    stop("could not find file ", file, call.=FALSE)
  }

  x <- yaml.load_file(file)

  x$file <- file

  missing <- setdiff(names(.yaml1),names(x))

  x <- c(x,.yaml1[missing])

  x$envir <- new.env()
  rcode <- NULL
  if(!is.null(x$script)) {
    rcode <- strsplit(x$script, split="\n")[[1]]
  }
  rcode <- c(rcode,x$objects)
  if(!is.null(rcode)) {
    foo <- eval(parse(text=rcode),envir=x$envir)
  }

  x <- handle_periods(x)

  if(is.na(x$covset[1])) {
    x$covset <- list(cov0="no_cov")
    x$covariate <- c(x$covariate,list(no_cov=list(formula="NULL~mt()")))
  }

  for(i in seq_along(x$arm)) {
    if(is.null(x$arm[[i]]$nid)) {
      stop("nid is required for every arm",call.=FALSE)
    }
    # If there is no sample indicated, take the first
    if(is.null(x$arm[[i]]$sample)) {
      x$arm[[i]]$sample <- names(x$sample)[1]
    }
    # If there is no sequence, take the first
    if(is.null(x$arm[[i]]$sequence)){
      x$arm[[i]]$sequence <- names(x$sequence)[1]
    }
    if(is.null(x$arm[[i]]$covset)) {
      x$arm[[i]]$covset <- names(x$covset)[1]
    }
  }

  x$arms <- get_arms(x)

  x$sequences <- get_sequences(x)

  periods <- make_periods(x)
  idata <- get_subjects(x)

  covsets <- make_covsets(x)
  check_covsets(covsets,x)

  x$designs <- make_designs(x$sample)
  x$periods <- periods
  x$covsets <- covsets
  x$arms$covsetn <- match(x$arms$covset,names(covsets))
  x$arms$sequencen <- match(x$arms$sequence,names(periods))
  x$arms$samplen <- match(x$arms$sample,names(x$sample))

  x
}

##' @export
sim_run <- function(mod,x,join=FALSE,.Request=x$endpoints) {
  out <- vector(mode="list", length(length(x$arm)))
  ids <- get_ids(x)
  for(i in seq_along(x$arm)) {
    .arm <- slice(x$arms,i)
    ev <- x$periods[[.arm$sequence]]
    this_arm <- slice(x$arms,rep(i,.arm$nid))
    this_arm$ID <- ids[[i]]
    des <- x$designs[[.arm$sample]]
    idata <- dplyr::select(this_arm,-sample,-sequence,-covset,-arm)
    idata <- mutate_random(idata,x$covsets[[.arm$covsetn]],x$envir)
    out[[i]] <- mrgsim(mod,
                       idata=idata,
                       events=ev,
                       tgrid=des,
                       obsonly=TRUE,
                       Req=.Request,
                       carry.out="sequencen")
    out[[i]] <- as_data_frame(out[[i]])
    out[[i]] <- mutate(out[[i]],sequence=.arm$sequence)
    out[[i]] <- dplyr::select(out[[i]],ID,time,sequence,sequencen,everything())
  }
  bind_rows(out)
}


calculate_ids <- function(nid) {
  end <- cumsum(nid)
  start <- c(0,end[-length(end)])+1
  ans <- mapply(start,end,SIMPLIFY=FALSE,FUN=function(x,y) {
    seq(x,y)
  })
  names(ans) <- names(x$arm)
  ans
}

get_ids <- function(x) {
 nid <- s_pick(x$arm,"nid")
 calculate_ids(nid)
}

##' @export
get_idata <- function(x) {
  a <- get_subjects(x)
  a <- group_by(a,.covsetn)
  ungroup(do(a,apply_covset(.,x)))
}


apply_covset <- function(df,x) {
  n <- as.integer(df[1,".covsetn"])
  covset <- x$covsets[[n]]
  mutate_random(df,covset,envir=x$envir)
}
