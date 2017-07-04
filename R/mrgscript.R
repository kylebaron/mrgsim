

##' Accessor functions.
##'
##' @param x sim object
##'
##' @rdname access
##' @export
arms <- function(x) return(x$arms)

##' @rdname access
##' @export
sequences <- function(x) return(x$sequences)

##' @rdname access
##' @export
covsets <- function(x) return(x$covsets)

# handle all periods in the runobject
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

# handle a single period
handle_period <- function(x) do.call("ev", x)

then_ev <- function(df,x) {
  if(nrow(df) <=1) return(df)
  tim <- sapply(x$period,FUN = function(y) exists("time", y))
  for(i in seq_along(rownames(df))[-1]) {
    j <- i-1
    if(df[j,"addl"]==0 ) {
      df[i,"time"] <- df[j,"time"]
    }
    df[i,"time"] <- df[j,"time"] + df[j,"ii"] * df[j,"addl"] + df[j,"ii"]
  }
  df
}

na2zero <- function(x) {
 x[is.na(x)] <- 0
 x
}
make_periods <- function(x) {
  ev <- vector(mode="list", length=length(x$sequence))
  names(ev) <- names(x$sequence)
  for(i in seq_along(x$sequence)) {
    sn <- x$sequence[[i]]
    ss <- x$period_ev[sn]
    ss <- lapply(ss,as.data.frame)
    ss <- bind_rows(ss)
    ss[] <- lapply(ss[],na2zero)
    ev[[i]] <- then_ev(ss,x)
  }
  lapply(ev,as.ev)
}

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

make_cov_formulae <- function(x) {
  y <- add_name_list(x$covariate,where="var")
  lapply(y,FUN=do.call,what=assemble_formula)
}

make_covsets <- function(x,cov_form=NULL) {
  if(is.null(cov_form)) cov_form <- make_cov_formulae(x)
  lapply(x$covset, function(y) {
    do.call("covset",cov_form[y])
  })
}

make_designs <- function(x) {
  lapply(x, do.call, what=mrgsolve::tgrid)
}

get_arms <- function(x) {
  ans <- bind_rows(lapply(x$arm, dplyr::as_data_frame))
  ans$arm <- names(x$arm)
  return(ans)
}

get_sequences <- function(x) {
  sequence <- names(x$sequence)
  periods <- sapply(x$sequence,FUN=paste, collapse=",")
  data_frame(sequence=sequence,periods=periods)
}

calculate_ids <- function(nid) {
  end <- cumsum(nid)
  start <- c(0,end[-length(end)])+1
  ans <- mapply(start,end,SIMPLIFY=FALSE,FUN=function(x,y) {
    seq(x,y)
  })
  ans
}

get_ids <- function(x) {
 nid <- s_pick(x$arm,"nid")
 setNames(calculate_ids(nid),names(x$arm))
}

##' Simulate from a scripted run.
##'
##' @param mod a model object
##' @param x the simrun object
##' @param .Request requested endpoints
##' @details
##' The object passed as \code{x} is the
##' result of a call to \code{\link{load_run}}.
##'
##' @export
sim_run <- function(mod,x,.Request=x$endpoints) {
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
                       carry.out="armn")
    out[[i]] <- as_data_frame(out[[i]])
    out[[i]] <- mutate(out[[i]],arm=.arm$arm)
    out[[i]] <- dplyr::select(out[[i]],ID,time,arm,armn,everything())
  }
  bind_rows(out)
}


##' Load a simulation run from file.
##'
##' @param file the name of a file containing \code{yaml} run specification
##'
##' @export
load_run <- function(file,text=NULL) {

  if(is.character(text)) {
    file <- tempfile()
    writeLines(con=file,text)
  }
  
  if(!file.exists(file)) {
    stop("could not find file ", file, call.=FALSE)
  }

  x <- yaml.load_file(file)

  x$file <- file
  
  missing <- setdiff(names(.yaml1),names(x))

  x <- c(x,.yaml1[missing])
  
  if(is.null(x$endpoints)) {
    stop("please specify endpoints to monitor",call.=FALSE)
  }
  if(is.null(x$sample)) {
    stop("please specify sampling times",call.=FALSE) 
  }

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

  if(is.null(x$covariate)) {
    x$covariate <- list(no_cov=list(formula="NULL~mt()")) 
  }
  if(is.null(x$covset)) {
    x$covset <- list(cov0="no_cov")
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

  covsets <- make_covsets(x)

  check_covsets(covsets,x)

  if(!all(x$arms$covset %in% names(covsets))) {
    stop("covset not found",call.=FALSE) 
  }
  if(!all(x$arms$sequence %in% names(x$sequence))) {
    stop("sequence not found",call.=FALSE) 
  }
  
  x$designs <- make_designs(x$sample)
  x$periods <- periods
  x$covsets <- covsets
  x$arms$covsetn <- match(x$arms$covset,names(covsets))
  x$arms$sequencen <- match(x$arms$sequence,names(periods))
  x$arms$samplen <- match(x$arms$sample,names(x$sample))
  x$arms$armn <- match(x$arms$arm,names(x$arm))
  x
}

