
expand_events <- function(x, id) {
  x <- .Call("mrgsolve_EXPAND_EVENTS", 0, data.matrix(x), id,
             PACKAGE="mrgsolve")
  as.data.frame(x)
}
##' @export
add_name_list <- function(x,where="name") {
  mapply(x,names(x),FUN=function(a,b) {
    a[[where]] <- b
    a
  })
}


##' @export
Eval <- function(x) {
  eval(parse(text=x))
}

