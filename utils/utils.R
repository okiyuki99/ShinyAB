save_to <- function(x, v, fun) {
  var <- substitute(v)
  eval(bquote(.(var) <- .(fun(x))), envir = globalenv())
  x
}