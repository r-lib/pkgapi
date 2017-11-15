format_reexported_functions <- function(x) {
  reexports <- Filter(Negate(is.null), x$imports[x$exports])
  unlist(unname(Map(format_reexport_fun, reexports, names(reexports))))
}

format_reexport_fun <- function(name, fun) {
  paste0(name, "::", fun)
}
