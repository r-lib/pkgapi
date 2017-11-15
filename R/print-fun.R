format_exported_functions <- function(x) {
  exports <- Filter(Negate(is.null), x$functions[x$exports])
  unlist(unname(Map(format_export_fun, names(exports), exports)))
}

format_s3_methods <- function(x) {
  s3_methods <- x$functions[x$s3_methods]
  s3_methods <- Filter(Negate(is.null), s3_methods)
  unlist(unname(Map(format_export_fun, names(s3_methods), s3_methods)))
}

format_export_fun <- function(name, fun) {
  paste0(name, "(", format_args(formals(fun)), ")")
}

format_args <- function(args) {
  if (length(args) == 0L) {
    ""
  } else {
    paste0(names(args), vapply(args, format_default, character(1L)), collapse = ", ")
  }
}

format_default <- function(lang) {
  if (identical(as.character(lang), "")) {
    ""
  } else {
    paste0(" = ", paste(deparse(lang, width.cutoff = 500), collapse = ""))
  }
}
