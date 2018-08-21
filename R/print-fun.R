format_exported_functions <- function(x) {
  exports <- Filter(Negate(is.null), x$functions[x$exports])
  unlist(unname(Map(format_export_fun, names(exports), exports)))
}

format_own_s3_methods <- function(x) {
  format_s3_methods(x, own = TRUE)
}

format_foreign_s3_methods <- function(x) {
  format_s3_methods(x, own = FALSE)
}

format_s3_methods <- function(x, own) {
  method_names <- gsub("^([^.]*)[.].*$", "\\1", x$s3_methods)
  method_is_own <- method_names %in% x$exports
  s3_methods <- x$functions[ x$s3_methods[method_is_own == own] ]
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
    paste0(tick_if_needed(names(args)), vapply(args, format_default, character(1L)), collapse = ", ")
  }
}

format_default <- function(lang) {
  if (identical(as.character(lang), "")) {
    ""
  } else {
    paste0(" = ", paste(deparse(lang, width.cutoff = 500, backtick = TRUE), collapse = ""))
  }
}
