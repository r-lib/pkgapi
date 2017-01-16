#' @export
print.pkgapi <- function(x, ...) {
  cat(format(x), sep = "\n")
}

#' @export
format.pkgapi <- function(x, ...) {
  c(
    format_caption("API for ", x$name, " package", level = 1),
    add_caption("Exported functions", format_exported_functions(x)),
    add_caption("S3 methods", format_s3_methods(x)),
    add_caption("Exported data", format_exported_data(x)),
    NULL
  )
}

format_caption <- function(..., level = 2) {
  c(paste0(paste(rep("#", level), collapse = ""), " ", ...))
}

add_caption <- function(caption, x) {
  if (length(x) == 0L) {
    character()
  } else {
    c("", format_caption(caption), "", x)
  }
}

format_exported_functions <- function(x) {
  exports <- Filter(Negate(is.null), x$functions[x$exports])
  unlist(unname(Map(format_export_fun, names(exports), exports)))
}

format_exported_data <- function(x) {
  exports <- Filter(Negate(is.null), x$data[x$exports])
  unlist(unname(Map(format_export_data, names(exports), exports)))
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

format_export_data <- function(name, data) {
  paste0(name, ": ", paste(class(data), collapse = ", "), " (", mode(data), "[", length(data), "])")
}
