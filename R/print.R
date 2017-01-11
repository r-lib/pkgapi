#' @export
print.pkgapi <- function(x, ...) {
  cat(format(x), sep = "\n")
}

#' @export
format.pkgapi <- function(x, ...) {
  c(
    format_caption("API for ", x$name, " package", level = 1),
    add_caption("Exports", format_exports(x)),
    add_caption("S3 methods", format_s3_methods(x))
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

format_exports <- function(x) {
  exports <- x$functions[x$exports]
  unlist(unname(Map(format_export, names(exports), exports)))
}

format_s3_methods <- function(x) {
  s3_methods <- x$functions[x$s3_methods]
  s3_methods <- Filter(Negate(is.null), s3_methods)
  unlist(unname(Map(format_export, names(s3_methods), s3_methods)))
}

format_export <- function(name, value) {
  if (is.function(value)) {
    format_export_fun(name, value)
  } else {
    format_export_data(name, value)
  }
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
  warning("NYI: data for ", name)
}
