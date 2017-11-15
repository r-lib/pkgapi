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
    add_caption("Reexported objects", format_reexported_functions(x)),
    NULL
  )
}

add_caption <- function(caption, x) {
  if (length(x) == 0L) {
    character()
  } else {
    c("", format_caption(caption), "", x)
  }
}


format_caption <- function(..., level = 2) {
  c(paste0(paste(rep("#", level), collapse = ""), " ", ...))
}
