format_exported_data <- function(x) {
  exports <- Filter(Negate(is.null), x$data[x$exports])
  unlist(unname(Map(format_export_data, names(exports), exports)))
}

format_export_data <- function(name, data) {
  paste0(name, ": ", paste(class(data), collapse = ", "), " (", mode(data), "[", length(data), "])")
}
