ns_tags <- c('api')

#' Roclet: make API file.
#'
#' This roclet automates the production of an `API` file that describes
#' the exported interface of a package.
#'
#' @export
#' @api
api_roclet <- function() {
  roxygen2::roclet("api")
}

#' @export
roclet_process.roclet_api <- function(x, parsed, base_path,
                                      global_options = list()) {
  format(extract_api(base_path))
}

#' @export
roclet_tags.roclet_api <- function(x) {
  list(
    api = roxygen2::tag_toggle
  )
}

#' @export
roclet_output.roclet_api <- function(x, results, base_path, ...) {
  file_name <- "API"
  API <- file.path(base_path, file_name)

  # FIXME: Add marker that indicates if this is "our" file
  # FIXME: write_if_different()
  writeLines(results, API)

  usethis::use_build_ignore(file_name)

  API
}

#' @export
roclet_clean.roclet_api <- function(x, base_path) {
  # FIXME: Check if this is "our" file
  API <- file.path(base_path, "API")
  unlink(API)
}
