
#' Parse all files of a package
#'
#' This includes all (unevaluated) code of the package. We use it
#' to find all function calls.
#'
#' @param path package root
#' @return List of expressions, one for each file, names are full
#'   file names.
#'
#' @keywords internal
#'
#' @importFrom tools list_files_with_type
#' @importFrom withr with_collate

parse_expressions <- function(path) {

   path_r <- file.path(path, "R")
   r_files <- with_collate(
     "C",
     list_files_with_type(path_r, "code", full.names = TRUE)
   )

   structure(
     lapply(r_files, parse, keep.source = TRUE),
     names = r_files
   )
}
