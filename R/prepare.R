
#' Get package data
#'
#' Parse and evaluate code from a package, and store it in an
#' environment, so that we can access it later without parsing it again.
#'
#' We need to evaluate code to get the functions, which might require
#' loading some packages, so we perform all this in a separate R process,
#' using the `callr` package.
#'
#' While we are at it, we also check the imports of the package,
#' and return where each function was imported from.
#'
#' @param path package root
#' @param targets character vector, function call targets to find
#' @return List with components `name`, `version`, `targets`, `functions`,
#'   `exports`, `imports`.
#'   `name` is the package name.
#'   `version` is the vesion of the package.
#'   `targets` is a character vector, the names of the environments where
#'   each target was found.
#'   `functions` is a list of functions, with source references.
#'   `exports` is a character vector of exported objects.
#'   `imports` is a named list of environment names, one for each import.
#'
#' @keywords internal
#' @importFrom callr r_vanilla

prepare_package <- function(path, targets) {

  r_vanilla(
    function(path, targets) {
      options(keep.source = TRUE)
      pkg <- pkgload::as.package(path)
      pkgload::load_all(pkg, export_all = FALSE)

      all_names <- ls(pkgload::ns_env(pkg))
      objects <- mget(all_names, pkgload::ns_env(pkg))

      functions <- Filter(is.function, objects)

      imports <- eapply(
        pkgload::imports_env(path),
        function(x) environmentName(environment(x))
      )

      target_funcs <- mget(
        targets,
        envir = pkgload::ns_env(pkg),
        mode = "function",
        inherits = TRUE,
        ifnotfound = NA_character_
      )

      target_envs <- lapply(
        target_funcs,
        function(x) {
          if (identical(x, NA_character_)) {
            x
          } else if (is.primitive(x)) {
            "base"
          } else {
            environmentName(environment(x))
          }
        }
      )

      list(
        name = pkg$package,
        version = pkg$version,
        targets = target_envs,
        functions = functions,
        exports = ls(pkgload::pkg_env(pkg)),
        imports = imports
      )
    },
    libpath = .libPaths(),
    repos = getOption("repos"),
    args = list(path = path, targets = targets)
  )
}
