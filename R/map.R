
#' Create the function call map of an R package
#'
#' Find all functions and function calls in a package. This requires the
#' loading of the package (in another R session, so the current R session
#' is unaffected by this), which means that all depended and imported
#' packages must be available as well.
#'
#' @param path path to an R package root
#' @return List with elements:
#'   * `name`: package name,
#'   * `exports`: character vector of exported funtions and operators.
#'   * `defs`: data frame of function definitions.
#'   * `calls`: data frame of function calls.
#'
#'   Columns in `defs`:
#'   * `name`: function name,
#'   * `file`: file path where it was defined, relative to the package root.
#'   * `line1`: line of start position of function definition.
#'   * `col1`: column of start position.
#'   * `line2`: line of end position.
#'   * `col2`: column of end position.
#'   * `exported`: whether the function is exported or not.
#'
#'   Columns in `calls`:
#'   * `file`: file path of the call, relative to the package root.
#'   * `from`: name of calling function, or empty string if called from
#'     outside of a function.
#'   * `to`: called function, in the `package::function` form.
#'   * `type`: call type, regular functions calls are `call`.
#'   * `line1`: line of the beginning of the call.
#'   * `col1`: column of the beginning of the call.
#'   * `line1`: line of the end of the call.
#'   * `col2`: column of the beginning of the call.
#'   * `str`: the actual code, text of the function call, usually just the
#'     called function's name.
#'
#' @export

map_package <- function(path = ".") {

  ## Need to parse expressions for calls first

  calls <- function_calls(path)

  ## Then we eval the code, to see which expression creates which function.
  ## This allows finding the function definitions. And while we are at it,
  ## we also find the targets of the calls, by simply evaluating the
  ## the names of the called functions, in the package environment.

  prep <- extract_api(path, calls$str)

  defs <- function_defs(prep$functions, prep$exports)
  calls$to <- ifelse(
    calls$to != "",
    calls$to,
    paste0(prep$targets, "::", calls$str)
  )

  ## Now we need to find the caller functions as well
  for (i in seq_along(calls$from)) {
    calls$from[i] <- find_caller(calls, i, defs)
  }

  ## Ready
  list(
    name = prep$name,
    exports = prep$exports,
    defs = defs,
    calls = calls
  )
}
