
map_package <- function(path = ".") {

  ## Need to parse expressions for calls first

  calls <- function_calls(path)

  ## Then we eval the code, to see which expression creates which function.
  ## This allows finding the function definitions. And while we are at it,
  ## we also find the targets of the calls, by simply evaluating the
  ## the names of the called functions, in the package environment.

  prep <- prepare_package(path, calls$str)

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
