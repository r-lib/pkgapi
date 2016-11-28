
#' Table of function definitions
#'
#' @param path path to package root
#' @return Data frame with columns: `name`, `file`, `line`, `col`.
#'
#' @keywords internal

function_defs <- function(funcs, exports) {

  locs <- lapply(funcs, find_function_location)

  res <- data.frame(
    stringsAsFactors = FALSE,
    name  = names(funcs),
    file  = vapply(locs, "[[", "", "file"),
    line1 = vapply(locs, "[[", 1L, "line1"),
    col1  = vapply(locs, "[[", 1L, "col1"),
    line2 = vapply(locs, "[[", 1L, "line2"),
    col2  = vapply(locs, "[[", 1L, "col2"),
    exported = names(funcs) %in% exports
  )

  row.names(res) <- NULL

  res
}

#' @importFrom utils getSrcLocation getParseData getSrcFilename getSrcref

find_function_location <- function(func) {

  ## This might happen in tricky cases, e.g. memoised functions via the
  ## memoise packag
  if (is.null(getSrcref(func))) {
    return(list(file = NA_character_,
                line1 = NA_integer_, col1 = NA_integer_,
                line2 = NA_integer_, col2 = NA_integer_))
  }

  ## We need to parse the file again, because of an R bug, at
  ## least on macOS. First-time parse in an R session fails
  pd <- getParseData(
    parse(getSrcFilename(func, full.names = TRUE), keep.source = TRUE)
  )

  line <- getSrcLocation(func, "parse")
  col  <- getSrcLocation(func, "column")

  pdline <- which(
    pd$parent == 0 &
    (pd$line1 < line | (pd$line1 == line & pd$col1 <= col)) &
    (pd$line2 > line | (pd$line2 == line & pd$col2 >= col))
  )

  list(
    file  = paste0("R/", getSrcFilename(func)),
    line1 = pd$line1[pdline],
    col1  = pd$col1 [pdline],
    line2 = pd$line2[pdline],
    col2  = pd$col2 [pdline]
  )
}
