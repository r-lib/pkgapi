
function_calls <- function(path) {

  exprs <- parse_expressions(path)
  calls <- lapply(exprs, find_calls)

  res <- do.call(rbind, calls)
  row.names(res) <- NULL
  res
}

find_calls <- function(expr) {
  pd <- getParseData(expr)
  fc <- which(pd$token == "SYMBOL_FUNCTION_CALL")

  res <- data.frame(
    stringsAsFactors = FALSE,
    file  = if (length(fc)) paste0("R/", getSrcFilename(expr)) else character(),
    from  = character(length(fc)),      # to be filled later
    to    = character(length(fc)),      # to be filled later
    type  = if (length(fc)) "call" else character(),
    line1 = pd$line1[fc],
    line2 = pd$line2[fc],
    col1  = pd$col1[fc],
    col2  = pd$col2[fc],
    str   = pd$text[fc],
    args  = character(length(fc))
  )

  ## We fill in explicit :: and ::: call targets here, the rest later
  ## Need to search for this:
  ## expr
  ## +- SYMBOL_PACKAGE
  ## +- NS_GET / NS_GET_INT
  ## +- SYMBOL_FUNCTION_CALL

  for (i in seq_along(fc)) {
    call_row <- fc[i]
    parent_id <- as.character(pd$parent[call_row])
    if (parent_id != 0 && pd[parent_id,]$token == "expr") {
      sibling_ids <- as.character(pd$id[pd$parent == parent_id])
      subpd <- pd[sibling_ids, , drop = FALSE]
      if (nrow(subpd) == 3) {
        subpd <- subpd[order(subpd$line1, subpd$col1), ]
        if (subpd$token[1] == "SYMBOL_PACKAGE" &&
            subpd$token[2] %in% c("NS_GET", "NS_GET_INT") &&
            subpd$token[3] == "SYMBOL_FUNCTION_CALL") {
          res$to[i] <- paste0(subpd$text[1], "::", subpd$text[3])
        }
      }
      arg_ids <- arg_ids(pd, call_row)
      res$args[i] <- paste(utils::getParseText(pd, arg_ids), collapse = " ")
    }
  }

  res
}

arg_ids <- function (pd, rn) {
  id <- pd$id[rn]
  pd$rn <- seq_len(nrow(pd))
  parent <- pd$parent[pd$id == id]
  grandparent <- pd$parent[pd$id == parent]
  uncles <- pd[pd$parent == grandparent & pd$rn > rn & pd$id != parent & pd$token != "COMMENT", ]

  return(uncles$id)
}


find_caller <- function(calls, idx, defs) {

  file <- calls$file[idx]
  line1 <- calls$line1[idx]
  line2 <- calls$line2[idx]
  col1 <- calls$col1[idx]
  col2 <- calls$col2[idx]

  w <- which(
    defs$file == file &
      (defs$line1 < line1 | (defs$line1 == line1 & defs$col1 <= col1)) &
      (defs$line2 > line2 | (defs$line2 == line2 & defs$col2 >= col2))
  )

  if (length(w) == 1) {
    defs$name[w]

  } else if (length(w) > 1) {
    ## This is possible if two symbols point to the same function,
    ## as the function's srcref will only point to a single location
    defs$name[w[1]]

  } else {
    ## Calling from outside a function
    ""
  }
}
