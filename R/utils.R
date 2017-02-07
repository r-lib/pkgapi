sort_c <- function(x) {
  withr::with_collate("C", sort(x))
}
