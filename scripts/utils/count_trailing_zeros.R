count_trailing_zeroes <- function(code) {
  parts <- unlist(strsplit(code, "-"))
  count <- sum(parts == "000000")
  return(count)
}
