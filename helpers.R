extract <- function(text) {
  text <- gsub(" ", "", text)
  split <- strsplit(text, ",", fixed = FALSE)[[1]]
  as.numeric(split)
}

# Returns NULL if inputs are valid, or a character string describing the error.
validate_inputs <- function(x, y) {
  if (anyNA(x) || length(x) < 3 || anyNA(y) || length(y) < 3) {
    return("Invalid input or not enough observations (at least 3 required)")
  }
  if (length(x) != length(y)) {
    return("Number of observations must be equal for x and y")
  }
  if (length(unique(x)) <= 1) {
    return("x must contain more than one distinct value")
  }
  NULL
}
