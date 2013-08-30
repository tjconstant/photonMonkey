label_scientific10 <- function(x) {
  parse(text=gsub("e", " %*% 10^", scientific_format()(x)))
}
