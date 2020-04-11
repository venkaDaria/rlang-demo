get_fractions <- function(m, n) {
  u <- (0:m)/m
  v <- (0:n)/n
  w <- sort(c(u, v), TRUE)
  w <- unique(w)
  w
  
  # rev(sort(union(0:m/m,0:n/n)))
}