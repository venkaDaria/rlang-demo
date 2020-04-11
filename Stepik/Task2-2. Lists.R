find_closest <- function(v, n) {
  t <- abs(v - n) # вектор разностей
  sort(which(t == min(t))) # все минимумы (сортируем)
}

build_ziggurat <- function(n) build_ziggurat_recursive(n, 1)

build_ziggurat_recursive <- function(n, count) {
  m <- matrix(count, nrow = n * 2 - 1, ncol = n * 2 - 1)
  if (n > 1) {
    m[2:(nrow(m) - 1), 2:(ncol(m) - 1)] <- build_ziggurat_recursive(n - 1, count + 1) 
  }
  m
}

count_elements <- function(x) {
  y <- unique(sort(x))
  rbind(y, sapply(y, function(i) sum(i == x)))
}