combin_count <- function(n, k, with_repretitions = FALSE) {
  if (with_repretitions) {
    print(factorial(n + k - 1)/(factorial(k) * factorial(n - 1)))
  } else {
    print(factorial(n) / (factorial(k) * factorial(n - k)))
  }
}