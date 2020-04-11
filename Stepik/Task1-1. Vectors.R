# Traditions, traditions
hello -> "Hello world!"
print(hello)
hello

# Lost in translation
ñ(1, 2, 3) # ?!
c(3, 2, 1)

# Important caveat
0.1 + 0.1 == 0.2
0.1 + 0.05 == 0.15
# R FAQ 7.31
# google "R why are these numbers not equal"
# http://stackoverflow.com/questions/9508518/why-are-these-numbers-not-equal
?all.equal
all.equal(0.1 + 0.05, 0.15)

# Non-trivial sequence
u <- seq(0, 1, 1/3)
v <- (0:7)/7
c(u, v)
help.search("sort")
w <- sort(c(u, v))
w <- unique(w)
w

# fizz-buzz, imperative style
y <- vector(mode = "character", length = 100)
y <- character(100)
for (i in 1:100) {
  if (i %% 15 == 0) {
    y[i] <- "fizz buzz"
  } else if (i %% 3 == 0) {
    y[i] <- "fizz"
  } else if (i %% 5 == 0) {
    y[i] <- "buzz"
  } else {
    y[i] <- i
  }
}
y

# fizz-buzz, vector-oriented style
x <- 1:100
z <- 1:100
x %% 5
x %% 5 == 0
z[x %% 5 == 0]
z[x %% 5 == 0] <- "buzz"
z[x %% 3 == 0] <- "fizz"
z[x %% 15 == 0] <- "fizz buzz"
z
all(y == z)

# Geometric progression
x <- 2 ^ (0:10)
x
log2(x)

# Some randomness
set.seed(42)
x <- sample(1:100, 50)

# Neigbors with greatest diff
x[-1]
x[-length(x)]
x[-1] - x[-length(x)]
k <- which.max(abs(x[-1] - x[-length(x)]))
x[c(k, k + 1)]

# Multiple min/max
x <- sample(1:100, 50, replace = TRUE)
min(x)
which.min(x)
which(x == min(x))

# Task 1-5-3
x[1:length(x) %% 7 > 0]
x[-(seq(7, length(x), by = 7))]
x[-(1:floor(length(x)/7) * 7)]
x[1:length(x) %% 7 != 0]
x[c(rep(T, 6), F)]

# Task 1-5-7
x <- c(1, 23, 5, 19, 15, 13, 5)
for (i in x){print(letters[i])}

# Task 1-5-9
is_monotone <- function(x) all(x == cummax(x)) || all(x == cummin(x))