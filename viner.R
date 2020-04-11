wiener_gen <- function(h, T) {
  return(
    cumsum ( rnorm(T/h, mean = 0,  sd = h) )
  )
}

h = 0.01
T = 10
graphics <- replicate(10, wiener_gen(h, T))
matplot(graphics, type = "l", col = 1:10, lwd = 1, lty = 1, xlim = c(0, T), ylim = c(-0.1,0.1),xlab = ("t"), ylab = ("W(t)"))