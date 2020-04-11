NA.position <- function(x){
  my_vector <- which(is.na(x))
}

NA.counter <- function(x){
  length(which(is.na(x))) 
}

NA.counter <- function(x){    
  sum(is.na(x))
}

filtered.sum <- function(x){
  sum(x[which(x > 0)])
}

filtered.sum <- function(x){    
  sum(x[x > 0], na.rm = T)
}

outliers.rm <- function(x){
  iqr <- IQR(x)
  quart1 <- quantile(x, probs = c(0.25))
  quart3 <- quantile(x, probs = c(0.75))
  x <- x[abs(quart1 - x) <= 1.5 * iqr | abs(quart3 - x) <= 1.5 * iqr] 
}

outliers.rm <- function(x){    
  q <- quantile(x, 0.25) + quantile(x, 0.75)    
  x[abs(x - q/2) <= 2*IQR(x)]
}