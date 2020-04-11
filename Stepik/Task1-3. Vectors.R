dice_roll <- function(n) {
  x <- runif(n)
  ifelse(x > 5/6, 6, 
         ifelse(x > 4/6, 5, 
                ifelse(x > 3/6, 4, 
                       ifelse(x > 2/6, 3, 
                              ifelse(x > 1/6, 2, 1)))))
  
  # dice_roll <- function(n) {
  #   for (i in 1:n) {
  #     print(sample(6,1))
  #   }}
}