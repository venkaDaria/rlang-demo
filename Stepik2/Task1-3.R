if (mean(my_vector) > 20) {
  result <- "My mean is great"
} else {
  result <- "My mean is not so great"
} 

good_months <- c()
for (i in 2:144) {
  if (AirPassengers[i] > AirPassengers[i-1]) {
    good_months <- c(good_months, AirPassengers[i])
  }
}

good_months <- AirPassengers[-1][AirPassengers[-1] > AirPassengers[-144]] 

res <- c()
for (i in 1:144) {
  res <- c(res, mean(AirPassengers[i:(i+9)]))
}
moving_average <- res[1:135]

moving_average <- numeric(135) # создаем пустой числовой вектор из 135 элементов    
last_index <- length(AirPassengers) - 9    
for (i in 1:last_index) {    
  end <- i + 9    
  moving_average[i] <- mean(AirPassengers[i:end])    
}    

n <- 10    
d <- AirPassengers    
cx <- c(0, cumsum(d))    
moving_average <- (cx[(n + 1):length(cx)] - cx[1:(length(cx) - n)]) / n