air <- airquality
ggplot(air, aes(x = factor(Month), y = Ozone))+geom_boxplot()

plot1 <- ggplot(mtcars, aes(x = mpg, y = disp, col = hp)) + geom_point()

save(my_sd, file = "my_sd.RData")