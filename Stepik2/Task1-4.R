new_var <- 0
n <- 0
for (i in 1:length(mtcars$gear)) {
  if (mtcars$mpg[i] > 20 & mtcars$cyl[i] != 3) {
    new_var <- new_var + mtcars$qsec[i]
    n <- n + 1
  }
}
result <-  new_var / n

result <- mean(mtcars$qsec[mtcars$cyl != 3 & mtcars$mpg > 20])

descriptions_stat <- aggregate(x = mtcars$hp, by = list(mtcars$am), FUN = sd)
descriptions_stat2 <- aggregate(x = mtcars$disp, by = list(mtcars$am), FUN = sd)
descriptions_stat$y <- descriptions_stat2$x
colnames(descriptions_stat) <- c("Group.1", "hp", "disp")

descriptions_stat <- aggregate(cbind(hp,disp) ~ am, mtcars, sd)

subs <- subset(airquality, airquality$Month %in% c(7,8,9))
result <- aggregate(Ozone ~ Month, subs, length)

describeBy(airquality,airquality$Month) 
describeBy(iris, iris$Species) 

fixed_vector <- my_vector
fixed_vector[is.na(my_vector)] <- mean(my_vector, na.rm=TRUE)