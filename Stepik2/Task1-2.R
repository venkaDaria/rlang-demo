mtcars$even_gear <- 0
for (i in 1:length(mtcars$gear)) {
  if (mtcars$gear[i] %% 2 != 1) {
    mtcars$even_gear[i] <- 1
  }
}

mpg_4 <- mtcars$mpg[mtcars$cyl == 4]

mini_mtcars <- mtcars[c(3, 7, 10, 12, nrow(mtcars)), ]

new_data <- subset(mtcars, cyl != 3 & qsec > mean(qsec))

mtcars$new_var <- 0
for (i in 1:length(mtcars$gear)) {
  if (mtcars$carb >= 4 || mtcars$cyl > 6) {
    mtcars$new_var[i] <- 1
  }
}

mtcars$new_var <- ifelse(mtcars$carb >= 4 | mtcars$cyl > 6, 1, 0) 
