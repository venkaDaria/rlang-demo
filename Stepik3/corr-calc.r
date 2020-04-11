# Напишите функцию corr.calc, которая на вход получает data.frame с двумя количественными переменными, 
# рассчитывает коэффициент корреляции Пирсона и возвращает вектор из двух значений: коэффициент корреляции и p - уровень значимости.
corr.calc <- function(x){
    coef <- cor.test(~x[[1]] + x[[2]], x)
    return(c(coef$estimate,coef$p.value))
}

# Напишите функцию filtered.cor которая на вход получает data.frame с произвольным количеством переменных
# (как количественными, так и любых других типов), рассчитывает коэффициенты корреляции Пирсона 
# между всеми парами количественных переменных и возвращает наибольшее по модулю значение коэффициента корреляции.
# (То есть функция может вернуть -0.9, если это наибольшая по модулю  корреляция).
# Гарантируется наличие в data.frame хотя бы двух количественных переменных.
filtered.cor <- function(x){
  df_num <- x[, sapply(x, is.numeric)]
  df <- psych::corr.test(df_num)
  diag(df$r) <- 0
  df_new <- as.vector(df$r)
  s <- df_new[which.max(abs(df_new))]
}
# vs
filtered.cor <- function(x){    
  num_var <- sapply(x, function(x) is.numeric(x))    
  cor_mat <- cor(x[, num_var])    
  diag(cor_mat) <- 0    
  return(cor_mat[which.max(abs(cor_mat))])
}

# Напишите функцию smart_cor, которая получает на вход dataframe с двумя количественными переменными. 
# Проверьте с помощью теста Шапиро-Уилка, что данные в обеих переменных принадлежат нормальному распределению.
# Если хотя бы в одном векторе распределение переменной отличается от нормального (p - value меньше 0.05), 
# то функция должна возвращать коэффициент корреляции Спирмена. (Числовой вектор из одного элемента).
# Если в обоих векторах распределение переменных от нормального значимо не отличается, 
# то функция должна возвращать коэффициент корреляции Пирсона.
smart_cor <- function(x) {
    g <- shapiro.test(x[[1]])
    h <- shapiro.test(x[[2]])
    if (h$p.value < 0.05 | g$p.value < 0.05) {
      b <- cor.test(~ x[[1]] + x[[2]], x, method = "spearman")
    } else {
      b <- cor.test(~ x[[1]] + x[[2]], x, method = "pearson")
    }
    b$estimate
}
# vs
smart_cor <- function(x){    
if (shapiro.test(x[[1]])$p < 0.05 | shapiro.test(x[[2]])$p < 0.05) {    
return(cor.test(x[[1]], x[[2]], method = 'spearman')$estimate)    
} else {    
return(cor.test(x[[1]], x[[2]], method = 'pearson')$estimate)}}

# Скачайте набор данных - dataframe с двумя количественными переменными 
# (вспомните при необходимости, как задавать разделитель и другие параметры функции read.table), 
# постройте линейную регрессию, где - первая переменная - зависимая, вторая - независимая. 
# В ответ укажите значения регрессионных коэффициентов сначала intercept затем  slope.
dataframe1 <- read.table("dataset_11508_12.txt", sep=' ' )    
mod1 <- lm(dataframe1[,1]~dataframe1[,2], dataframe1)
print(mod1$coefficients)

# Воспользуемся уже знакомыми данными diamonds из библиотеки ggplot2. 
# Только для бриллиантов класса Ideal (переменная cut) c числом карат равным 0.46 (переменная carat) постройте линейную регрессию, 
# где в качестве зависимой переменной выступает price, в качестве предиктора - переменная  depth. 
# Сохраните коэффициенты регрессии в переменную fit_coef.
data_for_model <- subset(diamonds, cut == 'Ideal' & carat == 0.46) #subset(d, d$cut == "Ideal" & d$carat == 0.46)   
fit <- lm(price ~ depth, data_for_model)    
fit_coef <- fit$coefficients
# vs
fit_coef <- lm(price ~ depth, diamonds, cut =='Ideal' & carat == 0.46)$coef

# Напишите функцию regr.calc, которая на вход получает dataframe c двумя переменными.
# Если две переменные значимо коррелируют (p - уровень значимости для коэффициента корреляции Пирсона меньше 0.05), 
# то функция строит регрессионную модель, где первая переменная - зависимая, вторая - независимая. 
# Затем создает в dataframe новую переменную с назанием fit, где сохраняет предсказанные моделью значения зависимой переменной. 
# В результате функция должна возвращать исходный dataframe с добавленной новой переменной fit.
# Если две переменные значимо не коррелируют, то функция возвращает строчку "There is no sense in prediction"
regr.calc <- function(x){
    pearson <- cor.test(x = x[[1]], y = x[[2]], method = "pearson")

    if (pearson$p.value < 0.05) {
        linm <- lm(x[[1]] ~ x[[2]])
        x$fit <- linm$fitted.values
        return(x)
    }

    return("There is no sense in prediction")
}

# Постройте scatterplot по данным iris, сохранив его в переменную my_plot : 
# Ось X - переменная Sepal.Width
# Ось Y -  переменная Petal.Width
# Цвет точек - переменная Species
# Также добавьте линейное сглаживание для каждой группы наблюдений по переменной Species.
library(ggplot2)
my_plot <- ggplot(iris, aes(Sepal.Width, Petal.Width, col=Species)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm")