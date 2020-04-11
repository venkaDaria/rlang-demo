# Напишите функцию fill_na, которая принимает на вход данные с тремя переменными:
# x_1  -  числовой вектор
# x_2 - числовой вектор
# y - числовой вектор с пропущенными значениями.
# Теперь — самое интересное. На первом этапе, используя только наблюдения, в которых нет пропущенных значений, 
# мы построим регрессионную модель (без взаимодействий), где  y — зависимая переменная, x_1 и x_2 — независимые переменные. 
# Затем, используя построенную модель, мы заполним пропущенные значения предсказаниями модели.
# Функция должна возвращать dataframe c новой переменной  y_full. Сохраните в нее переменную y, 
# в которой пропущенные значения заполнены предсказанными значениями построенной модели.
fill_na <- function(x){
  model <- lm(x[,3]~x[,1]+x[,2], x)
  x$y_full <- ifelse(is.na(x$y), predict(model, x), x$y)
  return(x)
}
# vs
fill_na <- function(x)
  data.frame(x, y_full = ifelse(is.na(x$y), predict(lm(y ~ x_1 + x_2, x), x), x$y))

# В переменной df сохранен subset данных mtcars только с переменными "wt", "mpg", "disp", "drat", "hp". 
# Воспользуйтесь множественным регрессионным анализом, чтобы предсказать вес машины (переменная "wt"). 
# Выберите такую комбинацию независимых переменных (из "mpg", "disp", "drat", "hp"), 
# чтобы значение R^2 adjusted было наибольшим. 
# Взаимодействия факторов учитывать не надо. 
summary(lm(wt ~ mpg + disp + drat + hp, data = df))$adj.r.squared # (remove one by one)
# Answer: summary(lm(wt ~ mpg + hp + disp, data = df))$adj.r.squared

# Воспользуйтесь встроенным датасетом attitude, чтобы предсказать рейтинг (rating) по переменным complaints и critical. 
# Каково t-значение для взаимодействия двух факторов?
summary(lm(rating ~ complaints * critical, data = attitude))

# Визуализируйте взаимодействие переменных wt и am, дополнив код, приведённый в задании:
# Ось x - переменная wt
# Ось y - переменная mpg
# Цвет регрессионных прямых - переменная am
library(ggplot2)
mtcars$am <- factor(mtcars$am)
my_plot <- ggplot(mtcars, aes(x = wt, y = mpg, col = am)) + geom_smooth(method='lm')
# my_plot <- qplot(wt, mpg, data=mtcars, col = am, geom="smooth", method="lm")

# C помощью функции step найдите оптимальную модель для предсказания rating в датасете attitude. 
# Model_full и model_null уже созданы. 
model_full <- lm(rating ~ ., data = attitude) 
model_null <- lm(rating ~ 1, data = attitude)
# Сохраните команду с функцией step в переменную ideal_model.
ideal_model <- step(model_full, diretion = 'backward')
# vs
scope <- list(lower = model_null, upper = model_full)
ideal_model <- step(model_full, scope = scope, direction = 'backward')

# Сравните полную модель из предыдущего степа и оптимальную модель с помощью функции anova. Введите получившееся F-значение.
# Разделителем дробной и целой части в ответе должна быть запятая.
anova(model_full, ideal_model)

# Напоследок потренируемся в эффективном написании формул. В этой задаче будем работать со встроенным датасетом LifeCycleSavings. 
# Попытаемся предсказать значение sr на основе всех остальных переменных в этом датасете. 
# Вспомните способы сокращения формул и напишите команду, которая создаёт линейную регрессию с главными эффектами 
# и всеми возможными взаимодействиями второго уровня. Сохраните модель в переменную model.
# Напишите программу. Тестируется через stdin → stdout
model <- lm(sr ~ (.)^2, LifeCycleSavings)
# vs 
model <- lm(sr ~ .*., LifeCycleSavings)
# vs
model <- lm(sr ~ .^2, LifeCycleSavings)