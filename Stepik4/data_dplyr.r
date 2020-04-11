# Давайте потренируемся обращаться к данным. Вы можете использовать базовый синтаксис, функции из пакета dplyr или data.table.
# Поработаем с данными diamonds из пакета ggplot2.
# В переменную d сохраните только нeчетные строчки исходных данных diamonds.
# Обратите внимание на функцию seq(). Она может вам пригодиться вам не только в этой задаче.
slice(diamonds, seq(1, nrow(diamonds), by = 2))
# vs
diamonds[c(T,F), ]
# vs 
subset(diamonds, c(T,F))
# vs
filter(diamonds, row_number() %% 2 != 0)

# Из данных mtcars отберите только четыре переменные: mpg, hp, am, vs. 
# Оставьте только те наблюдения, для которых значения mpg > 14 и hp > 100. 
# Отсортируйте получившиеся данные по убыванию переменной mpg и возьмите только первые 10 строчек.
#  Переменную mpg переименуйте в Miles per gallon, а переменную hp в  Gross horsepower 
#  (обратите внимание, dplyr позволит нам создать переменные с пробелами в названии). 
library(dplyr)
my_df <- mtcars %>% 
  select(mpg, hp, am, vs) %>% 
  filter(mpg > 14, hp > 100) %>% 
  arrange(-mpg) %>% 
  slice(1:10) %>% 
  rename("Miles per gallon" = mpg, "Gross horsepower" = hp)

# Напишите функцию, all_to_factor, которая преобразует dataframe, переводя все его переменные в фактор.
all_to_factor <- function(x){
    mutate_each(x, funs(as.factor)) # or just factor
}
# vs
all_to_factor <- function(x){
    mutate_all(x, factor)
}

# В этом задании от вас потребуется написать функцию для предобработки данных log_transform. 
# В статистике часто трансформируют исходные переменные. Например, используют значение натурального логарифма исходной переменной.
# Ваша задача написать функцию, которая получает на вход dataframe с произвольным числом переменных разных типов. 
# На первом этапе функция должна выполнить предобработку числовых переменных. 
# Т.к. значение логарифма мы можем рассчитать только для положительных чисел. 
# Для этого сделаем центрирование всех переменных (Rescaling), только еще добавим единичку, чтобы у нас не осталось нулей.
# После того как мы масштабировали каждую переменную, осталось рассчитать значение натурального логарифма каждого наблюдения 
# (функция log) и вернуть новый dataframe. 
log_transform <- function(td) {
  mutate_if(td, is.numeric, funs(log(((.) - min(.))/(max(.)-min(.)) + 1)))
}
# vs
log_transform <- function(df){
    df %>% mutate_if(is.numeric, funs(log(scales::rescale(.) + 1)))
}

# Итак, ваша задача будет написать функцию descriptive_stats, которая рассчитывает основные описательные статистики 
# в каждой группе наблюдений для описанного выше примера. Функция получает на вход dataframe с тремя переменными 
# salary - значение заработной платы, gender - фактор с двумя градациями (male, female), 
# country - фактор с двумя градациями (England, France).
# Функция должна возвращать dataframe с описательными статистиками и количеством NA, 
# рассчитанными в каждой группе: количество наблюдений, среднее значение, стандартное отклонение, медиана, 
# первый квартиль, третий квартиль, число пропущенных значений.
descriptive_stats <- function (dataset){
    dataset %>% 
      group_by(gender, country) %>% 
      summarise(n = n(),
            mean = mean(salary, na.rm = T),
            sd = sd(salary, na.rm = T),
            median = median(salary, na.rm = T),
            first_quartile = quantile(salary, 0.25, na.rm = T), # quantile(salary, na.rm = T)[2] 
            third_quartile = quantile(salary, 0.75, na.rm = T), # quantile(salary, na.rm = T)[4]
            na_values = sum(is.na(salary)))
}

# Напишите функцию, to_factors, которая получает на вход dataframe 
# с произвольным числом количественных переменных и вектор с номерами колонок, которые нужно перевести в фактор.
# Для перевода числовых колонок в фактор будем использовать следующий принцип, 
# если наблюдение больше среднего всей переменной то 1, иначе 0.
to_factors <- function(test_data, factors){
  test_data %>% mutate_at(factors, funs(as.factor(ifelse(. > mean(.), 1, 0 ))))
}
# vs
mutate_at(df, factors, funs(factor(. > mean(.), labels = 0:1)))
# vs
df %>% mutate_at(factors, funs(as.factor(as.numeric(. > mean(.)))))
# vs
test_data %>% 
  mutate_if(.predicate=(log_col), funs(as.factor(ifelse(.>mean(.),1,0))))

# Возьмем данные diamonds для работы в этой задаче. 
# Создайте новый dataframe с именем high_price, в котором будут хранится только 10 самых дорогих бриллиантов каждого цвета. 
# Также в итоговом datafrmae должны храниться только две переменные color и price.
high_price <- diamonds %>% select(color, price) %>% group_by(color) %>% arrange(color, -price) %>% slice(1:10)
# vs
high_price <- diamonds %>% group_by(color) %>% arrange(desc(price)) %>% slice(1:10) %>% select(color, price)
