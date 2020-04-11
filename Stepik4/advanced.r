# В переменной my_df сохранен dataframe с произвольным числом количественных переменных. 
# При помощи функции apply найдите максимальное значение в каждой строке. 
# Сохраните результат (вектор максимальных значений) в переменную row_max.
row_max <- apply(my_df, MARGIN = 1 , FUN = max)

# В переменной my_df сохранен dataframe с произвольным числом количественных переменных. 
# Рассчитайте медиану для всех столбцов с количественными переменными. 
# В переменную col_median сохраните вектор полученных значений. 
col_median <- apply(my_df, MARGIN = 2, FUN = median)

# Напишите функцию get_negative_values, которая получает на вход dataframe произвольного размера. 
# Функция должна для каждой переменной в данных проверять, есть ли в ней отрицательные значения. 
# Если в переменной отрицательных значений нет, то эта переменная нас не интересует, для всех переменных, 
# в которых есть отрицательные значения мы сохраним их в виде списка или матрицы, 
# если число элементов будет одинаковым в каждой переменной (смотри пример работы функции).
get_negative_values <- function(test_data){
    res <- apply(test_data[apply(test_data, 2, function(X) length(X[X<0 & !is.na(X)])>0)], 2, function(X) X[X<0 & !is.na(X)])
    return (res[sapply(res,length)>0])
}
# vs
get_negative_values <- function(test_data){    
    negative_col <- apply(test_data, 2, function(x) any(x[!is.na(x)] < 0))    
    return(apply(test_data[negative_col], 2, function(x) x[!is.na(x) & x <0]))
}

# Напишите функцию na_rm которая заменяет все пропущенные значения в столбцах dataframe на соответствующее среднее значение. 
# То есть все NA в первом столбце заменяются на среднее значение первого столбца (рассчитанного без учета NA). 
# Все NA второго столбца заменяются на среднее значение второго столбца и т.д.  
# Функция na_rm на вход получает dataframe произвольной размерности, состоящий из количественных переменных. 
# Функция должна возвращать  dataframe с замененными NA. Ни порядок столбцов, ни порядок строк в dataframe изменять не нужно.
na_rm <- function(x) {
  as.data.frame(apply(x, 2, function(y) ifelse(is.na(y), mean(y, na.rm = T), y)))
}
# vs
na_rm  <- function(x){    
	na_to_mean <- function(v){    
		v[is.na(v)] <- mean(v , na.rm = T)    
		return(v)
    }    
	as.data.frame(apply(x, 2, na_to_mean))
}
# vs
na_rm  <- function(test_data){
    as.data.frame(apply(test_data, 2, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x)))
}

# Напишите функцию positive_sum, которая получает на вход dataframe с произвольным количеством числовых переменных. 
# Основная задача функции - найти сумму положительных значений в каждой переменной и сохранить их в список. 
positive_sum <- function(test_data) {
  lapply(test_data, function(x) sum(x[x>0], na.rm = T))
}
# vs
positive_sum <-  function(d){
    lapply(d, FUN = function(x) sum(subset(x, x > 0)))
}

# Таким образом в процессе проверки на вход вашей функции будет подаваться два аргумента:
# 1. Датафрейм, c произвольным количеством строк, где имена генов сохранены в переменной names (фактор)
#  в формате ****@name , а уровень экспрессии в переменной expression.
# 2. Вектор с именами генов, для которых мы хотим отобрать наблюдения.
#  Гарантируется, что имена указанные в векторе есть в данных.
# Функция возвращает датафрейм с наблюдениями только для указанных генов.
my_names <- function (dataset, names){
    one <- sapply(names, function(x) grepl(x, dataset$name))
    dataset[as.logical(apply(one, 1, sum)),]
}
# vs
my_names <- function (dataset, names){    
dataset[as.numeric(lapply(names, function(x) which(grepl(x, dataset$name)))),]}
# vs
my_names <- function (dataset, names){    
gs=gsub('^.*\\@','',dataset[,1])    
return(dataset[gs %in% names,])}

# Напишите функцию find_outliers, которая получает на вход dataframe с одной количественной переменной 
# и произвольным числом факторных переменных. Факторные переменные разбивают все наши наблюдения на определенное число групп. 
# Например, если мы посмотрим на данные mtcars и возьмем в качестве группирующих переменных: am - две градации и cyl три градации, 
# то получим 6 групп наблюдений на пересечении градаций этих переменных.
library(dplyr)
library(lazyeval)
find_outliers <- function(t){
  num_var <- names(which(sapply(t, is.numeric)))
  group_var <- names(which(!sapply(t, is.numeric)))    
  t %>% 
    group_by_(.dots = group_var) %>%
    mutate_(is_outlier = interp(~ifelse(abs(x - mean(x)) > 2 * sd(x), 1, 0), x = as.name(num_var)))
}
# vs
find_outliers <- function(df){
  fctr_cols_names <- names(which(sapply(df, is.factor)))
  df %>%
    group_by_(.dots = fctr_cols_names) %>% 
    mutate_all(funs(is_outlier = as.numeric(abs(. - mean(.)) > 2 * sd(.))))
}
# vs
find_outliers <- function(test_data){
  test_data %>% 
    group_by_if(is.factor) %>% 
    mutate_if(is.numeric, funs(is_outlier = ifelse(abs(. - mean(.)) > 2 * sd(.), 1, 0)))
}
# vs
find_outliers <- function(data) {
    num_cols <- sapply(data, is.numeric)
    splitted <- split(data[, num_cols], data[!num_cols])
    outliers <- lapply(splitted, function(x) as.numeric(abs(x - mean(x)) > sd(x) * 2))
    data$is_outlier <- unsplit(outliers, data[!num_cols])
    data
}

# Напишите функцию smart_lm, которая получает на вход data.frame с произвольным числом количественных переменных. 
# Первая колонка в данных - это зависимая переменная, все остальные - предикторы. 
# На первом этапе вы должны отобрать предикторы для модели.
# Функция возвращает в виде вектора коэффициенты линейной регрессии построенной только для отобранных предикторов 
# (условие нормальности распределения). 
# Если таких предикторов в данных не оказалось, то функция возвращает предупреждение "There are no normal variables in the data".
smart_lm <- function(df){
    is.normal <- sapply(df[-1], function(x) shapiro.test(x)$p > 0.05)
    if (sum(is.normal) > 0) {
      lm(df[c(T, is.normal)])$coeff
    } else {
      "There are no normal variables in the data"
    }
}

# Напишите функцию one_sample_t, которая получает на вход два аргумента:
# 1. Dataframe произвольного размера с произвольным числом переменных различного типа.
# 2. Числовое значение среднего в генеральной совокупности.
# Ваша функция должна применять одновыборочный t - test к каждой числовой переменной в данных, 
# и сравнивать среднее значение этой переменной с указанным значением среднего в генеральной совокупности (второй аргумент функции).
# Функция должна возвращать список, где каждый элемент это вектор, состоящий из 
# t - значения, числа степеней свобод (df) и значения p - value.
one_sample_t <- function(test_data, general_mean){
  b <- names(which(sapply(test_data, is.numeric)))
  lapply(test_data[b], function(x) c(t.test(x,mu=general_mean)$statistic,t.test(x,mu=general_mean)$parameter,t.test(x,mu=general_mean)$p.value))
}
# vs
one_sample_t <- function(data, mean) {
    lapply(data[sapply(data, is.numeric)],
           function(x) unlist(t.test(x, mu = mean)[c("statistic", "parameter", "p.value")]))
}

# Итак, ваша задача, написать функцию get_p_value, которая получает на вход список (назовем его главным списком), 
# каждый элемент этого списка тоже список - результат выполнения функции shapiro.test (смотри пример normality_tests). 
# Ваша задача из каждого элемента главного списка вытащить только p - value. 
# В итоге функция возвращает список где каждый элемент - одно значение - p - value (как в примере normality_tests_p).
get_p_value <- function(test_list){
     lapply(test_list, function(x) x$p.value)
}
# vs
get_p_value <- function(test_data){    
    sapply(test_data, '[', 2)
}