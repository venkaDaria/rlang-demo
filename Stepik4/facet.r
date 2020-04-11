# Потренируемся с разбиением графика на подгруппы! 
# Используя facet_grid постройте следующий график и сохраните его в переменную mpg_facet.
# ось x - переменная mpg
# facet - переменная am по строчкам и vs по столбцам
mpg_facet <- ggplot(mtcars, aes(mpg)) +
  geom_dotplot() +
  facet_grid(am ~ vs)

# Используя данные iris, постройте график плотности для переменной Sepal.Length. 
# Разбейте график на части по переменной Species при помощи facet_wrap. Результат сохраните в переменную sl_wrap. 
sl_wrap <- ggplot(iris, aes(Sepal.Length)) + 
  geom_density() + 
  facet_wrap(~ Species)

# Используя данные Iris, постройте график, иллюстрирующий взаимосвязь переменных Sepal.Length и Sepal.Width 
# внутри каждого вида (переменной Species), при помощи facet_wrap().
# В этом задании вам потребуется использовать два geom:
# geom_point - для отображения отдельных наблюдений,
# geom_smooth - для добавления сглаживания.
# Сохраните график в переменную my_plot.

# важна очредность геомов
my_plot <- ggplot(iris, aes(Sepal.Length, Sepal.Width)) + 
  facet_wrap(~ Species) + 
  geom_point() +
  geom_smooth()

# Вы можете скачать данные myMovieData (жмите на ссылку), в которых представлена различная информация 
# о голливудских фильмах с 2002 по 2005: тип жанр, бюджет и год выхода на экраны. 
# Давайте построим следующий график, чтобы выяснить есть ли различия в бюджетах фильмов разного жанра из года в год. 
# Cохраните результат в переменную my_plot.
# ось x - переменная Type
# ocь y - переменная Budget
# facet - переменная Year (используйте facet_grid)
my_plot <- ggplot(myMovieData, aes(Type, Budget)) + 
  geom_boxplot() + 
  facet_grid(. ~ Year) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
