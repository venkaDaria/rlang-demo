# Давайте потренируемся комбинировать различные geoms на одном графике. 
# Используя данные mtcars скомбинируем два варианта отображения количественных данных boxplot и violin plot:
my_plot <- ggplot(mtcars, aes(factor(am), mpg)) +
  geom_violin() +
  geom_boxplot(width = 0.2) 

# Отобразите взаимосвязь между доходом (income) и числом продаж (sale), цветом точек указав номер магазина (shop)
my_plot <- ggplot(sales, aes(income, sale)) + 
  geom_point(aes(col = shop)) + 
  geom_smooth()

# При помощи функции stat_summary постройте график с доверительными интервалами для демонстрации различий 
# в доходах двух магазинов с учетом времени года:
# переменная shop - ось x;
# переменная income - ось y;
# переменная season - цвет;
# geom pointrange.
my_plot <- ggplot(sales, aes(shop, income, col = season))+
  stat_summary(geom = "pointrange", position = position_dodge(0.2)) 
# vs
my_plot <-ggplot(sales, aes(x=shop, y=income, color=factor(season))) +
        stat_summary(fun.data = mean_cl_boot, position = position_dodge(0.2))

# Теперь давайте отобразим на графике различия в продажах (переменная sale), в зависимости от:
# года (date) - ось x;
# и номера магазина (shop) - цвет.
# Дополните предложенный код, чтобы получился график как в примере ниже. 
# Используйте функцию mean_cl_boot для построения доверительных интервалов.
# Вам также понадобится использовать три geoms: errorbar, point, line. 
# Используйте их прямо внутри функции stat_summary().
library(ggplot2)
install.packages("Hmisc")
library(Hmisc)
ggplot(sales, aes(date, sale, color = shop)) + 
  stat_summary(fun.data = mean_cl_boot,
               geom = "errorbar",
               position = position_dodge(0.2),
               width = 0.2,
               size = 1) +
  stat_summary(fun.data = mean_cl_boot,
               geom = "point",
               position = position_dodge(0.2),
               size = 2) +
  stat_summary(fun.y = mean,
               geom = "line",
               position = position_dodge(0.2),
               size = 1)

# В этом задании мы построим график используя данные Iris. 
# Наша цель отобразить взаимосвязь переменных Sepal.Length (ось X) и Petal.Length (ось Y) внутри трех групп по переменной Species. Для этого постройте scaterplot, отобразите цветом значения переменной Species и добавьте линейное сглаживание в каждой группе.
# Далее от вас потребуется привести график к более завершенному виду.
# Мы переведем на русский название осей, название легенды и ее расшифровку:
# Ось X - "Длина чашелистика".
# Ось Y - "Длина лепестка".
# Название легенды - "Вид цветка".
# Расшифровка легенды: "Ирис щетинистый", "Ирис разноцветный", "Ирис виргинский".
# Также мы чуть измени отображение значений по осям.
# Значения по оси X должны начинаться с 4 и заканчиваться на 8 с шагом в единицу.
# Значения по оси Y должны начинаться с 1 и заканчиваться на 7 с шагом в единицу.
ggplot(iris, aes(Sepal.Length, Petal.Length, col = Species)) +
  geom_point() +
  geom_smooth(method = "lm") + # линенйное сглаживание
  scale_x_continuous(name = "Длина чашелистика",
                     breaks = seq(4, 8 , 1),
                     limits = c(4,8)) + 
  scale_y_continuous(name = "Длина лепестка",
                     breaks = seq(1, 7, 1),
                     limits = c(1, 7)) + 
  scale_color_discrete(name = "Вид цветка",
                       labels = c("Ирис щетинистый",
                                  "Ирис разноцветный",
                                  "Ирис виргинский"))