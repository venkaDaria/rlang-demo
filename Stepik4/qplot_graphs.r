# Используя функцию qplot, постройте гистограмму переменной depth из данных diamonds. Сохраните график в переменную depth_hist.
depth_hist <- qplot(diamonds$depth)

# Постройте диаграмму рассеивания (scatter plot) как в указанном ниже примере, 
# результат сохраните в переменную price_carat_clarity_points.
# данные - diamonds
# ось x - carat
# ось y - price
# цвет точек - clarity
price_carat_clarity_points <- qplot(x = carat,
                                    y = price,
                                    color = clarity,
                                    data = diamonds)

# Используя функцию qplot, постройте график плотности переменной x из данных diamonds. Сохраните график в переменную x_density.
x_density <- qplot(x = x, data = diamonds, geom = "density")                              

# Усложним задачу, постройте график плотности переменной x для каждой группы наблюдений по переменной cut из данных diamonds. 
# Таким образом за цвет графика теперь отвечает переменная cut. 
# Сохраните результат в переменную x_cut_density.
x_cut_density <- qplot(x = x,
      color = cut,
      data = diamonds,
      geom = "density")

# Давайте построим график violin plot для переменной price в каждой группе наблюдений по переменной color. 
# Сохраните результа в переменную price_violin.
price_violin <- qplot(x = color,
      y = price,
      data = diamonds,
      geom = "violin")
