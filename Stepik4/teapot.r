# Датасет (ссылка) содержит информацию о полигонах трехмерной модели чайника. 
# Три столбца таблицы задают координаты (x,y,z), а тройки строк задают треугольники 
# (т.е. строки 1,2,3 - первый треугольник, 4,5,6 - второй, и так далее).
# Напишите функцию, которая будет принимать data.table с этими данными, и возвращать объект plotly с трехмерной моделью. 
# Следует воспользоваться установкой индексов i,j,k.
# NB! Не стоит хардкодить количестве строк в датасете. Решение не должно зависеть от этого параметра.
make.fancy.teapot <- function(teapot.coords) {
    i <- seq(0,length(teapot.coords[­[1]])-1,3)
    j <- seq(1,length(teapot.coords[­[1]]),3)
    k <- seq(2,length(teapot.coords[­[1]]),3)

    plot_ly(teapot.coords, type="mesh3d",
    x = x, y = y, z = z,
    i = i, j = j, k = k,
    alphahull = 0)
}
# vs
make.fancy.teapot <- function(teapot.coords) {
  is <- (1:(nrow(teapot.coords) / 3)) * 3 - 3
  js <- (1:(nrow(teapot.coords) / 3)) * 3 - 2
  ks <- (1:(nrow(teapot.coords) / 3)) * 3 - 1
  
plot_ly(teapot.coords, x=x,y=y,z=z,
               i = is, 
               j = js,
               k = ks, type='mesh3d')
}