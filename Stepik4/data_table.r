# Напишите функцию filter.expensive.available, которая принимает на вход products (объект типа data.table) и вектор названий брендов, 
# и возвращает только те строчки, которые соответствуют товарам, цена которых больше или равна 5000 рублей, 
# которые доступны на складе и принадлежат одному из переданных брендов.
filter.expensive.available <- function(products, brands) {
  products[(price > 500000) & (available == T) & (brand %in% brands)]
}
# vs
products[brand %in% brands][price >= 500000][available == T]

# Создайте функцию ordered.short.purchase.data, которая будет принимать purchases, объект data.table, 
# и возвращать таблицу только со столбцами с номером заказа и ID продукта.
# Упорядочите результат по убыванию стоимости купленного товара. 
# Возвраты (записи с отрицательным количеством предметов в позиции) надо удалить.
ordered.short.purchase.data <- function(purchases) {
  purchases[order(-price)][!(quantity < 0), .(ordernumber, product_id)] # quantity >= 0
}
# vs
purchases[quantity >= 0][order(-price), .(ordernumber, product_id)]

# Напишите функцию purchases.median.order.price, у которой один аргумент: 
# purchases, и которая возвращает медианную стоимость заказа (число).
# Группировку стоит проводить с помощью data.table. 
# Записи с неположительным количеством купленных товаров (возвраты) игнорировать.
# Обратите внимание, что одному заказу может соответствовать несколько записей – «позиций» с одинаковым ordernumber, 
# и что при расчете стоимости заказа надо учитывать ситуации, когда пользователь купил несколько товаров одного типа 
# (их количество указано в quantity).
purchases.median.order.price <- function(purchases) {
  purchases[quantity > 0][, .(w = sum(price*quantity)), by = ordernumber][, median(w)]
}
# vs
median(purchases[quantity >= 0][, list(w = sum(price * quantity)), by=list(ordernumber)]$w)}

# Создайте функцию get.category.ratings, которая будет возвращать суммарный оборот (с учетом скидок) каждой категории,
# и количество купленных предметов по таблице покупок и таблице принадлежности товара к категории. 
# Если купленный товар принадлежит нескольким категориям, его необходимо учитывать во всех. При решении используйте ключи.
get.category.ratings <- function(purchases, product.category) {
     setkey(purchases)
     setkey(product.category)
     tb <- merge(purchases, product.category, by = 'product_id', allow.cartesian=TRUE)
     tb[, lapply(.SD,sum), by=.(category_id)][, list(category_id, totalcents, quantity)]
}
# vs 
get.category.ratings <- function(purchases, product.category) {
     setkey(purchases)
     setkey(product.category)
     tb <- merge(purchases, product.category, by = 'product_id', allow.cartesian=TRUE)
     tb[, list(totalcents=sum(totalcents), quantity=sum(quantity)), by = category_id]
}

# Напишите функцию, которая будет с помощью := добавлять столбец «price.portion», 
# содержащий процент стоимости товара в заказе, с двумя знаками после запятой (нули после запятой не опускать). 
# Проверяться будет возвращаемая из функции таблица. 
# Тип нового столбца - character (строка). 
# Записи с неположительным количеством товаров убрать перед расчётом.
mark.position.portion <- function(purchases) {
  purchases[quantity >= 0, 'price.portion' := sprintf("%.2f", round((price * quantity)/sum(price * quantity)*100, 2)), by = 'ordernumber'][!is.na(price.portion)]
}
# vs
mark.position.portion <- function(purchases) {  
  purchases <- purchases[quantity > 0]  
  purchases[, price.portion := format(round(100 * price * quantity / sum(price * quantity), 2),                                       
   nsmall=2,digits=2, scientific = F), by=ordernumber]
}