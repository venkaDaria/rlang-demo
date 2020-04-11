prop.table(HairEyeColor[ , ,'Male'],2)['Red','Blue']

t1 <- prop.table(HairEyeColor[ , ,'Male'],2)
red_men <- t1['Red','Blue']
print(red_men)

library("ggplot2")
mydata <- as.data.frame(HairEyeColor)
obj <- ggplot(data = subset(mydata, Sex == 'Female'), 
      aes(x = Hair, y = Freq, fill=Eye)) 
      + geom_bar(stat="identity", position=position_dodge()) 
      + scale_fill_manual(values=c("Brown", "Blue", "Darkgrey", "Darkgreen"))

new_tab <- HairEyeColor["Brown",,"Female"]
chisq.test(new_tab)

diamonds_table <- table(diamonds$cut, diamonds$color)    
chi_result <- chisq.test(diamonds_table )    
main_stat <- chi_result$statistic

diamonds_table <- xtabs(~cut+color, data=diamonds)
chi_result <- chisq.test(diamonds_table)
main_stat <- chi_result[1]

diamonds$factor_price <- 1
diamonds[diamonds$price < mean(diamonds$price), ]$factor_price <- 0
diamonds$factor_carat <- 1
diamonds[diamonds$carat < mean(diamonds$carat), ]$factor_carat <- 0
chi_result <- chisq.test(diamonds$factor_carat, diamonds$factor_price)
main_stat <- chi_result[1]

diamonds$factor_price <- factor(ifelse(diamonds$price >= mean(diamonds$price), 1, 0))    
diamonds$factor_carat <- factor(ifelse(diamonds$carat >= mean(diamonds$carat), 1, 0))    
main_stat_master <- chisq.test(diamonds$factor_price, diamonds$factor_carat)$statistic

t <- fisher.test(mtcars$am, mtcars$vs)
fisher_test <- t$p.value