s1 <- subset(ToothGrowth,ToothGrowth$supp=='OJ'& ToothGrowth$dose==0.5)
s2 <- subset(ToothGrowth,ToothGrowth$supp=='VC'& ToothGrowth$dose==2)
test <- t.test(s1$len, s2$len)
t_stat <- test[1]

correct_data <- subset(ToothGrowth, supp=='OJ' & dose==0.5 | supp=='VC' & dose==2)    
t_stat <- t.test(len ~ supp, correct_data)$statistic

setwd("D:/Google Drive/My Documents/MyPrograms/R/Stepik - 2")
mydata <- read.csv('lekarstva.csv')
test <- t.test(mydata$Pressure_before, mydata$Pressure_after, paired = T)
test[1]

tab <- read.table("dataset_11504_15.txt")
bartlett.test(V1 ~ V2, tab)
wilcox.test(V1 ~ V2, tab)
t.test(V1 ~ V2, tab, var.equal = TRUE)

tab <- read.table("dataset_11504_16.txt")
t.test(tab$V1, tab$V2)