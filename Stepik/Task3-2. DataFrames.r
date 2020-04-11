# Task 2-2-10
avian <- read.csv("avianHabitat.csv")

avian2 <- read.csv("avianHabitat2.csv", skip = 5, sep = ';', comment.char = '%', na.strings = c("Don't remember"))
avian2$Observer <- 'KL'
#reorder as avian
avian2 <- avian2[, colnames(avian)]
#merge together
avian <- rbind(avian, avian2)

colsHt <- names(avian)[grepl("P", names(avian))]
print(avian[, colsHt])
av <- rowSums(avian[, colsHt])
print(sum(av)/1088)
