avian <- read.csv("avianHabitat.csv")
colsHt <- names(avian)[grepl("Ht", names(avian))]
maxHt <- apply(X = avian[, colsHt], 2, max)
maxes <- sort(maxHt)
print(maxes)

print(avian[avian$DBHt == maxes["DBHt"], ]["Observer"])
print(avian[avian$WHt == maxes["WHt"], ]["Observer"])
print(avian[avian$EHt == maxes["EHt"], ]["Observer"])
print(avian[avian$AHt == maxes["AHt"], ]["Observer"])
print(avian[avian$HHt == maxes["HHt"], ]["Observer"])
print(avian[avian$LHt == maxes["LHt"], ]["Observer"])