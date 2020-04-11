library(data.table)
df <- fread("https://raw.githubusercontent.com/tonytonov/Rcourse/master/R%20markdown/demos/glacier.csv", sep=',')
summary(df)
sum(df$GEO == "Helm Glacier - southern Coast Mountains (Garibaldi Provincial Park), British Columbia")
sum(df$GEO == "Peyto Glacier - Rocky Mountain eastern slopes (Banff National Park), Alberta")
sum(df$GEO == "Place Glacier - southern Coast Mountains, British Columbia")
sum(df$GEO == "Devon Ice Cap NW - Devon Island, Nunavut")
sum(df$GEO == "Meighen Ice Cap - Meighen Island, Nunavut")
sum(df$GEO == "White Glacier - Axel Heiberg Island, Nunavut")
# vs
ice <- fread("https://raw.githubusercontent.com/tonytonov/Rcourse/master/R%20markdown/demos/glacier.csv", sep=',')
# 1. 
tapply(ice$Ref_Date, ice$GEO, range)
# 2. 
tapply(ice$Value, interaction(ice$GEO, ice$MEASURE), function(x) median(x, na.rm = T))
# 3. 
tapply(ice$Value, ice$GEO, anyNA)
# vs 
glacier <- fread("https://raw.githubusercontent.com/tonytonov/Rcourse/master/R%20markdown/demos/glacier.csv", sep=',')
# 1. 
aggregate(Ref_Date~GEO, glacier, length)
# 2.
aggregate(Value~GEO, glacier[glacier$MEASURE=='Annual mass balance',], function(x) median(x, na.rm = T))
# 3. 
aggregate(is.na(Value)~GEO, glacier, sum) 