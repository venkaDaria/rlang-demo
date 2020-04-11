# Воспользуемся встроенными данными npk, иллюстрирующими влияние применения различных удобрений на урожайность гороха (yield).
# Нашей задачей будет выяснить, существенно ли одновременное применение азота (фактор N) и фосфата (фактор P). 
# Примените дисперсионный анализ, где будет проверяться влияние фактора применения азота (N), влияние фактора 
# применения фосфата (P) и их взаимодействие.
# В ответе укажите p-value для взаимодействия факторов N и P.
summary(aov(yield ~ N*P, data = npk))[[1]][["Pr(>F)"]]

  
# В этой задаче вам дан набор данных, в котором представлена информация о температуре нескольких пациентов, 
# которые лечатся разными таблетками и у разных врачей.
# Проведите однофакторный дисперсионный анализ с повторными измерениями: влияние типа таблетки (pill) 
# на температуру (temperature) с учётом испытуемого (patient). Каково p-value для влияния типа таблеток на температуру?
# 
# Данные: https://stepic.org/media/attachments/lesson/11505/Pillulkin.csv
# 
# Не забудьте, важно перевести переменную patient в фактор!  
df <- read.csv(url('https://stepic.org/media/attachments/lesson/11505/Pillulkin.csv'))
df$patient <- as.factor(df$patient)
summary(aov(temperature ~ pill + Error(patient/pill), data = df))
# vs
summary(aov(temperature ~ pill + Error(as.factor(patient)/pill), 
    data = read.csv(url("https://stepic.org/media/attachments/lesson/11505/Pillulkin.csv"))))
      
# Теперь вашей задачей будет провести двухфакторный дисперсионный анализ с повторными измерениями: 
# влияние факторов doctor, влияние фактора pill и их взаимодействие на temperature. 
# Учтите обе внутригрупповые переменные: и тот факт, что один и тот же больной принимает разные таблетки, 
# и тот факт, что  один и тот же больной лечится у разных врачей! Каково F-значение для взаимодействия 
# факторов доктора (doctor) и типа таблеток (pill)?
# Данные: https://stepic.org/media/attachments/lesson/11505/Pillulkin.csv
df <- read.csv(url('https://stepic.org/media/attachments/lesson/11505/Pillulkin.csv'))
df$patient <- as.factor(df$patient)
summary(aov(temperature ~ pill*doctor + Error(patient/(pill*doctor)), data = df))