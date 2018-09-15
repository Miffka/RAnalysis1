setwd(as.character("C:/Users/miffka/Documents/!DataMining/RAnalysis1"))
getwd()
library(psych)
library(ggplot2)
library(Hmisc)

# урок о дисперсионном анализе

# тильда служит для агрегирования и - здесь - для указания связи м-ду переменными

DV ~ IV #на зависимую переменную DV влияет одна независимая переменная IV
DV ~ IV1 + IV2 #влияют две переменные, причем по отдельности
DV ~ IV1:IV2 #влияние одной нп на зп зависит от уровня второй нп
DV ~ IV1 + IV2 + IV1:IV2 #формула с главными эффектами плюс взаимодействие
DV ~ IV1 * IV2 #то же самое, что предыдущая формула
DV ~ IV1 + IV2 + IV3 + IV1:IV2 #можно добавлять сколько угодно чего угодно
DV ~ (IV1 + IV2 + IV3)^2 #главные эффекты плюс все возможные взаимодействия
DV ~ IV1 + Error(subject/IV1) #DV здесь м/групповая переменная, а subject - внутригрупповая, и мы указываем влияние 

# начинаем сравнивать на примере одного из сетов

mydata <- read.csv('shops.csv')
str(mydata)

# однофакторный ANOVA - строим графики 

boxplot(price ~ origin, data = mydata)

ggplot(mydata, aes(x = origin, y = price))+
  geom_boxplot()

fit1 <- aov(price ~ origin, data = mydata) # делаем анализ
summary(fit1) # просматриваем результаты анализа

# на основании данных отвергаем гипотезу о том, что цены не зависят от страны происхождения

# двухфакторный ANOVA

fit2 <- aov(price ~ origin + store, data = mydata)
summary(fit) # страна производства значимо превышает цену, а вот магазин - нет

model.tables(fit2, "means") # просматриваем инфу по таблице

# анализ взаимодействия факторов

# здесь пришлось заглянуть в документацию для position

ggplot(mydata, aes(store, price, col = origin, group = origin))+
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, lwd = 0.8, position = position_dodge(width = 0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = "line", size = 1.5, position = position_dodge(width = 0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = "point", size = 5, position = position_dodge(width = 0.2), pch = 15)+
  theme_classic()

# что же мы видим - продукты из российских магазинов в минимаркетах стоят дешевле,
# чем в супермаркетах, а импортные продукты в минимаркетах стоят дороже, чем 
# в супермаркетах

fit3 <- aov(price ~ origin + store + origin:store, data = mydata)
summary(fit3)

# страна производства значима, тип магазина незначим, взаимодействие 
# страны производства и типа магазина значимо

fit4 <- aov(price ~ origin * store, data = mydata)
summary(fit4)

# задача 2 - существенно ли одновременное применение азота(N) и фосфата(P)

str(npk)
task2 <- aov(yield ~ P * N, data = npk)
summary(task2)

# задача 3 - влияние типов удобрений

str(npk)
task3 <- aov(yield ~ (N + P + K) ^ 2, data = npk) # со взаимодействием
summary(task3)

task32 <- aov(yield ~ N + P + K, data = npk)
summary(task32)

# а теперь прмеры с несколькими уровнями фактора и поправками на множественное сравнение

str(mydata)

ggplot(data = mydata, aes(x = food, y = price))+
  geom_boxplot()

fits5 <- aov(price ~ food, data = mydata)
summary(fits5)

# видим, что фактор еда является значимым для переменной цена, но вот какая еда?

# сравнение с поправкой Тьюки - она менее строга, чем Бонферрони 
TukeyHSD(fits5)

#во всех случаях, кроме различия хлеб-сыр, мы не можем отвергнуть нулевую гипотезу
# только различие сыр-хлеб является значимым

# задача 4 - попарные сравнения Sepal.Width по видам

str(iris)

task4 <- aov(Sepal.Width ~ Species, data = iris)
summary(task4)
TukeyHSD(task4)

boxplot(Sepal.Width ~ Species, data = iris)

# анализ с повторными измерениями

mydata2 <- read.csv("therapy_data.csv")
str(mydata2)

mydata2$subject <- as.factor(mydata2$subject) #делаем испытуемых фактором

fit6a <- aov(well_being ~ therapy, data = mydata2) #сначала строим обычное
summary(fit6a) #различия вроде бы незначимы

#а теперь вносим поправку
fit6b <- aov(well_being ~ therapy + Error(subject/therapy), data = mydata2)
summary(fit6b)

#в первом блоке данные негруппированные
#во втором - терапия с учетом испытуемого, и различия все так же незначимы
#модели отличаются по степеням свободы - в первой 1 и 27, во втором - 1 и 8

#сначала строим влияние сочетания терапии и цены без учета поправки
fit6c <- aov(well_being ~ therapy * price, data = mydata2)
summary(fit6c)

# цена - единственный значимый фактор

ggplot(mydata2, aes(x = price, y = well_being))+
  geom_boxplot()

# строим модель с поправкой
fit6d <- aov(well_being ~ therapy * price + Error(subject/(therapy * price)), data = mydata2)
summary(fit6d)
#а теперь получаем, что различия на уровне статистической тенденции

ggplot(mydata2, aes(x = price, y = well_being))+
  geom_boxplot()+
  facet_grid(~ subject) #этот слой дает разбивку по параметру
# и мы видим, что не у всех испытуемых цена оказала влияние

#а теперь возьмем ситуацию, когда есть межгрупповые факторы, которые можно разбить
#по группам пациентов, и внутригрупповые - вроде пола

#ВАЖНО - в ошибку добавляем только МЕЖГРУППОВЫЕ факторы, внутригрупповые (пол) - нет
fit6e <- aov(well_being ~ therapy* price * sex + Error(subject/(therapy*price)), data = mydata2)
summary(fit6e)
#при учитывании дисперсии, связанной с испытуемыми, 
#что мы видим - влияние пола(subject) незначимо
#влияние терапии и цены, сгруппированных с испытуемыми по отдельности, тоже незн
#трехфакторное взаимодействие оказывается незначимым

# задача 5 - влияние типа таблетки на температуру с учетом испытуемого

task5 <- read.csv("Pillulkin.csv")
str(task5)

task5$patient <- as.factor(task5$patient)
str(task5)

task51 <- aov(temperature ~ pill + Error(patient/pill), data = task5)
summary(task51)

# задача 6 - влияние факторов доктор и пилл с учетом поправки на испытуемого

task6 <- aov(temperature ~ pill*doctor + Error(patient/(pill*doctor)), data = task5)
summary(task6)

# задача 7 - шаблон для графика дополняем линией, соединяющей точки

#это аргумент group в аргументе aes!!!!
str(ToothGrowth)
obj <- ggplot(ToothGrowth, aes(x = as.factor(dose), y = len, col = supp, group = supp))+
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width = 0.1, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'point', size = 3, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'line', position = position_dodge(0.2))
obj + theme_bw()
