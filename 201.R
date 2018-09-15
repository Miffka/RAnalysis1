setwd(as.character("C:/Users/miffka/Documents/!DataMining/RAnalysis1"))
getwd()
library(psych)
library(ggplot2)

df <- read.csv("grants.csv")

str(df)

df$status <- as.factor(df$status) # превращаем интежер в фактор
levels(df$status) <- c("Not funded", "Funded")# присваиваем осмысленные значения
levels(df$status)

# делаем то же самое одной строкой
df$status <- factor(df$status, labels = c("Not funded", "Funded"))

t1 <- table(df$status) # ищем количество поддержаных и неподдержаных
t1

# размерность таблицы
dim(t1)
help(dim)

t2 <- table(df$status, df$field)# создаем двумерную таблицу
t2

dim(t2)
t2 <- table(status = df$status, field = df$field) # присваиваем имена столбцам и строкам
t2
dim(t2)

# пропорциональное распределение по столбцам
prop.table(t2)
prop.table(t2, 1)# сумма в строках равна 100%
prop.table(t2, 2)# сумма в столбцах равна 100%

# 3d table
t3 <- table(Years = df$years_in_uni, Fiels = df$field, Status = df$status)
t3
dim(t3)

# задача 1 - процент рыжих голубоглазых мужчин от всех голубоглазых мужчин

HairEyeColor[ , ,"Male"]

red_men <- prop.table(HairEyeColor[, "Blue", "Male"])[3]

# задача 2 - сумма зеленоглазых женщин

task2 <- sum(HairEyeColor[ , "Green","Female"])

# графики номинативных данных

barplot(t1)
barplot(t2)
# ДОПОЛНИТЕЛЬНЫЕ АРГУменты для легенды и отображения столбцов рядом
barplot(t2, legend.text = TRUE, args.legend = list(x = "topright"), beside = TRUE)

mosaicplot(t2)

# задача 3 гистограмма распр-я цвета глаз по цвету волос у женщин

library(ggplot2)

task3 <- as.data.frame(HairEyeColor) #делаем таблицу датафреймом

# при помощи сабсета выбираем только женщин, НЕ в предыдущем шаге!
obj <- ggplot(data = subset.data.frame(task3, Sex == "Female"), aes(x = Hair, y = Freq, fill = Eye))+
  geom_bar(stat = "identity", position = "dodge")+
  scale_fill_manual(values = c("Brown", "Blue", "Darkgrey", "Darkgreen"))
obj

help(geom_bar)
help("subset.data.frame")
subset.data.frame(task3, Sex == "Female")

# биномиальный тест
binom.test(x = 5, n = 20, p = 0.5)
binom.test(t1)# соотношение одобренных и неодобренных

#хи-квадрат

chisq.test(t1)
chi <- chisq.test(t1)
chi$expected
chi$observed

chisq.test(t2)
t2

# Точный критерий Фишера - если одна из ячеек таблицы пуста

fisher.test(t2)

# задача 4

task4 <- HairEyeColor["Brown", ,"Female"]
task4

chisq.test(task4)

# задача 5

task5 <- table(cut = diamonds$cut, color = diamonds$color)
task51 <- chisq.test(task5)
main_stat <- task51$statistic

help("chisq.test")

# задача 6 добавление факторной переменной через ifelse

diamonds$factor_price <- factor(ifelse(diamonds$price < mean(diamonds$price), 0, 1))
diamonds$factor_carat <- factor(ifelse(diamonds$carat < mean(diamonds$carat), 0, 1))

task61 <- chisq.test(diamonds$factor_price, diamonds$factor_carat)$statistic


#diamonds$price[c(diamonds$price > mean(diamonds$price))]
#mean(diamonds$price)
#mean(diamonds$carat)
#str(diamonds)
#help("ifelse")

# задача 7

str(mtcars)

fisher_test <- fisher.test(mtcars$am, mtcars$vs)$p.value
str(fisher.test(mtcars$am, mtcars$vs))
    