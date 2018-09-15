setwd(as.character("C:/Users/miffka/Documents/!DataMining/RAnalysis1"))
getwd()
library(psych)
library(ggplot2)
library(Hmisc)


#install.packages("Hmisc")
#install.packages("psych")
#install.packages("ggplot2")

# урок посвящен однофакторному анализу в R - t-test

?iris
df <- iris

str(df)

df1 <- subset(df, Species != "setosa")
str(df1)
table(df1)

# хотим сравнить длину ленестка по двум группам у двух видов

hist(df1$Sepal.Length)

ggplot(df1, aes(x = Sepal.Length))+
  geom_histogram(fill = "white", col = "black", binwidth = 0.4)+
  facet_grid(Species ~ .)

ggplot(df1, aes(Sepal.Length, fill = Species))+
  geom_density(alpha = 0.3)

ggplot(df1, aes(x = Species, y = Sepal.Length))+
  geom_boxplot()

# првоеряем нормальность распределения и гомогенность дисперсии

shapiro.test(df1$Sepal.Length) # тест шапиро-вилка на соответствие распределения норм
s <- shapiro.test(df1$Sepal.Length)
str(s)

shapiro.test(df1$Sepal.Length[df1$Species == "versicolor"])

shapiro.test(df1$Sepal.Length[df1$Species == "virginica"])

# тест барлетта на гомогенность дисперсии в группах
bartlett.test(Sepal.Length ~ Species, df1)

# test itself

t.test(Sepal.Length ~ Species, df1) # команда, разбивающая num по факторам
test1 <- t.test(Sepal.Length ~ Species, df1)
str(test1)
test1$p.value

# additional

t.test(Sepal.Length ~ Species, df1, var.equal = T) # доп смотри в документации

mean(df1$Sepal.Length)

t.test(df1$Sepal.Length, mu = 8) # проверка того, что среднее по выборке - это mu

# сравнение зависимых выборок
# нулевая гипотеза длина и ширина лепестка равны

t.test(df1$Petal.Length, df1$Petal.Width, paired = T)
# важно - для парных сравнений нужно, чтобы две переменные были количественными
# для обычного т-критекрия нужно, чтобы одна была количественной, а вторая - фактором

# задача 1

?ToothGrowth
str(ToothGrowth)

task11 <- ToothGrowth$len[ToothGrowth$supp == "OJ" & ToothGrowth$dose == 0.5]
task12 <- ToothGrowth$len[ToothGrowth$supp == "VC" & ToothGrowth$dose == 2]
str(t.test(task11, task12))
t_stat <- t.test(task11, task12)$statistic

# задача 2

task2 <- read.csv("lekarstva.csv")
str(task2)

t.test(task2$Pressure_before, task2$Pressure_after, paired = T)

# визуализация результатов - доверительные интервалы для среднего значения

ggplot(df1, aes(Species, Sepal.Length))+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar")

# визуализация результатов - добавление свистоперделок, среднего
ggplot(df1, aes(Species, Sepal.Length))+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.1)+
  stat_summary(fun.y = mean, geom = "point", size = 4)

# без дополнительного слоя
ggplot(df1, aes(Species, Sepal.Length))+
  stat_summary(fun.data = mean_cl_normal, geom = "pointrange", size = 2)

# непараметрический аналог Стьюдента - тест Манна-Уитни

?wilcox.test
wilcox.test(Petal.Length ~ Species, df1)
# точно так же можем сохранять как переменную и обращаться к разным частям этой переменной

test2 <- wilcox.test(Petal.Length ~ Species, df1)$statistic

ggplot(df1, aes(Species, Petal.Length))+
  geom_boxplot()

paired_wtest <- wilcox.test(df1$Petal.Length, df1$Petal.Width, paired = T)

# задача 3 проанализировать набор данных и выбрать соответствующую статистику

task3 <- read.table("dataset_11504_15.txt")
str(task3)

bartlett.test(V1 ~ V2, task3)
wilcox.test(V1 ~ V2, task3)

# задача 4

task4 <- read.table("dataset_11504_16.txt")
str(task4)

t.test(task4$V1, task4$V2, var.equal = F)


