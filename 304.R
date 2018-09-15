setwd(as.character("C:/Users/miffka/Documents/!DataMining/RAnalysis1"))
getwd()
library(psych)
library(ggplot2)
library(Hmisc)
library(boot)
library(QuantPsyc)

#install.packages("QuantPsyc")

#диагностика моделей

data(swiss)
str(swiss)

#ищем взаимосвязи между переменными
pairs(swiss)

ggplot(swiss, aes(Examination, Education))+
  geom_point()+
  theme(axis.text = element_text(size = 25),
        axis.title = element_text(size = 25, face = "bold"))

#ищем выбросы и работаем с ними
ggplot(swiss, aes(Examination, Education))+
  geom_point()+
  theme(axis.text = element_text(size = 25),
        axis.title = element_text(size = 25, face = "bold"))+
  geom_smooth(method = "lm")

#нормальность распределения переменных
ggplot(swiss, aes(x = Examination))+
  geom_histogram()
ggplot(swiss, aes(Education))+
  geom_histogram() #так себе нормальность

#попытаемся нормализовать распределение способом преобразования предиката
#в квадрат, логарифм или что-нибудь еще
ggplot(swiss, aes(x = log(Education)))+
  geom_histogram()

#задача 1 - преобразование переменной

my_vector <- c(0.027, 0.079, 0.307, 0.098, 0.021, 0.091, 0.322, 0.211, 0.069, 0.261, 0.241, 0.166, 0.283, 0.041, 0.369, 0.167, 0.001, 0.053, 0.262, 0.033, 0.457, 0.166, 0.344, 0.139, 0.162, 0.152, 0.107, 0.255, 0.037, 0.005, 0.042, 0.220, 0.283, 0.050, 0.194, 0.018, 0.291, 0.037, 0.085, 0.004, 0.265, 0.218, 0.071, 0.213, 0.232, 0.024, 0.049, 0.431, 0.061, 0.523)

shapiro.test(my_vector)
shapiro.test(1/my_vector)
shapiro.test(sqrt(my_vector)) #подходит
shapiro.test(log(my_vector))

#задача 2 - написать функцию, которая получает датафрейм с двумя кол-ми
#переменными, а возвращает стандартизованные коэффициенты для регрессионной
#модели, в которой первая переменная - зависима, а вторая - независима

task2 <- lm(mtcars[,1] ~ mtcars[,3])
summary(task2)
str(task2)

beta.coef <- function(x) {
  return(lm(scale(x[,1]) ~ scale(x[,2]))$coefficients)
}
beta.coef(mtcars[,c(1,3)])
beta.coef(swiss[,c(1,4)])

str(mtcars)

#функция, которая выдает нормализованные коэффициенты зависимости!
lm1 <- lm(mpg ~ disp, mtcars)
lm.beta(lm1)

#задача 3 - написать функцию, которая получает датафрейм, проверяет
#распределение каждой переменной на нормальность по shapiro.test и
#возвращает вектор с p-value, полученными в результате проверки на
#нормальность каждой переменной. Названия должны совпадать с названиями
#переменных

?apply
?sapply

task32 <- function(x) {
  return(shapiro.test(x)$p.value)
}
normality.test <- function(x) {
  return(apply(x, 2, task32))
}

normality.test(iris[,-5])
normality.test(mtcars[, 1:6])

#верное решение 

normality.test  <- function(x){    
  return(sapply(x, FUN =  shapiro.test)['p.value',])}