setwd(as.character("C:/Users/miffka/Documents/!DataMining/RAnalysis1"))
getwd()
library(psych)
library(ggplot2)
library(Formula)
library(survival)
library(lattice)
library(Hmisc)
library(boot)
library(QuantPsyc)
library(gvlma)

install.packages("gvlma")
#диагностика модели - продолжение

#допущение линейности взаимосвязи

swiss <- data.frame(swiss)

ggplot(data = swiss, aes(Examination, Education))+
  geom_point()+
  geom_smooth()

lm1 <- lm(Education ~ Examination, swiss)
summary(lm1)

#строим полиномиальную модель
swiss$Examination_squared <- (swiss$Examination)^2

lm2 <- lm(Education ~ Examination + Examination_squared, swiss)
summary(lm2)

anova(lm2, lm1)

#добавим инфу в датасет

swiss$lm1_fitted <- lm1$fitted
swiss$lm2_fitted <- lm2$fitted
swiss$lm1_resid <- lm1$resid
swiss$lm2_resid <- lm2$resid
swiss$obs_number <- 1:nrow(swiss)

ggplot(swiss, aes(Examination, Education))+
  geom_point(size = 3)+
  geom_line(aes(Examination, lm1_fitted), col = "red", lwd = 1)+
  geom_line(aes(Examination, lm2_fitted), col = "blue", lwd = 1)

ggplot(swiss, aes(x = lm1_fitted, y = lm1_resid))+
  geom_point(size = 3)  +  geom_hline(aes(yintercept = 0), col = "red", lwd = 1)

ggplot(swiss, aes(x = lm2_fitted, y = lm2_resid))+
  geom_point(size = 3)  +  geom_hline(aes(yintercept = 0), col = "red", lwd = 1)

#остатки во второй модели распределены более равномерно

#оцениваем нарушение независимости остатков

ggplot(swiss, aes(x = obs_number, y = lm1_resid))+
  geom_point(size = 3) + geom_smooth()
ggplot(swiss, aes(x = obs_number, y = lm2_resid))+
  geom_point(size = 3) + geom_smooth()
 
#идеальная картина - прямая линия

#гомоскедастичность

ggplot(swiss, aes(x = lm1_fitted, y = lm1_resid))+
  geom_point(size = 3)

ggplot(swiss, aes(lm2_fitted, y = lm2_resid))+
  geom_point(size = 3)

#задача 1

task1 <- read.csv("homosc.csv")

#функция позволяет получить оценку выполнения основных допущений линейной
#регрессии
?gvlma

str(task1)
task11 <- gvlma(DV ~ IV, data = task1)
summary(task11)


#нормальность распределения остатков

#линейная модель
ggplot(swiss, aes(lm1_resid))+
  geom_histogram(binwidth = 4, fill = "white", col = "black")

qqnorm(lm1$residuals)
qqline(lm1$residuals)

shapiro.test(lm1$residuals)

#квадратичная модель
ggplot(swiss, aes(lm2_resid))+
  geom_histogram(binwidth = 4, fill = "white", col = "black")

qqnorm(lm2$residuals)
qqline(lm2$residuals)

shapiro.test(lm2$residuals)
  
#задача 2 - функция тестирует распределение остатков модели на нормальность
#и создает гистограмму при помощи ggplot, с зависимость от распределения
#остатков
#ненормальное - заливка "red", нормальное - заливка "green"

resid.norm <- function(x){
  u <- as.data.frame(x$residuals)
  return(ggplot(u, aes(x$residuals)) + geom_histogram(fill = ifelse(shapiro.test(u[,1])$p.value < 0.05, "red", "green")))
}

?ifelse
task21 <- lm(mpg ~ disp, mtcars)
task22 <- lm(mpg ~ wt, mtcars)

resid.norm(task22)

#задача 3 - проверка данных на мультиколлинеарность

high.corr <- function(x){    
  cr <- cor(x)    
  diag(cr) <- 0    
  return(rownames(which(abs(cr)==max(abs(cr)),arr.ind=T)))
  }

cor(swiss)

#решение задачи на величину максимального коэффициента корреляции
high.corr <- function(x){    
  num_var <- sapply(x, function(x) is.numeric(x))    
  cor_mat <- cor(x[, num_var])    
  diag(cor_mat) <- 0
  u <- which(abs(cor_mat) == max(abs(cor_mat)), arr.ind = TRUE)
 return(rownames(u))
}

num_var <- sapply(swiss, function(swiss) is.numeric(swiss)) #отбираем числовые   
cor_mat <- cor(swiss[, num_var]) #строим таблицу коэффициентов корреляции   
diag(cor_mat) <- 0 #убираем диагональные элементы
u <- which(abs(cor_mat) == max(abs(cor_mat)), arr.ind = TRUE) #ищем
#тот индекс массива, к которому относится максимальное значение 
#в массиве

high.corr(swiss)
high.corr(iris)
high.corr(mtcars)
