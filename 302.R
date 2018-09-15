setwd(as.character("C:/Users/miffka/Documents/!DataMining/RAnalysis1"))
getwd()
library(psych)
library(ggplot2)
library(Hmisc)

#множественная линейная регрессия

?swiss
str(swiss)
swiss <- data.frame(swiss)

hist(swiss$Fertility, col = "green")

#первая модель - рождаемость от физподготовки и католицизма

#сначала - просто посмотрим все по функции lm
fit <- lm(Fertility ~ Examination + Catholic, data = swiss)
summary(fit)

#посмотрим взаимодействие переменных

fit2 <- lm(Fertility ~ Examination * Catholic, swiss)
summary(fit2)

#взаимодействие факторов незначимо

#доверительные интервалы для модели
confint(fit2)

#задача 1
?lm

fill_na <- function(x) {
  fit_t1 <- lm(y ~ x_1 + x_2, x, na.action = "na.omit")
  x$y_full <- x$y
  x$y_full[is.na(x$y_full)] <- predict(fit_t1, subset(x, is.na(x$y)))
  return(x)
}

#правильное решение
fill_na <- function(my_df){    
  fit <- lm(y ~ x_1+x_2, my_df)    
  my_df$y_full = ifelse(is.na(my_df$y), predict(fit, my_df), my_df$y)    
  return(my_df)
}

#задача 2 - подобрать оптимальное сочетание факторов

str(mtcars)
task2 <- mtcars[c(1,3:6)]

fit_t21 <- lm(wt ~ mpg + disp + drat + hp, task2)
summary(fit_t21)
r_21 <- c()
r_21[1] <- 0.8374


#удаляем drat
fit_t22 <- lm(wt ~ mpg + disp + hp, task2)
summary(fit_t22)
r_21[2] <- 0.8428 #это правильное сочетание!

#удаяем hp
fit_t23 <- lm(wt ~ mpg + disp, task2)
summary(fit_t23)
r_21[3] <- 0.8242

#задача 3

task3 <- lm(rating ~ complaints * critical, attitude)
summary(task3)

#линейная регрессия с категориальными предикторами

swiss <- data.frame(swiss)
swiss$religious <- ifelse(swiss$Catholic > 60, "Lots", "Few")
swiss$religious <- as.factor(swiss$religious)

fit3 <- lm(Fertility ~ Examination + religious, swiss)
summary(fit3)
#теперь в интерсепте хранится предсказанное значение для первого уровня (Few)
#категориальной переменной при всех непрерывных переменных, равных 0
#коэффициент перед examination говорит о том, насколько изменяется 
#рождаемость для значения фактора Few
#коэффициент religiousLots говорит, насколько изменяется рождаемость при
#переходе от провинций Few к провинциям Lots

#перейдем к взаимодействию
fit4 <- lm(Fertility ~ Examination * religious, swiss)
summary(fit4)
#коэффициент intersept - значение рождаемости для провинций с фактором Few
#и значением examination 0
#коэффициент при физической подготовке - изменение рождаемости при
#изменении этого предиктора при условии фактора few
#коэффициент religiouslots говорит об изменении рождаемости при переходе
#от фактора few к фактору lots при постоянных остальных предикторах
#в данном случае он не является значимым
#коэффициент examination:religiousLots говорит об изменении уровня 
#рождаемости при изменеии предиктора examination при факторной 
#переменной, равной lots

#перестановка предикторов помогает


#графики зависимостей

ggplot(swiss, aes(Examination, Fertility))+
  geom_point()+
  geom_smooth(method = "lm")

ggplot(swiss, aes(Examination, Fertility, col = religious))+
  geom_point()+
  geom_smooth(method = "lm")

#несколько непрерывных предикторов и один категориальный

fit5 <- lm(Fertility ~ religious*Infant.Mortality*Examination, swiss)
summary(fit5)

#задача 4 и 5

task4 <- data.frame(mtcars)
task4$am <- factor(task4$am, labels = c("Automatic", "Manual"))

fit_t4 <- lm(mpg ~ am * wt, task4)
summary(fit_t4)

#задача 6

ggplot(task4, aes(wt, mpg, col = am))+
  geom_smooth(method = "lm")



