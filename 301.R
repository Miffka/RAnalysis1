setwd(as.character("C:/Users/miffka/Documents/!DataMining/RAnalysis1"))
getwd()
library(psych)
library(ggplot2)
library(Hmisc)

df <- mtcars
#расчет коэффициента корреляции

#функция принимает на вход только две переменных
# можем указать дополнительно способ расчета корреляции 
# method = kendall - непараметрический критерий
# "pearson" - по умолчанию
cor.test(x = df$mpg, y = df$hp)
fit <- cor.test(x = df$mpg, y = df$hp)
str(fit)

fit$p.value

#бывает запись через формулу
cor.test(~mpg + hp, df)

#посмотрим на распределение
plot(x = df$mpg, y = df$hp)

#строим это в ggplot2 - и задаем переменную как фактор внутри кода плота
ggplot(df, aes(x = mpg, y = hp, col = factor(cyl)))+
  geom_point(size = 5)

#попарное исследование взаимосвязи данных

df_numeric <- df[,c(1,3:7)]

#ПОДГОТОВИТЕЛЬНЫЙ ЭТАП
#построение диаграммы рассеивания для количественных датасетов
pairs(df_numeric)

#функция расчитывает только коэф корр, но зато принимает датасеты
cor(df_numeric)

#не путать с cor.test!
fit2 <- corr.test(df_numeric)
str(fit2)
fit2$r #выводим коэффициен корреляции
fit2$p #и p-уровень значимости

#задача 1

corr.calc <- function(p){
  p <- as.data.frame(p)
  s <- cor.test(x = p[,1], y = p[,2])
  return(c(s$estimate, s$p.value))
}

corr.calc(iris[,1:2])

#задача 2

filtered.cor <- function(p) {
  p <- as.data.frame(p)#делаем входной документ датафреймом
  p1 <- data.frame(c(1:nrow(p)))
  for (i in 1:ncol(p)) { # в цикле отбираем только числовые данные
    if (is.numeric(p[, i])){
      p1 <- cbind(p1, p[, i])
    }
  }
  p1 <- p1[,c(2:ncol(p1))]
  s <- corr.test(p1)
  diag(s$r) <- 0
  n <- which.max(abs(s$r))
  return(s$r[n])
}

#правильное решение
filtered.cor <- function(x){    
  num_var <- sapply(x, function(x) is.numeric(x))    
  cor_mat <- cor(x[, num_var])    
  diag(cor_mat) <- 0    
  return(cor_mat[which.max(abs(cor_mat))])
  }


filtered.cor(mtcars)
filtered.cor(iris)

#задача 3

smart.cor <- function(p){
  p <- as.data.frame(p)
  if (shapiro.test(p[,1])$p.value < 0.05 || shapiro.test(p[,2])$p.value < 0.05) {
    return(cor.test(x = p[, 1], y = p[, 2], method = "spearm")$estimate)
    print("Spearman method is used")
  }
  else{
    return(cor.test(x = p[, 1], y = p[, 2])$estimate)
  }
}

test_data <- as.data.frame(list(col1 = c(1.32, -1.3, 1.44, -0.05, -0.5, -0.13, 1.01, 0.74, 0.32, -1.03, -1.7, -1.24, 1.72, 1.72, 1.25, 0.91, -0.14, -0.7, -0.95, -0.13, 0.43, 1.51, -1.33, -1.42, 0.18, 0.22, -1.88, 0.73, 1.73, 0.24), col2 = c(-1.39, -0.22, -2.53, -0.94, -0.95, -0.14, 1.79, -2.09, -0.17, -1.2, 0.62, -0.98, 0.44, -0.77, 0.98, -0.68, -1.95, -0.22, -0.17, -1.1, 0.02, -2.01, -0.59, 1.3, -0.45, 0.59, 0.01, 0.68, 1.06, -0.82)))

task3 <- mtcars[,c(1,3)]
shapiro.test(task3[, 1])
shapiro.test(task3[, 2])
plot(task3)
cor.test(task3[, 1], task3[, 2])

smart.cor(task3)
task31 <- cor.test(test_data[,1], test_data[,2])
str(task31)

# регрессия 
df2 <- mtcars
df_numeric <- df2[,c(1,3:7)]

# linear model
fit <- lm(mpg ~ hp, df2)
summary(fit) # просматриваем инфу

#строим график
ggplot(df2, aes(hp, mpg))+
  #geom_point(size = 5)+ #можем и вовсе удалять точки, оставляя только линию тренда
  #geom_smooth() # сглаживает тем, чем хочет
  geom_smooth(method = "lm", se = F)+
  facet_grid(.~cyl)

#посмотреть предсказанные значения
fitted_values_mpg <- data.frame(mpg = df2$mpg, fitted = fit$fitted.values)

#предсказать значения для новых данных
new_hp <- data.frame(hp = c(100, 150, 129, 300))

predict(fit, new_hp)

new_hp$mpg <- predict(fit, new_hp)#записываем предсказанные значения во фрейм

#предиктор(нп) - номинативная переменная

my_df <- mtcars
my_df$cyl <- factor(my_df$cyl, labels = c("four", "six", "eigth"))

fit <- lm(mpg ~ cyl, my_df)
summary(fit) #что же выводит наш фит теперь?
str(fit)

ggplot(my_df, aes(cyl, mpg))+
  geom_point()+
  #geom_smooth(method = "lm")+ #при факторах сглаживания нет
  theme(axis.text=element_text(size=25),
        axis.title=element_text(size=25, face="bold"))

aggregate(mpg ~ cyl, my_df, mean)
#видим, что среднее по 4м цилиндрам - это интерсепт
#коэффициенты - это изменение среднего значения при переходе в другую
#градацию фактора

#задача 4

task4 <- read.table("dataset_11508_12.txt")
str(task4)
task41 <- lm(V1 ~ V2, task4)
summary(task41)

#задача 5

task5 <- data.frame(diamonds)
str(task5)
task5 <- subset(task5, cut == "Ideal" & carat == 0.46)
task5_fit <- lm(task5$price ~ task5$depth)
fit_coef <- c(task5_fit$coefficients)
task51 <- lm(task5[,5] ~ task5[,7])

#задача 6

regr.calc <- function(p){
  p <- as.data.frame(p)
  if (cor.test(p[, 1], p[, 2])$p.value < 0.05) {
    p$fit <- lm(p[, 1] ~ p[, 2])$fitted.values
    return(p)
  }
  print("There is no sense in prediction")
}

regr.calc(iris[, c(1,4)])

#задача 7

ggplot(iris, aes(Sepal.Width, Petal.Width, col = Species))+
  geom_point()+
  geom_smooth(method = "lm") 










