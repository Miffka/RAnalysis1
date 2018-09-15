setwd(as.character("C:/Users/miffka/Documents/!DataMining/RAnalysis1"))
getwd()
library(psych) #для статистики
library(ggplot2) #для графиков
library(Formula) #для survival
library(survival) #для lattice
library(lattice) #для Hmisc
library(Hmisc) #для чего-то нужного
library(boot)
library(QuantPsyc) #для регрессий
library(gvlma)
library(caTools) #для gplots
library(gplots) #для ROCR
library(ROCR) #для логистических регрессий
library(xtable) #для сохранения данных регрессий
library(stargazer) #для сохранения данных регрессий

#пакеты для сохранения данных

install.packages("xtable")
install.packages("stargazer")

fit1 <- lm(mpg ~ cyl + disp, mtcars)
fit2 <- aov(mpg ~ am * vs, mtcars)
summary(fit1)

#функция создает специальный объект в виде таблицы
fit_table1 <- xtable(fit1)
fit_table2 <- xtable(fit2)

print(fit_table1, type = "html", file = "fit_table.html")
print(fit_table2, type = "html", file = "fit_table2.html")

stargazer(fit1, type = "html",
          dep.var.labels = "mpg",
          covariate.labels = c("cyl", "disp", out = "models1.html"))



