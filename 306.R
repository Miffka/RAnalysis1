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

#install.packages("caTools")
#install.packages("gplots")
#install.packages("ROCR")
#логистическая регрессия

#смотрим, как различные переменные влияют на номинативную переменную 
#с двумя градациями
#pi = b0 + b1x1 + b2x2 + ... + bkxk
#основная проблема - справа переменные имеют большой размер, а слева - нет
#подход - нормируем функцию справа
#расчитаем odds - отношение вероятности успеха к вероятности неудачи
#а теперь возьмем логарифм от odds - для вероятности 0,5 получим 0
#строим график вида
#log(odds) = log(p/(1-p)) = b0 + b1x1
#p = exp(b0 + b1x1)/(1 + exp(b0 + b1x1))

my_df <- read.csv("train.csv", sep = ";")
str(my_df)

ggplot(my_df, aes(read, math, col = gender))+
  geom_point()+
  facet_grid(.~hon)+
  theme(axis.text = element_text(size = 25),
        axis.title = element_text(size = 25, face = "bold"))

#пишем функцию glm
fit <- glm(hon ~ read + math + gender, my_df, family = "binomial")
summary(fit)        

#интерпретируем коэффициенты 
#интерсепт - показатель для женщин при условии, что у них нулевые баллы
#это число - это log(odds) - то есть, соотношение p/(1-p) около e^-12
#коэффициенты перед количественными переменными
#при условии пола ж и фиксированных баллов по математике 
#значение log(odds) будет увеличиваться на 0,06677 при увел-и на 1 балл
#переход от женщин к мужчинам понижает log(odds) на 1,18

exp(fit$coefficients) #получаем коэффициенты в линейном виде

head(predict(object = fit))

#переводим логарифмы соотношения вероятностей в вероятности.
head(predict(object = fit, type = "response"))
my_df$prob <- predict(object = fit, type= "response")

#что же нам делать с полученной вероятностью?
#существует масса способов оценить, как ее интерпретировать

#задача 1

str(mtcars)

task1 <- glm(am ~ disp + vs + mpg, mtcars, family = "binomial")
summary(task1)

log_coef <- task1$coefficients

#задача 2

str(ToothGrowth)
obj <- ggplot(data = ToothGrowth, aes(supp, len, fill = factor(dose)))+
  geom_boxplot()

#строим рок-кривые

pred_fit <- prediction(my_df$prob, my_df$hon) #предсказываем
perf_fit <- performance(pred_fit, "tpr", "fpr") #вычисляем тру позитив рэйт
#и фолс позитив рэйт
#строим рок-кривую
plot(perf_fit, colorize = T, print.cutoffs.at = seq(0,1,by = 0.1))
#чем более жесткий критерий мы выбираем, тем ниже фолс позитив рэйт,
#однако тем и ниже тру позитив рэйт
?performance

auc <- performance(pred_fit, "auc") #а также вычисляем площадь под кривой
str(auc)
auc@y.values #площадь под кривой

#какой же брать порог отсечения?
#ищем, как соотносятся чувствительность и специфичность результата

perf3 <- performance(pred_fit, x.measure = "cutoff", measure = "spec")
perf4 <- performance(pred_fit, x.measure = "cutoff", measure = "sens")
perf5 <- performance(pred_fit, x.measure = "cutoff", measure = "acc")

plot(perf3, col = "red", lwd = 2)
#строим график специфичности - то, насколько хорошо мы предсказываем
#отрицательные результаты
plot(add = T, perf4, col = "green", lwd = 2)
#добавим на график чувствительность - то, насколько хорошо удается 
#предсказывать положительные результаты
plot(add = T, perf5, lwd = 2)

legend(x = 0.2, y = 0.3, c("spec","sens","accur"),
       lty = 1, col = c('red', 'green', 'black'), 
       bty = 'n', cex = 0.5, lwd = 2)

#выбираем в качестве порога точку пересечения всех этих линий
abline(v = 0.225, lwd = 2)

my_df$pred_resp <- factor(ifelse(my_df$prob > 0.225, 1, 0), labels = c("N", "Y"))
#добавляем переменную-фактор, которая определяет, получит ли выпускник крдип

my_df$correct <- ifelse(my_df$pred_resp == my_df$hon, 1, 0)

ggplot(my_df, aes(x = prob, fill = factor(correct)))+
  geom_dotplot()+
  theme(axis.text = element_text(size = 25),
       axis.title = element_text(size = 25, face = "bold"))

#можно посчитать процент правильно предсказанных результатов
mean(my_df$correct[my_df$pred_resp == "N"])

#а теперь предсказываем результат для нового сета на основании модели,
#полученной на данных старого сета
test_df <- read.csv("test.csv", sep = ";")

test_df$prob <- predict(fit, newdata = test_df, type = "response")
#response дает вероятность, а не log(odds)
test_df$pred_resp <- factor(ifelse(test_df$prob > 0.225, 1, 0), labels = c("N", "Y"))
test_df$correct <- ifelse(test_df$pred_resp == test_df$hon, 1, 0)

mean(test_df$correct[test_df$pred_resp == "Y"])

#задача 3

task3 <- read.csv("data.csv")
str(task3)
?subset
task31 <- subset(task3, !is.na(task3$admit))

fit_t3 <- glm(admit ~ rank*gpa, task31, family = "binomial")
str(fit_t3)

task3$prob <- predict(object = fit_t3, newdata = task3, type= "response")
task3$pred_resp <- factor(ifelse(task3$prob >= 0.4, 1, 0), labels = c("N", "Y"))
#получили данные по всем
task32 <- subset(task3, is.na(task3$admit))
length(task32$pred_resp[task32$pred_resp == "Y"])
