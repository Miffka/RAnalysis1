setwd(as.character("C:/Users/miffka/Documents/!DataMining/RAnalysis1"))
getwd()
library(psych)
library(ggplot2)
library(Hmisc)

#отбор моделей

swiss <- data.frame(swiss)

fit_full <- lm(Fertility ~ ., swiss) #точка в области предикторов - 
#предсказание от всех предикторов в этом датасете
summary(fit_full)
#оказывается, что examination не влияет на рождаемость!

fit_reduced <- lm(Fertility ~ Infant.Mortality + Examination + Catholic + Education, swiss)
#убираем agriculture
summary(fit_reduced)
#и снова физическая подготовка незначима!
#однако R^2 во втором случае меньше

#можем сравнить долю дисперсии, которой объясняется зависимость
anova(fit_full, fit_reduced)
#доля дисперсии, объясняемая полной моделью, значимо больше, чем доля
#дисперсии, объясняемая усеченной моделью - нужно предпочесть полную

anova(fit_reduced, fit_full)

#еще одна усеченная модель
fit_reduced2 <- lm(Fertility ~ Infant.Mortality + Education + Catholic + Agriculture, swiss)
summary(fit_reduced2)

anova(fit_full, fit_reduced2)
#две модели примерно одинаково объясняют долю дисперсии

#автоматический отбор моделей

optimal_fit <- step(fit_full, direction = 'backward')
summary(optimal_fit)

#задача 1 - подобрать оптимальную модель
#датасет attitude, модель для построения rating от всего остального

model_full <- lm(rating ~ ., attitude)
summary(model_full)
model_null <- lm(rating ~ 1, attitude)
summary(model_null)

#новый аргумент в функции step - scope, позволяет искать оптимальную 
#модель в пространстве, задаваемом двумя границами

ideal_model <- step(model_null, scope = list(lower = model_null, upper = model_full), direction = "forward")
summary(ideal_model)

task1 <- step(model_full, scope = list(lower = model_null, upper = model_full), direcion = 'backward')

#задача 2 - сравнение в анова

anova(model_full, ideal_model)

#задача 3 - предсказываем значение sr на основе остальных 
#датасет LifeCycleSavings

task3 <- lm(sr ~ pop15*pop75*dpi*ddpi, LifeCycleSavings)
summary(task3)
#не то, нужны только взаимодействия второго уровня

task31 <- lm(sr ~ (.)^2, LifeCycleSavings)
summary(task31)



LifeCycleSavings
