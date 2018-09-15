# i say, we wrote the script and want to save it
setwd("~/R") # в самом начале ставим директорию

library(ggplot2)#затем подключаем небходимые пакеты
library(psych)

df  <- mtcars

mean_mpg  <- mean(df$mpg)

descr_df  <- describe(df[,-c(8,9)])

my_boxplot  <- ggplot(df, aes(x = factor(am), y = disp))+
  geom_boxplot()+
  xlab("Transmission")+
  ylab("Displacement")+
  ggtitle("My boxplot")

my_boxplot

# сохранять скрипты можно через save, как обычн
# сохранение данных идет по умолчанию в рабочую директорию

getwd() # находим путь к рабочей директории

setwd("~/R/RCourse") # какбы эту команду нужно помещать в начало скрипта, чтобы все сохранялось в нужной папке, но сейчас она не работает
#help(setwd)

# сохранение графико можно делать через export - save as image

# сохраняем данные датафреймов в формате csv
write.csv(df, "mtcars.csv")

# сохраняем в формане csv результаты анализа
write.csv(descr_df, "descr_df.csv")

#сохранение переменной 
my_mean  <- mean(10^6 : 10^7)
save(my_mean, file = "my_mean.RData") # название файла, файл и его имя с расширением
#можем сохранять все Окружение через сэйв
#