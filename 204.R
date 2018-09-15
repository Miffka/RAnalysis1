setwd(as.character("C:/Users/miffka/Documents/!DataMining/RAnalysis1"))
getwd()
library(psych)
library(ggplot2)
library(Hmisc)

# свои функции

my_calc <- function(x, y){ #задаем аргументы в скобках
  s <- x + y #задаем внутренние переменные и пишем тело
  return(s) #возвращаем результат
}

result <- my_calc(x = 10, y = 15)

#функция с несколькими выводными данными пишется с вектором в return!
my_calc1 <- function(x, y){
  s <- x + y #внутренние переменные остаются внутри функции
  d <- x - y #они не меняют внешних
  return(c(s, d))
}

#в определении функции можно задать значение по умолчанию
my_calc3 <- function(x, y, z = 10){ 
  s <- x + y + z
  d <- x - y - z
  return(c(s,d))
}

my_calc3(1,2)

# первая пробная функция
distr1 <- rnorm(100)
hist(distr1)
distr1[1:30] <- NA

#заменяем пропущенные значения на среднее по оставшимся

distr1[is.na(distr1)] <- mean(distr1, na.rm = T)

#а теперь пишем функцию, которая будет делать это со всем, что в нее входит
my_na_rm <- function(x){
  if (is.numeric(x)){
    x[is.na(x)] <- mean(x, na.rm = T)
  return(x)
  }
  else {
    print("X is not numeric")
  }
}

distr1 <- my_na_rm(x = distr1)
hist(distr1)

#что будет, если введем другой тип переменной в функцию?
my_na_rm(x = c("2", "3", NA))
#все плохо
#а когда сделали все по фэн-шую, то все лучше

#модифицируем нашу функцию так, чтобы она в случае распределения, сильно
#отличающегося от нормального, заменяла пропущенные значения не на 
#среднее, а на медиану
#и дописываем, чтобы она выводила сообщение, было ли распределение
#нормальным или не было

my_na_rm <- function(x){
  if (is.numeric(x)){
    stat_test <- shapiro.test(x)
    if (stat_test$p.value > 0.05) {
      x[is.na(x)] <- mean(x, na.rm = T)
      print("NA values were replaced with mean")
    }
    else {
      x[is.na(x)] <- median(x, na.rm = T)
      print("NA values were replaced with median")
    }
  return(x)
  }
  else {
    print("X is not numeric")
  }
}

distr1 <- rnorm(1000)
distr1[1:30] <- NA
distr1 <- my_na_rm(distr1)

d1 <- rnorm(2000)
d2 <- runif(2000) #равномерное распределение

d1[1:10] <- NA
d2[1:10] <- NA

d1 <- my_na_rm(d1)
head(d1)
d2 <- my_na_rm(d2)
hist(d2)

#переносим функцию в отдельный файл и сохраняем там в виде скрипта
#достаем функцией source

source("my_na_rm.R") #можем сдесь прописать полный путь
my_na_rm()

#задача 1 - написать функцию, которая выводит номера позиций проп набл

NA.position <- function(x){
  s <- which(is.na(x))
  return(s)
}

task1 <- rnorm(10)
task1[c(2,4,8)] <- NA

NA.position(task1)

#задача 2 - подсчет пропущенных значений в векторе

NA.counter <- function(x){
  s <- length(which(is.na(x)))
  return(s)
}

NA.counter(task1)

#функция, которая объединяет несколько файлов в один датафрейм

dir(pattern = "*.csv") #выводим все csv

grants <- data.frame()

#сначала просто сделаем
for (i in dir(pattern = "*.csv")){
  temp_df <- read.csv(i)
  grants <- rbind(temp_df, grants)
}

setwd(as.character("C:/Users/miffka/Documents/!DataMining/RAnalysis1/Grants_data"))

read_data <- function(){
  df <- data.frame()
  number <<- 0
  for (i in dir(pattern = "*.csv")){
    df1 <- read.csv(i)
    df <- rbind(df1, df)
    number <<- number + 1
  }
  print(paste(as.character(number), "files were combined"))
  return(df)
}

grants2 <- read_data()

#если хотим, чтобы функция сохраняла внутренние переменные, используем <<-
#можно бы и дополнить эту функцию

#задача 3  
#получает вектор с пропущенными, положительными и отрицательными
#значениями и возвращает сумму положительных элементов вектора

filtered.sum <- function(x){
  s <- sum(x[which(x > 0)], na.rm = T)
  return(s)
}

filtered.sum(c(1, -2, 3, NA, NA))

#задача 4
# написать функцию, которая находит и удаляет выбросы при помощи boxplot
#выброс - наблюдение, отклоняются от квартилей более, чем на 1.5*IQR,
#где IQR - межквартильный размах

task4 <- rnorm(100)
boxplot(task4)
#task4 <- outliers.rm(task4)
#boxplot(task4)


#task41 <- quantile(task4, probs = c(0.25, 0.75))
#task42 <- IQR(task4)

#сначала пропишем все вне функции

#добавим функцию, которая убирает значения одного вектора из другого
exclude_val <- function(full_vector,searched_vector){
  found=c()
  for(i in full_vector){  
    if(any(is.element(searched_vector,i))){
      searched_vector[(which(searched_vector==i))[1]]=NA
    }
    else{
      found=c(found,i)
    }
  }
  return(found)
}

#task43 <- task4[which(task4 < 1.5 * IQR(task4) * quantile(task4, probs = 0.25))]
#task44 <- task4[which(task4 > 1.5 * IQR(task4) * quantile(task4, probs = 0.75))]
#task45 <- append(task43, task44)
#task4 <- exclude_val(task4, task45)
#task4 <- setdiff(task4, task43)
#task4 <- setdiff(task4, task44)

x <- rnorm(100)

task43 <- c(x < 1.5 * IQR(x) + quantile(x, probs = 0.25))
task44 <- c(x > 1.5 * IQR(x) + quantile(x, probs = 0.75))
task45 <- as.logical(task43 + task44)
x1 <- x[!task45]

#теперь запишем в общем виде

outliers.rm <- function(x){
  y <- IQR(x)
  k <- quantile(x, probs = 0.25) - y * 1.5
  l <- y * 1.5 + quantile(x, probs = 0.75)
  task43 <- c(x < k)
  task44 <- c(x > l)
  task45 <- as.logical(task43 + task44)
  x <- x[!task45]
  return(x)
}

#красивое решение
outliers.rm <- function(x){
  q <- quantile(x, 0.25) + quantile(x, 0.75)    
  return(x[abs(x - q/2) <= 2*IQR(x)])
}

#это исходный вектор
t1 <- c(1.34, 0.32, -11.57, 0.32, 0.31, 23.03, 0.14, -1.28, 1.5, -3.01, 1.43, 0.8, 0.32, 0.78, -2.44, 0.28, -3.5, -0.39, -0.18, -0.02, -0.76, 0.42, -2.74, -0.75, -0.98, -60.76, 0.33, 41.99, -1.12, -3.92)
#это правильный ответ
t2 <- c(1.34, 0.32, 0.32, 0.31, 0.14, -1.28, 1.5, -3.01, 1.43, 0.8, 0.32, 0.78, -2.44, 0.28, -3.5, -0.39, -0.18, -0.02, -0.76, 0.42, -2.74, -0.75, -0.98, 0.33, -1.12)
#это мой ответ
t3 <- c(0.32, 0.32, 0.31, 0.14, -1.28, -3.01, 0.8, 0.32, 0.78, -2.44, 0.28, -0.39, -0.18, -0.02, -0.76, 0.42, -2.74, -0.75, -0.98, 0.33, -1.12)

t2 == outlier(t1)

summary(t1)

boxplot(t1)
boxplot(t2)
boxplot(t3)
histogram(t1)
histogram(t2)
histogram(t3)
