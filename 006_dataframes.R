
?read.table

my_data <- read.csv('evals.csv')

head(my_data)
head(my_data, 4)
tail(my_data, 2)

View(my_data)
str(my_data)

summary(my_data)

names(my_data)

Score <- my_data$score
mean(my_data$score)
summary(my_data$score)

my_data$ten_point_scale <- my_data$score*2

my_data$new_variable <- 0
my_data$number <- 1:nrow(my_data)
summary(my_data$number)

nrow(my_data)
ncol(my_data)

my_data$score[1:nrow(my_data)]
my_data[1, 1]
my_data[c(300, 101), 1]
my_data[67,]
my_data[, 6]

my_data[,5:11]
head(my_data[,3:6])

my_data$gender == 'male'
my_data$score[my_data$gender == 'male']

head(my_data[my_data$gender == 'male', 2:4])

subset(my_data, gender == 'female')
head(subset(my_data, gender == 'male'))

head(subset(my_data, score > mean(my_data$score)))

#rbind, cbind

my_data2 <- subset(my_data, gender == 'male')
my_data3 <- subset(my_data, gender == 'female')

my_data4 <- rbind(my_data2, my_data3)

my_data5 <- my_data[,c(2,5,8)]
my_data6 <- my_data4[, c(3,7,22)]

my_data7 <- cbind(my_data5, my_data6)
head(my_data7, 2)

library(help = "datasets")
data(CO2)
help(CO2)

data_CO2 <- CO2

data_mtcars <- mtcars
head(mtcars, 3)

#1
data_mtcars$even_gear <- (data_mtcars$gear + 1) %% 2

head(data_mtcars$even_gear)

#2

mpg_4 <- mtcars$mpg[mtcars[,2] == 4]

mpg_4

#3

mini_mtcars <- mtcars[c(3,7,10,12,nrow(mtcars)),]
mini_mtcars



