?mtcars
mtcars
data.frame(mtcars)
MT <- data.frame(mtcars)


str(MT)

MT$vs <- factor(MT$vs, labels = c("V", "S"))
str(MT)


MT$am <- factor(MT$am, labels = c("Auto", "Manual"))
str(MT)

?mtcars

median(MT$mpg)
mean(MT$mpg)
sd(MT$mpg)
range(MT$mpg)

mean_disp <- mean(MT$disp)

mean(MT$mpg[MT$cyl == 6])

mean(MT$mpg[MT$cyl == 6 & MT$vs == "V"])

sd(MT$hp[MT$cyl != 3 & MT$am == "Auto"])

# task 1

result <- mean(mtcars$qsec[mtcars$cyl != 3 & mtcars$mpg > 20])

# aggregate

?aggregate
mean_hp_vs <-  aggregate(x = MT$hp, by = list(MT$vs), FUN = mean)

colnames(mean_hp_vs) <- c("VS", "mean_hp")

aggregate(hp ~ vs, MT, mean)

aggregate(hp ~ vs + am, MT, mean)
aggregate(x = MT$hp, by = list(MT$vs, MT$am), FUN = mean)

aggregate(x = MT[, -c(8,9)], by = list(MT$am), FUN = median)

aggregate(x = MT[,c(1,3)], by = list(MT$vs, MT$am), FUN = sd)

aggregate(cbind(hp, disp) ~ vs + am, MT, sd)
str(MT)

cbind(MT$mpg, MT$disp)

my_stats <- aggregate(cbind(hp, disp) ~ vs + am, MT, sd)

# task 2

descriptions_stat <- aggregate(cbind(hp, disp) ~ am, mtcars, sd)

# section 3

library(psych)
library(ggplot2)

?describe
describe(x = MT)
str(MT)

descr <- describe(x = MT[,-c(8,9)])

decr2 <- describeBy(x = MT[,-c(8,9)], group = MT$vs)

decr2$V

descr2 <- describeBy(x = MT[,-c(8,9)], group = MT$vs, mat = TRUE, digits = 1)

descr3 <- describeBy(x = MT[,-c(8,9)], group = MT$vs, mat = TRUE, digits = 1, fast = TRUE)

describeBy(MT$qsec, group = list(MT$vs, MT$am), mat = T, digits = 1, fast = T)

# пропущенные наблюдения

is.na(MT)
sum(is.na(MT))

MT$mpg[1:10] <- NA

mean(MT$mpg)

mean(MT$mpg, na.rm = TRUE)

aggregate(mpg ~ am, MT, sd)

# task 4

AQ <- data.frame(airquality)

?airquality

str(AQ)
head(AQ)

x <- 5
x %in% c(3, 4, 5)

?subset.data.frame

AQ_sub <- subset.data.frame(airquality, airquality$Month %in% c(7,8,9))
result <- aggregate(Ozone ~ Month, AQ_sub, FUN = length)

# task 5

describeBy(airquality[,c(1:4)], group = airquality$Month, digits = 1)

# task 6

?iris

ir <- data.frame(iris)

?aggregate

sd(ir$Sepal.Length)
sd(ir$Sepal.Width)
sd(ir$Petal.Length)
sd(ir$Petal.Width)

# task 7

describe(ir[,c(1:4)])

#task 8

?replace

one <- rnorm(30)

one[sample(1:30, 10)] <- NA

two <- replace(x = one, list = is.na(one), values = mean(one, na.rm = TRUE))

?na.exclude()

?mean



