df <- mtcars
df$vs <- factor(df$vs , labels = c("V", "S"))
df$am <- factor(df$am , labels = c("Auto", "Manual"))

hist(df$mpg)
hist(df$am)
hist(df$mpg, breaks = 20, xlab = "MPG")

boxplot(mpg ~ am, df, ylab = "MPG")

plot(df$mpg, df$hp)

plot(df$mpg, df$am) #есть дополнительные аргументы, но они не здесь

# а теперь прогуляемся по функциям ggplot2

library(ggplot2)

ggplot(df, aes(x = mpg))+ 
  # geom_histogram(fill = "white", col = "black", bindwigth = 2)
  #geom_dotplot()
  #geom_density(fill = "red", col = "yellow")

# раскрашиваем наблюдаемые величины в зависимости от значения
  # связанной номинативной переменной
ggplot(df, aes(x = mpg, fill = am))+
  geom_dotplot()

ggplot(df, aes(x = mpg, fill = am))+
  geom_density(alpha = 0.5) # строим график плотности с прозрачностью 0,5

# если хотим, чтобы к-л характеристика зависела от другой переменной,
# пишем ее в fill
# эту часть функции ggplot можно выносить в geom, но обязательно указывать aes
ggplot(df, aes(x = mpg, fill = vs))+
  geom_density(alpha = 0.1) # записали в аргументы ggplot

ggplot(df, aes(x = mpg))+
  geom_density(aes(fill = vs), alpha = 0.1) # а теперь записали в geom

# добавляем различные переменные в качестве аргументов по цвету
ggplot(df, aes(x = am, y = hp, col = vs))+
  #geom_point()
  geom_boxplot()

ggplot(df, aes(x = mpg, y = hp))+
  geom_point(aes(col = am, size = qsec))

# можем сохранять графики как переменные и проводить с ними операции
my_plot <- ggplot(df, aes(x = mpg, y = hp, col = vs, size = qsec))+
  geom_point()

my_plot2 <- ggplot(df, aes(x = am, y = hp, col = vs))

# например, добавлять аргументы типа geom
my_plot2 + geom_boxplot()

# задача 1

df2 <- airquality
df2$Month <- as.factor(df2$Month)

ggplot(df2, aes(x = Month, y = Ozone))+
  geom_boxplot()

# задача 2

df3 <- mtcars
df3$vs <- factor(df3$vs , labels = c("V", "S"))
df3$am <- factor(df3$am , labels = c("Auto", "Manual"))

plot1 <- ggplot(df3, aes(x = mpg, y = disp, col = hp)) + geom_point()

plot1

# задача 3

df4 <- iris

#1 
ggplot(df4, aes(Sepal.Length, fill = Species))+
  geom_histogram()

#2
ggplot(df4, aes(Sepal.Length))+
  geom_histogram(aes(fill = Species))

# задача 4

df5 <- iris

# первоначальный код
ggplot(aes(Sepal.Length, Sepal.Width, col = Species))+
  geom_points(iris, size + Petal.Length)

# исправленный код
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species))+
  geom_point(aes(size = Petal.Length))
