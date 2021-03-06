df <- mtcars
df$vs <- factor(df$vs , labels = c("V", "S"))
df$am <- factor(df$am , labels = c("Auto", "Manual"))

hist(df$mpg)
hist(df$am)
hist(df$mpg, breaks = 20, xlab = "MPG")

boxplot(mpg ~ am, df, ylab = "MPG")

plot(df$mpg, df$hp)

plot(df$mpg, df$am) #���� �������������� ���������, �� ��� �� �����

# � ������ ����������� �� �������� ggplot2

library(ggplot2)

ggplot(df, aes(x = mpg))+ 
  # geom_histogram(fill = "white", col = "black", bindwigth = 2)
  #geom_dotplot()
  #geom_density(fill = "red", col = "yellow")

# ������������ ����������� �������� � ����������� �� ��������
  # ��������� ������������ ����������
ggplot(df, aes(x = mpg, fill = am))+
  geom_dotplot()

ggplot(df, aes(x = mpg, fill = am))+
  geom_density(alpha = 0.5) # ������ ������ ��������� � ������������� 0,5

# ���� �����, ����� �-� �������������� �������� �� ������ ����������,
# ����� �� � fill
# ��� ����� ������� ggplot ����� �������� � geom, �� ����������� ��������� aes
ggplot(df, aes(x = mpg, fill = vs))+
  geom_density(alpha = 0.1) # �������� � ��������� ggplot

ggplot(df, aes(x = mpg))+
  geom_density(aes(fill = vs), alpha = 0.1) # � ������ �������� � geom

# ��������� ��������� ���������� � �������� ���������� �� �����
ggplot(df, aes(x = am, y = hp, col = vs))+
  #geom_point()
  geom_boxplot()

ggplot(df, aes(x = mpg, y = hp))+
  geom_point(aes(col = am, size = qsec))

# ����� ��������� ������� ��� ���������� � ��������� � ���� ��������
my_plot <- ggplot(df, aes(x = mpg, y = hp, col = vs, size = qsec))+
  geom_point()

my_plot2 <- ggplot(df, aes(x = am, y = hp, col = vs))

# ��������, ��������� ��������� ���� geom
my_plot2 + geom_boxplot()

# ������ 1

df2 <- airquality
df2$Month <- as.factor(df2$Month)

ggplot(df2, aes(x = Month, y = Ozone))+
  geom_boxplot()

# ������ 2

df3 <- mtcars
df3$vs <- factor(df3$vs , labels = c("V", "S"))
df3$am <- factor(df3$am , labels = c("Auto", "Manual"))

plot1 <- ggplot(df3, aes(x = mpg, y = disp, col = hp)) + geom_point()

plot1

# ������ 3

df4 <- iris

#1 
ggplot(df4, aes(Sepal.Length, fill = Species))+
  geom_histogram()

#2
ggplot(df4, aes(Sepal.Length))+
  geom_histogram(aes(fill = Species))

# ������ 4

df5 <- iris

# �������������� ���
ggplot(aes(Sepal.Length, Sepal.Width, col = Species))+
  geom_points(iris, size + Petal.Length)

# ������������ ���
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species))+
  geom_point(aes(size = Petal.Length))
