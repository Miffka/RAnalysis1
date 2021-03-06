# i say, we wrote the script and want to save it
setwd("~/R") # � ����� ������ ������ ����������

library(ggplot2)#����� ���������� ���������� ������
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

# ��������� ������� ����� ����� save, ��� �����
# ���������� ������ ���� �� ��������� � ������� ����������

getwd() # ������� ���� � ������� ����������

setwd("~/R/RCourse") # ����� ��� ������� ����� �������� � ������ �������, ����� ��� ����������� � ������ �����, �� ������ ��� �� ��������
#help(setwd)

# ���������� ������� ����� ������ ����� export - save as image

# ��������� ������ ����������� � ������� csv
write.csv(df, "mtcars.csv")

# ��������� � ������� csv ���������� �������
write.csv(descr_df, "descr_df.csv")

#���������� ���������� 
my_mean  <- mean(10^6 : 10^7)
save(my_mean, file = "my_mean.RData") # �������� �����, ���� � ��� ��� � �����������
#����� ��������� ��� ��������� ����� ����
#