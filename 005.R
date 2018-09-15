age <- c(16, 18, 22, 27)
is_married <- c(F, F, T, T)
name <- c("Olga", "Maria", "Nastya", "Polina")

data <- list(age, is_married, name)

data[[2]][1]
data[2]

df <- data.frame(Name = name, Age = age, Status = is_married)
typeof(df)
