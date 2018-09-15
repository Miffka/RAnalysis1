
a <- -10
  
if (a>0) {
  print('positive')
} else if (a<0) {
  print('negative')
} else {
  print('zero')
}

#ifelse

ifelse(a > 0, 'positive', 'not positive')

#ifelse vector function

a <- c(-1, 1)

ifelse(a > 0, 'positive', 'not positive')

#for

for (i in 1:100) {
  print(i+1)
}

for (i in 1:nrow(my_data)) {
  print(my_data$cls_level[i])
}

head(my_data, 2)

#for+if

for (i in 1:nrow(my_data)) {
  if (my_data$ethnicity[i] == 'minority') {
    print(my_data$score[i])
  }
}

my_data$quality <- rep(NA, nrow(my_data))

for (i in 1:nrow(my_data)) {
  if (my_data$score[i] > 4) {
    my_data$quality[i] = 'good'
  } else my_data$quality[i] = 'bad'
}

my_data$cls_stud_est <- ifelse (my_data$cls_students > mean(my_data$cls_students),
                                'many', 'few')

head(my_data)

#while

j <- 1
while (j < mean(nrow(my_data))) {
  print(my_data$rank[j])
  j <-  j + 1
}

levels(my_data$rank)

#task 1

MT <- data.frame(mtcars)

MT$new_var <- ifelse (MT$carb >= 4 | MT$cyl > 6, 1, 0)

head(MT)


#task 3

? AirPassengers

AP <- as.vector(AirPassengers)

good_months <- rep(NA, length(AP))
i <- 1
for (i in 2:length(AP)) {
  if (AP[i] > AP[i-1]) {
    good_months[i] <- AirPassengers[i]
  }
}

head(good_months)

# they say it is a sample of a task being solved without cycles 
#good_months <- AirPassengers[AirPassengers[-1] > AirPassengers[-144]]
# but it doesn't work right

# task 4

?cumsum
str(AirPassengers)



x <- cumsum(AirPassengers[i:(i+9)])[-c(1:9)]/10

a <- ifelse ()

moving_average <- c()
i <- 1

while (i < (length(AirPassengers)-8)) {
  moving_average[i] <- cumsum(AirPassengers[i:(i+9)])[-c(1:9)]/10
  i <- i + 1
}
moving_average




























