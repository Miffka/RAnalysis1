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