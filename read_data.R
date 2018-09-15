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