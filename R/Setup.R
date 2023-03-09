#function that drops first two rows of data when reading in xls file

drop_first_two_rows <- function(file_path) {
  data <-  read.csv(file_path)
  data <- data[-c(1,2),]
  return(data)
}

#steps for each data point
#1)variable labels
#2)value re-code (-2 to +2)
#3)overall Tukey test - compare scales at an overall level
#4)by market Tukey test - compare each scale between markets
#5)within each market
