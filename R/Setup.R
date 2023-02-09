#function that drops first two rows of data when reading in xls file

drop_first_two_rows <- function(file_path) {
  data <- read_excel(file_path)
  data <- data[-c(1,2),]
  return(data)
}
