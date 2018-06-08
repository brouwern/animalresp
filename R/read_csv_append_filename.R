### Create function that reads .csv files of raw data in a 
##   directory and appends them with source filename
read_csv_append_filename <- function(filename){
  ret <- read.csv(filename)
  ret$SourceFile <- filename
  ret
}