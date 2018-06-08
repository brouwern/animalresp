# clean all files in a directory files

clean_and_resave_multi_dat <- function(directory, ...){
  cat(length(directory),"files being cleaned and resaved to specified clean data directory.")
  for(i in 1:length(directory)){
    clean_and_resave_resp_dat(directory[i])
  }
}
