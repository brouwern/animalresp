# Function to clean the deafult column names


## To do:
##  Remove columns that are all NAs
##  add color output eg cat(crayon::blue("Hello", "world!\n"))
##  

clean_resp_dat <- function(raw_data_folder = "./data-raw/",
                           raw_data_file = "23-apr.csv",
                           clean_data_folder = "./data/",
                           clean_file_name = "my_clean_data.csv",
                           object_name = "my_phys"){
  

  raw_dat <- paste0(raw_data_folder,raw_data_file)
  cat("\nData file ",raw_dat," loaded")
  
  dat <- read.csv(raw_dat)
  
  cat("\nThis file contains",dim(dat)[1]," rows")
  cat("\nThis file contains",dim(dat)[2]," columns")
  
  #clean out uneeded characters
  names(dat) <- gsub("[\\.]{1,3}",".",names(dat))
  
  #remove "latest"
  names(dat) <- gsub("Latest","",names(dat))
  
  #change mu to "u"
  names(dat) <- gsub("µ","u",names(dat))
  
  #change accented characters
  names(dat) <- iconv(names(dat),to='ASCII//TRANSLIT')
  
  
  #remove misc "."
  names(dat) <- gsub("^[\\.]","",names(dat))
  names(dat) <- gsub("[\\.]{2}","",names(dat))
  names(dat) <- gsub("[\\.]$","",names(dat))
  
  
  #final name changes
  names(dat)[1] <- "time.min"
  names(dat)[grep("Chan",names(dat))] <- "channel"
  
  
  
  ## Focal columns
  # Latest: Time (min) = time.min
  # Latest: Q-S102 O2 (Pcor) (%) = Q.S102.O2.Pcor
  # Latest: Channel = channel
  
  
  clean_file_name <- paste0(clean_data_folder,clean_file_name)
  
  cat("\nCleaned data saved as ",clean_file_name)
  write.csv(dat, file = clean_file_name)
  
  cat("\nData is saved in R memory as",object_name)
  assign(object_name, value = dat, pos = 1)
  
  rm(list = "dat")
}

