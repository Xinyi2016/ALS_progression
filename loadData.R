## Load all the .csv files
temp = list.files(pattern="*.csv")
list2env(
  lapply(setNames(temp, make.names(gsub("*.csv$", "", temp))), 
         read.csv), 
  envir = .GlobalEnv
  )