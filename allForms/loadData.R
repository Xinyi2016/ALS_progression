lib <- function(pkg){
  npkg <- pkg[!(pkg %in% installed.packages()[,"Package"])]
  if (length(npkg))
    install.packages(npkg, dependencies = TRUE)
  lapply(pkg, require, character.only=TRUE)
}

pkgs <- c("RODBC")
lib(pkgs)


# Load all the .csv files -------------------------------------------------

temp = list.files(pattern="*.csv")
list2env(
  lapply(setNames(temp, make.names(gsub("*.csv$", "", temp))), 
         read.csv), 
  envir = .GlobalEnv
  )

targets <- levels(as.factor(alsfrs$subject_id))


multmerge = function(mypath){
  filenames=list.files(path=mypath, full.names=TRUE)
  datalist = lapply(filenames, function(x){read.csv(file=x)})
  Reduce(function(x,y) {merge(x,y, all=TRUE)}, datalist)}

multmerge("~/23May/ALS_progression")
