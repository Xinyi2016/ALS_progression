load("dum.rData")
#load("norm.rData")
source("mlr.R")

start <- function(pkg){
  npkg <- pkg[!(pkg %in% installed.packages()[,"Package"])]
  if (length(npkg))
    install.packages(npkg, dependencies = TRUE)
  lapply(pkg, require, character.only=TRUE)
}

pkgs <- c("e1071","doSNOW", "kernlab")
start(pkgs)


kernlab_grid_search <- function(df, G, formula, k, cores=6, maxIter=NA) {
  library(doSNOW, quietly=TRUE)
  
  nIter <- ifelse(is.na(maxIter), nrow(G), maxIter)
  
  cl <- makeCluster(cores)
  registerDoSNOW(cl)
  
  res <- foreach(i = 1:nIter, .packages="kernlab", .combine="rbind") %dopar% {
    g <- G[i,]
    
    res <- c(
      "kernel"= as.character(g$kernel),
      "C"     = g$C,
      "p1"    = g$p1,
      "p2"    = g$p2,
      "error" = NA,
      "cross" = NA,
      "numSV" = NA,
      "obj_f" = NA,
      "time"  = NA
    )
    
    try({
      t <- system.time(
        if(g$kernel == "rbfdot") {
          m <- ksvm(formula, data=df, kernel=as.character(g$kernel), C=g$C, kpar=list(sigma=g$p1), cross=k)
        } else if (g$kernel == "tanhdot") {
          m <- ksvm(formula, data=df, kernel=as.character(g$kernel), C=g$C, kpar=list(scale=g$p1, offset=g$p2), cross=k)
        } else if (g$kernel == "polydot") {
          m <- ksvm(formula, data=df, kernel=as.character(g$kernel), C=g$C, kpar=list(degree=g$p1, offset=g$p2), cross=k)
        }
      )
      
      res <- c(
        "kernel"= as.character(g$kernel),
        "C"     = g$C,
        "p1"    = g$p1,
        "p2"    = g$p2,
        "error" = m@error,
        "cross" = m@cross,
        "numSV" = m@nSV,
        "obj_f" = m@obj,
        "time"  = t["elapsed"]
      )
    })
    
    return(res)
  }
  
  stopCluster(cl)
  
  return(res)
}


# grid --------------------------------------------------------------------

G <- rbind(
  expand.grid(kernel = "rbfdot",  C = 10^(-2:5), p1 = 10^(-5:1), p2 = NA)
  #expand.grid(kernel = "tanhdot", C = 10^(-2:5), p1 = 10^(-5:1), p2 = seq(-2.4,2.4,0.6))
  #expand.grid(kernel = "polydot", C = 10^(-2:2), p1 = c(1,2,3,4,5), p2 = c(0,1))
)

G <- G[sample(nrow(G)),]

formula <- f


fullResult <- NA
step_size <- 40
num_of_iterations <- floor(nrow(G) / step_size) + ifelse(nrow(G)%%step_size==0,0,1)

for(i in 1:num_of_iterations) {
  subG <- G[((i-1)*step_size+1):min((i*step_size), nrow(G)),]
  print(sprintf("G from %d to %d",((i-1)*step_size+1),min((i*step_size), nrow(G))))
  
  subResult <- kernlab_grid_search(training, subG, formula, k=5)
  
  if(!is.na(fullResult)) {
    fullResult <- rbind(fullResult, subResult)
  } else {
    fullResult <- subResult
  }
  
  save.image("svm.grid.search.RData")
}

fullResult_P <- fullResult[order(fullResult[,"cross"]),]
save.image("svm.grid.search.RData")


# final model -------------------------------------------------------------

#m <- ksvm(f, data=training, kernel="tanhdot", C=10, kpar=list(scale=0.1, offset=-1.2))

#res <- data.frame(ALSFRS_slope = test$ALSFRS_slope, pred = as.numeric(as.character(predict(m, test))))
#write.table(res, "result.txt", row.names=FALSE, sep=",")
