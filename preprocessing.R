source("http://bioconductor.org/biocLite.R")
biocLite("impute")
library(impute)
start <- function(pkg){
  npkg <- pkg[!(pkg %in% installed.packages()[,"Package"])]
  if (length(npkg))
    install.packages(npkg, dependencies = TRUE)
  lapply(pkg, require, character.only=TRUE)
}

pkgs <- c("lattice", "ggplot2")
start(pkgs)

load("out.rData")

v_num = make.names(numfeat)
v_cat = make.names(catfeat)

allslope = data.frame(ALSslope$SubjectID,ALSslope$ALSFRS_slope)
colnames(allslope) = c("SubjectID","ALSFRS_slope")
allslope = merge(allslope, out, by="SubjectID", all.x = FALSE) 

numTab = allslope[v_num]
numMat = as.matrix(numTab)
ki = impute.knn(data=numMat, rng.seed=100)

catTab = allslope[v_cat]
for(i in colnames(catTab)){
  cur_col = unlist(catTab[i])
  cur_mode = mode(na.omit(cur_col))
  cur_col[is.na(cur_col)] = cur_mode
  catTab[i] = as.factor(cur_col)
}

# Combine -----------------------------------------------------------------

nnslope = data.frame(allslope$SubjectID, allslope$ALSFRS_slope, numTab, catTab)
colnames(nnslope)[1:2] = c("SubjectID","ALSFRS_slope")

# Represent distributions -------------------------------------------------

kidat = as.data.frame(ki$data)
hist(kidat[,1])
plot(density(kidat[,2]))
boxplot(kidat[,3])
bwplot(kidat[,4])
ggplot(kidat, aes(kidat[,5])) +
  geom_boxplot()



# Transform skewed features -----------------------------------------------

posSkewed = c("Gamma.glutamyltransferase", "CK", "Red.Blood.Cells..RBC.", "Urine.Ph", "AST.SGOT.", "ALT.SGPT.", "Bilirubin..Total.")
negSkewed = c("onset_delta", "hands", "leg", "mouth", "respiratory", "trunk", "ALSFRS_Total")
for(i in 1:length(kidat[1,])){
  currentFeature = kidat[,i]
  f = colnames(kidat)[i]
  if(f %in% posSkewed){
    currentFeature = scale(sqrt(currentFeature))
    # currentFeature = scale(log10(currentFeature))
    # currentFeature = scale(1/currentFeature)
  } else if(f %in% negSkewed){
    currentFeature = scale(currentFeature^2)
    # currentFeature = scale(currentFeature^3)
    # currentFeature = scale(currentFeature^4)
  } else{
    currentFeature = scale(currentFeature)
  }
  kidat[,i] = currentFeature
}

### dummy coding

appendPrefix=function(i_names, prefix){
  if(length(i_names)==1){
    return( paste0(prefix, i_names) )
  }
  newNames=c()
  for(i in 1:length(i_names)){
    featureName=paste0(prefix, i_names[i])
    newNames=c(newNames, featureName)
  }
  return(newNames)
}

transformed_tab = nnslope[c("SubjectID", "ALSFRS_slope", v_num)]
v_dmy = c()
for(i in v_cat){
  col_vals = unlist(nnslope[i])
  modelString = paste("~", i)
  t_mat = model.matrix( eval(parse(text=modelString)), data =nnslope)
  t_mat = t_mat[,2:length(t_mat[1,])]
  lvls = levels(col_vals)
  lvls = lvls[2:length(lvls)]
  t_dat = data.frame(t_mat)
  colnames(t_dat) = appendPrefix(lvls, prefix=paste0(i,"."))
  v_dmy = c(v_dmy, colnames(t_dat))
  transformed_tab = data.frame(transformed_tab, t_dat)
}




### Save

dum_tab = merge(transformed_tab, surv_res, by="SubjectID", all.x = FALSE)
save(dum_tab, file="dum.rData")

norm_tab = nnslope[c("SubjectID", "ALSFRS_slope", v_num, v_cat)]
norm_tab = merge(norm_tab, surv_res, by="SubjectID", all.x = FALSE)
save(norm_tab, file="norm.rData")