
# Load data ---------------------------------------------------------------

allform <- read.delim("all_forms_PROACT_training.txt", sep = "|", header = T)
ALSslope <- read.delim("ALSFRS_slope_PROACT_training.txt", sep = "|", header = T)
surv_res <- read.delim("surv_response_PROACT_training.txt", sep = "|", header = T)



# Subset data according to patients with known ALS slope -----------------------

rem <- c("Adverse Event", "Concomitant Medication")
subform <- allform[!(allform$form_name %in% rem), ]

ids <- as.data.frame(ALSslope$SubjectID)
colnames(ids) <- c("SubjectID")
fform <- merge(ids, subform, by="SubjectID", all.x=FALSE)
fsurv <- merge(ids, surv_res, by="SubjectID", all.x=FALSE)




# One feature one list ----------------------------------------------------

features <- levels(factor(fform$feature_name))
allvar = vector("list")
for(i in features){
  tmp = fform[fform$feature_name == i,]
  tmp = tmp[ , -which(names(tmp) %in% c("form_name","feature_name"))]
  for(j in 2:length(tmp)){
    tmp[,j] = factor(tmp[,j])
  }
  allvar[[i]] <- tmp[tmp$feature_value!="-",]
}




# Subset features with the portion of missing values less than .2 --------

pmissing <- c()
for(n in 1:length(allvar)){
  pmissing[n] <- sum(is.na(allvar[[n]][,-3]))/nrow(allvar[[n]]) 
  pmissing[is.na(pmissing)] <- 1
}

subvar <- allvar[pmissing<0.8]
subfeat <- names(subvar)

# Patient IDs -------------------------------------------------------------

patients= c()
for(i in subfeat){
  featTable = subvar[[i]]
  patient_sub = names(table((featTable$SubjectID)))
  patients = union(patients, patient_sub)
}

# Impute NAs ------------------------------------

catfeat <- c("Gender", "if_use_Riluzole", "onset_site", "Race", "treatment_group")
numfeat = setdiff(subfeat, catfeat)

mode <- function(x){
  v = na.omit(as.vector(unlist(x)))
  return(v[which.max(table(v))])
}

LongitudinalToSingular <- function(data, featName, patients, type, method=mean){
  featTable = data[[featName]]
  feature_singular = c()
  for(patient in patients){
    sub_tab = featTable[featTable$SubjectID==patient,]
    sub_tab = sub_tab[c("SubjectID","feature_value","feature_delta")]
    sub_tab = na.omit(sub_tab)
    
    patientIsMissing = FALSE
    if(is.null(sub_tab)){
      patientIsMissing = TRUE
    } else if(length(sub_tab[,1])<1) {
      patientIsMissing = TRUE
    }
    
    if(patientIsMissing){
      feature_singular=c(feature_singular, NA)
    } else {
      
      xy = data.frame(sub_tab$feature_delta, sub_tab$feature_value)
      colnames(xy)=c("delta","value")
      xy["delta"] = as.numeric(as.matrix(xy$delta))
      
      if(type=="numeric"){
        xy["value"] = as.numeric(as.matrix(xy$value))
        xy = na.omit(xy)
      } else {
        xy["value"] = as.character(as.matrix(xy$value))
        xy = na.omit(xy)
      }
      
      xy = xy[xy$delta<91,]
      if(length(xy[,1])<1){
        feature_singular=c(feature_singular, NA)
      } else{
        pfeat_sing = method(xy$value)
        feature_singular=c(feature_singular, pfeat_sing)
      }
    }
  }
  out = data.frame(patients, feature_singular, stringsAsFactors=FALSE)
  colnames(out)=c("SubjectID", featName)
  return(out)
}

out=data.frame(patients)
colnames(out) = c("SubjectID")
for(i in subfeat){
  if(i %in% catfeat){
    tmp = LongitudinalToSingular(data=subvar, featName=i, type="categorical", patients=patients, method=mode)
    out = merge(out, tmp, by="SubjectID", all = TRUE)
  } else{
    tmp = NULL
    if(i == "ALSFRS_Total"){
      tmp = LongitudinalToSingular(data=subvar, featName=i, type="numeric", patients=patients, method=min)
    } else{
      tmp = LongitudinalToSingular(data=subvar, featName=i, type="numeric", patients=patients, method=mean)
    }
    out = merge(out, tmp, by="SubjectID", all = TRUE)
  }
}



save(out, file="out.rData")
