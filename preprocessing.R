source("http://bioconductor.org/biocLite.R")
biocLite("impute")
library(impute)

allform <- read.delim("all_forms_PROACT_training.txt", sep = "|", header = T)
ALSslope <- read.delim("ALSFRS_slope_PROACT_training.txt", sep = "|", header = T)
surv_res <- read.delim("surv_response_PROACT_training.txt", sep = "|", header = T)

rem <- c("Adverse Event", "Concomitant Medication")
subform <- allform[!(allform$form_name %in% rem), ]

features <- levels(factor(subform$feature_name))
allvar = vector("list")
for(i in features){
  temp = subform[subform$feature_name == i,]
  temp = temp[ , -which(names(temp) %in% c("form_name","feature_name"))]
  for(j in 2:length(temp)){
    temp[,j] = factor(temp[,j])
  }
  allvar[[i]] <- temp[temp$feature_value!="-",]
}

subfeat = c("Lymphocytes","Basophils","Monocytes","Total Cholesterol","Gamma-glutamyltransferase","CK","height","Red Blood Cells (RBC)","White Blood Cell (WBC)","Urine Ph","Bicarbonate","if_use_Riluzole","Q10_Respiratory","respiratory_rate","Calcium","Phosphorus","Platelets","Alkaline Phosphatase","bp_diastolic","bp_systolic","pulse","treatment_group","Hematocrit","Hemoglobin","Chloride","fvc","fvc_normal","fvc_percent","Potassium","Q5a_Cutting_without_Gastrostomy","Sodium","AST(SGOT)","Blood Urea Nitrogen (BUN)","Creatinine","ALT(SGPT)","Bilirubin (Total)","onset_delta","Age","ALSFRS_Total","Gender","hands","leg","mouth","onset_site","Q1_Speech","Q2_Salivation","Q3_Swallowing","Q4_Handwriting","Q5_Cutting","Q6_Dressing_and_Hygiene","Q7_Turning_in_Bed","Q8_Walking","Q9_Climbing_Stairs","Race","respiratory","trunk","weight")

patients = c()
for(i in subfeat){
  featTable = allvar[[i]]
  patient_sub = names(table((featTable$SubjectID)))
  patients = union(patients, patient_sub)
}

catfeat = c("Race", "onset_site", "Gender", "treatment_group", "if_use_Riluzole")
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
    tmp = LongitudinalToSingular(data=allvar, featName=i, type="categorical", patients=patients, method=mode)
    out = merge(out, tmp, by="SubjectID", all = TRUE)
  } else{
    tmp = NULL
    if(i == "ALSFRS_Total"){
      tmp = LongitudinalToSingular(data=allvar, featName=i, type="numeric", patients=patients, method=min)
    } else{
      tmp = LongitudinalToSingular(data=allvar, featName=i, type="numeric", patients=patients, method=mean)
    }
    out = merge(out, tmp, by="SubjectID", all = TRUE)
  }
}
write.csv(out, file="sin.csv", row.names=FALSE)

out=read.csv("sin.csv")

### impute singular values: KNN based

v_num = make.names(numfeat)
v_cat = make.names(catfeat)

allslope = data.frame(ALSslope$SubjectID,ALSslope$ALSFRS_slope)
colnames(allslope) = c("SubjectID","ALSFRS_slope")
allslope = merge(allslope, out, by="SubjectID", all.x = FALSE) 

numTab = allslope[v_num]
numMat = as.matrix(numTab)
ki = impute.knn(data=numMat, rng.seed=100)

kidat = ki$data
rightSkewed = c("Gamma.glutamyltransferase", "CK", "Red.Blood.Cells..RBC.", "Urine.Ph", "AST.SGOT.", "ALT.SGPT.", "Bilirubin..Total.")
leftSkewed = c("onset_delta", "hands", "leg", "mouth", "respiratory", "trunk", "ALSFRS_Total")
for(i in 1:length(kidat[1,])){
  currentFeature = kidat[,i]
  f = colnames(kidat)[i]
  if(f %in% rightSkewed){
    l_offs = abs(min(currentFeature)) + 1
    currentFeature = scale(log(currentFeature+l_offs))
  } else if(f %in% leftSkewed){
    currentFeature = scale(currentFeature^3)
  } else{
    currentFeature = scale(currentFeature)
  }
  kidat[,i] = currentFeature
}

catTab = allslope[v_cat]
for(i in colnames(catTab)){
  cur_col = unlist(catTab[i])
  cur_mode = mode(na.omit(cur_col))
  cur_col[is.na(cur_col)] = cur_mode
  catTab[i] = as.factor(cur_col)
}

nnslope = data.frame(allslope$SubjectID, allslope$ALSFRS_slope, kidat, catTab)
colnames(nnslope)[1:2] = c("SubjectID","ALSFRS_slope")


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



### prepare cleaned data files

dum_tab = merge(transformed_tab, surv_res, by="SubjectID", all.x = FALSE)
save(dum_tab, file="dum.rData")

norm_tab = nnslope[c("SubjectID", "ALSFRS_slope", v_num, v_cat)]
norm_tab = merge(norm_tab, surv_res, by="SubjectID", all.x = FALSE)
save(norm_tab, file="norm.rData")