allform <- read.delim("all_forms_PROACT_training.txt", sep = "|", header = T)
ALSslope <- read.delim("PROACT_ progression/ALSFRS_slope_PROACT_training.txt", sep = "|", header = T)
surv_res <- read.delim("PROACT_survival/surv_response_PROACT_training2.txt", sep = "|", header = T)

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
