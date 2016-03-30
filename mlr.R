install.packages("MASS")
library(MASS)

ind <- sample(2, nrow(dum_tab), replace=TRUE, prob=c(0.8,0.2))
training <- dum_tab[ind ==1, ]
test <- dum_tab[ind == 2, ]
fit <- lm(ALSFRS_slope~., training)
step <- stepAIC(fit, direction="both")
step$anova

f <- ALSFRS_slope ~ Gamma.glutamyltransferase + height + White.Blood.Cell..WBC. + 
  Bicarbonate + Phosphorus + Platelets + bp_diastolic + pulse + 
  fvc_normal + fvc_percent + AST.SGOT. + Creatinine + ALT.SGPT. + 
  Bilirubin..Total. + onset_delta + ALSFRS_Total + hands + 
  mouth + Q2_Salivation + Q3_Swallowing + Q4_Handwriting + 
  Q5_Cutting + Q6_Dressing_and_Hygiene + Q8_Walking + Q9_Climbing_Stairs + 
  respiratory + trunk + Race.Hispanic + onset_site.Limb + onset_site.Limb.and.Bulbar + 
  Gender.M + if_use_Riluzole.Yes + time_event + status

