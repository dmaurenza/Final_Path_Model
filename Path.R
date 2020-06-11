# Path Model
library(piecewiseSEM)
 
#control = lmeControl(opt='optim', optimMethod = "BFGS", maxIter = 1000, msMaxIter = 1000, niterEM = 1000)
 
unif<- psem(
  direct <- lm(RR_var ~ Time_mean + ForestUntl10_25Km + bldfie_75Km + slope_50Km + cropIIS_05Km + OpCostIIS_100Km_SUM + PercUrbArea09_50Km + RuralPop_50Km + RuralPvty_05Km + deltaGPWDensity0010_25Km +  fty_25Km + strictlyPA_100Km , data = Global_Var),
#summary(direct)#
  
  FOREST <- lm(ForestUntl10_25Km ~ Time_mean + WDeficit99_05Km + BAI1317_10Km + GrossDef_100Km + OpCostIIS_100Km_SUM + RuralPvty_05Km + T1RoadDensity_50Km + strictlyPA_100Km + AnualTemp_05Km +  slope_50Km, data = Global_Var),
  
 CROP <- lm(cropIIS_05Km ~ BAI1317_10Km + GrossDef_100Km + RuralPop_50Km +  deltaGPWDensity0010_25Km, data = Global_Var),
 # surege entrar com -  #

  URBAN <- lm(PercUrbArea09_50Km ~ T1RoadDensity_50Km + deltaGPWDensity0010_25Km,  data = Global_Var))


summary(unif)
x <- coefs(unif)
write_excel_csv(x, "RData/coefs.csv")
#plot(Global_Var$ForestUntl10_25Km ~ Global_Var$strictlyPA_100Km)
#plot(Global_Var$RR_var ~ Global_Var$strictlyPA_100Km)

rsquared(unif)
coefs <- coefs(unif)
write_excel_csv(coefs, "RData/coefs.csv")

#### Results----
# Direct effect
direct <- 0.2711+(-0.4893)+(-0.3724)+(-0.2168)+(-0.5113)+ (-0.3723)+0.4548+0.3131+(-0.2458)+(-0.3386)+0.3606+(-0.2726)
# indirect effect

# Cropland
