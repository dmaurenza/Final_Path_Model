# Socioeconomic -----
library(tidyverse)
library(MuMIn)

# Reading dataset ----
#Need to check number of colums, in scale function
data <- read_csv("RData/Datafull2.csv")
#data <- read_csv("RData/Datafull2_urban.csv")
# Dataframe for Eco and Socio variable ------------------------------------

Socio <- data %>% 
  select(Site, RR_var, RRmean, BAI1317_05Km: BAI1317_75Km, Cropland_05Km: Cropland_75Km, GrossDef_05Km: NBR1317_75Km, OpCostIISMean_05Km: PercUrbArea09_75Km, RuralPop_05Km:TotalRoadDensity_75Km, cdd_05Km: deltaGPWDensity0010_75Km, fty_05Km: pastoIIS_75Km, sa_05Km: sa_75Km, strictlyPA_05Km: strictlyPA_75Km, wfire_05Km: wfire_75Km, Taxon, -latitude_209564535, -longitude_209564535)
colnames(data)
colnames(Socio)


# Socio <- data %>% 
#   select(Site, RR_var, RRmean, BAI1317_05Km: BAI1317_75Km, Cropland_05Km: Cropland_75Km, GrossDef_05Km: NBR1317_75Km, OpCostIISMean_05Km: PercUrbArea09_75Km, RuralPop_05Km:TotalRoadDensity_75Km, cdd_05Km: deltaGPWDensity0010_75Km, fty_05Km: pastoIIS_75Km, sa_05Km: sa_75Km, strictlyPA_05Km: strictlyPA_75Km, wfire_05Km: wfire_75Km, Taxon, -latitude_209564535, -longitude_209564535, -Latitude.y, -Latitude.y, -Longitude.y)
# colnames(data)
# colnames(Socio)

# Model selection ---------------------------------------------------------
# Socio

list_variable_Socio <- colnames(Socio[4:196])
lm_variable_Socio <- as.data.frame(Socio[4:196])

# Using Variance

Listagem_Var_Socio <- list()
for(i in 1:length(list_variable_Socio)){
  #i = 1
  #message(i)
  Listagem_Var_Socio [[i]] <- lm(RR_var ~ lm_variable_Socio[,i], data = Socio)
}
names(Listagem_Var_Socio) <- list_variable_Socio


# Model.sel Function ----

# AIC, Variance, Socio

names(Listagem_Var_Socio)
BAI1317 <- model.sel(Listagem_Var_Socio[1:6]) # BAI1317_10Km
#Cropland <- model.sel(Listagem_Var_Socio[7:12]) # Cropland_05Km
GrossDef <- model.sel(Listagem_Var_Socio[13:18]) # GrossDef_100Km
# HFPrint <- model.sel(Listagem_Var_Socio[19:24]) # HFPrint_50Km
# IDH03 <- model.sel(Listagem_Var_Socio[25:30]) # IDH03_50Km
# IDH9010 <- model.sel(Listagem_Var_Socio[31:36]) # IDH9010_50Km
# NBR1317 <- model.sel(Listagem_Var_Socio[37:42]) # NBR1317_05Km
# OpCostIISMean <- model.sel(Listagem_Var_Socio[43:48]) # OpCostIISMean_100Km
# OpCostIIS <- model.sel(Listagem_Var_Socio[49:54]) # OpCostIIS_100Km_SUM
PastAgric <- model.sel(Listagem_Var_Socio[55:60]) # PastAgric_05Km
PercUrbArea09 <- model.sel(Listagem_Var_Socio[61:66]) # PercUrbArea09_50Km
RuralPop <- model.sel(Listagem_Var_Socio[67:72]) # RuralPop_50Km
RuralPvty <- model.sel(Listagem_Var_Socio[73:78]) # RuralPvty_05  Km
#SustainablePA <- model.sel(Listagem_Var_Socio[79:84]) # SustainablePA_100Km
#T1RoadDensity <- model.sel(Listagem_Var_Socio[85:90]) # T1RoadDensity_50Km
#T2RoadDensity <- model.sel(Listagem_Var_Socio[91:96]) # T2RoadDensity_05Km
#T3RoadDensity <- model.sel(Listagem_Var_Socio[97:102]) # T3RoadDensity_05Km
#T4RoadDensity <- model.sel(Listagem_Var_Socio[103:108]) # T4RoadDensity_100Km
#T5RoadDensity <- model.sel(Listagem_Var_Socio[109:114]) # T5RoadDensity_05Km
#TotalRoadDensity <- model.sel(Listagem_Var_Socio[115:120]) # TotalRoadDensity_05Km
cdd <- model.sel(Listagem_Var_Socio[121:126]) # cdd_100Km
#cropIIS <- model.sel(Listagem_Var_Socio[127:132]) # cropIIS_100Km
#croppastureIIS <- model.sel(Listagem_Var_Socio[133:138]) # croppastureIIS_100Km
#deltaGHSL9015 <- model.sel(Listagem_Var_Socio[139:144]) # deltaGHSL9015_25Km
#deltaGPWCount0010 <- model.sel(Listagem_Var_Socio[145:150]) # deltaGPWCount0010_10Km
#deltaGPWDensity0010 <- model.sel(Listagem_Var_Socio[151:156]) # deltaGPWDensity0010_10Km
fty <- model.sel(Listagem_Var_Socio[157:162]) # fty_25Km - Selecionamos 25 por causa da variação dos dados. O mesmo com sa
govrnce <- model.sel(Listagem_Var_Socio[163:168]) # govrnce_05Km
#pastoIIS <- model.sel(Listagem_Var_Socio[169:174]) # pastoIIS_10Km
sa <- model.sel(Listagem_Var_Socio[c(176,178:180)]) # sa_25Km - 10 e 05 estavavam com problema mas foram os selecionados
strictlyPA <- model.sel(Listagem_Var_Socio[181:186]) # strictlyPA_75Km
wfire <- model.sel(Listagem_Var_Socio[187:192]) # wfire_75Km 

# Combining variables  -------------
# Var

OPPCost <- model.sel(c(Listagem_Var_Socio[43:48], Listagem_Var_Socio[49:54])) # OpCostIIS_100Km_SUM
RoadDensity <- model.sel(c(Listagem_Var_Socio[85:90], Listagem_Var_Socio[91:96], Listagem_Var_Socio[97:102], Listagem_Var_Socio[103:108], Listagem_Var_Socio[109:114], Listagem_Var_Socio[115:120])) # T1RoadDensity_50Km

# DD <- model.sel(c(Listagem_Var_Socio[121:126], Listagem_Var_Socio[157:162], Listagem_Var_Socio[175:180], Listagem_Var_Socio[187:192])) # fty_05Km

IDH <- model.sel(c(Listagem_Var_Socio[25:30], Listagem_Var_Socio[31:36])) # IDH03_25Km
# PA <- model.sel(c(Listagem_Var_Socio[79:84], Listagem_Var_Socio[181:186])) #SustainablePA_05Km 
FIRE <- model.sel(c(Listagem_Var_Socio[1:6], Listagem_Var_Socio[37:42], Listagem_Var_Socio[187:192])) # BAI1317_10Km
POP <- model.sel(c(Listagem_Var_Socio[139:144], Listagem_Var_Socio[145:150], Listagem_Var_Socio[151:156])) # deltaGPWDensity0010_25Km
#IIS <- model.sel(c(Listagem_Var_Socio[127:132], Listagem_Var_Socio[133:138], Listagem_Var_Socio[169:174]))

Crop <- model.sel(c(Listagem_Var_Socio[7:12], Listagem_Var_Socio[127:132])) # cropIIS_05Km
Pasture <- model.sel(c(Listagem_Var_Socio[55:60], Listagem_Var_Socio[169:174])) # PastoIIS_15Km

# Final Table -----
Socio_Var <- data %>% 
  select(Site, RR_var, BAI1317_10Km, cropIIS_05Km, GrossDef_100Km,  IDH03_25Km, OpCostIIS_100Km_SUM, pastoIIS_10Km, PercUrbArea09_50Km,  RuralPop_50Km, RuralPvty_05Km, T1RoadDensity_50Km, cdd_100Km, deltaGPWDensity0010_25Km, fty_25Km, govrnce_05Km, sa_50Km, strictlyPA_100Km)
write_csv(Socio_Var, "RData/Socio_Var2.csv")
write_csv(Socio_Var, "RData/Socio_Var2_urban.csv")
#### TERMINA AQUI #####
#Hist----- chamamos datafull_base2 do pré-processing antes de scalonar e tiramos os outlyers

Hist_Socio <- datafull_base2

hist(Hist_Socio$BAI1317_10Km, 100)
plot(Socio_Var$RR_var ~ Socio_Var$BAI1317_10Km)
x<-lm(Socio_Var$RR_var ~ Socio_Var$BAI1317_10Km)
abline(x)

hist(Hist_Socio$cropIIS_05Km, 100)
mean(Hist_Socio$cropIIS_05Km)#19%
plot(Socio_Var$RR_var ~ Socio_Var$cropIIS_05Km)
x<-lm(Socio_Var$RR_var ~ Socio_Var$cropIIS_05Km)
abline(x)

hist(Hist_Socio$GrossDef_100Km, 100)
mean(Hist_Socio$GrossDef_100Km)#0.0276
plot(Socio_Var$RR_var ~ Socio_Var$GrossDef_100Km)
x<-lm(Socio_Var$RR_var ~ Socio_Var$GrossDef_100Km)
abline(x)

hist(Hist_Socio$IDH03_25Km, 100)
mean(Hist_Socio$IDH03_25Km)#0.63
plot(Socio_Var$RR_var ~ Socio_Var$IDH03_25Km)
x<-lm(Socio_Var$RR_var ~ Socio_Var$IDH03_25Km)
abline(x)

hist(Hist_Socio$OpCostIIS_100Km_SUM, 100)
mean(Hist_Socio$OpCostIIS_100Km_SUM)#100074450
plot(Socio_Var$RR_var ~ Socio_Var$OpCostIIS_100Km_SUM)
x<-lm(Socio_Var$RR_var ~ Socio_Var$OpCostIIS_100Km_SUM)
abline(x)

hist(Hist_Socio$pastoIIS_10Km, 100)
mean(Hist_Socio$pastoIIS_10Km)#0.017
plot(Socio_Var$RR_var ~ Socio_Var$pastoIIS_10Km)
x<-lm(Socio_Var$RR_var ~ Socio_Var$pastoIIS_10Km)
abline(x)

hist(Hist_Socio$PercUrbArea09_50Km, 100)
mean(Hist_Socio$PercUrbArea09_50Km)#
plot(Socio_Var$RR_var ~ Socio_Var$PercUrbArea09_50Km)
x<-lm(Socio_Var$RR_var ~ Socio_Var$PercUrbArea09_50Km)
abline(x)

hist(Hist_Socio$RuralPop_50Km, 100)
mean(Hist_Socio$RuralPop_50Km)#444
plot(Socio_Var$RR_var ~ Socio_Var$RuralPop_50Km)
x<-lm(Socio_Var$RR_var ~ Socio_Var$RuralPop_50Km)
abline(x)

hist(Hist_Socio$RuralPvty_05Km, 100)
mean(Hist_Socio$RuralPvty_05Km)#10.49
plot(Socio_Var$RR_var ~ Socio_Var$RuralPvty_05Km)
x<-lm(Socio_Var$RR_var ~ Socio_Var$RuralPvty_05Km)
abline(x)

hist(Hist_Socio$T1RoadDensity_50Km, 100)
mean(Hist_Socio$T1RoadDensity_50Km)#6.2
plot(Socio_Var$RR_var ~ Socio_Var$T1RoadDensity_50Km)
x<-lm(Socio_Var$RR_var ~ Socio_Var$T1RoadDensity_50Km)
abline(x)

hist(Hist_Socio$cdd_100Km, 100)
plot(Socio_Var$RR_var ~ Socio_Var$cdd_100Km)
x<-lm(Socio_Var$RR_var ~ Socio_Var$cdd_100Km)
abline(x)

hist(Hist_Socio$deltaGPWDensity0010_25Km, 100)
plot(Socio_Var$RR_var ~ Socio_Var$deltaGPWDensity0010_25Km)
x<-lm(Socio_Var$RR_var ~ Socio_Var$deltaGPWDensity0010_25Km)
abline(x)

hist(Hist_Socio$fty_25Km, 100)
plot(Socio_Var$RR_var ~ Socio_Var$fty_25Km)
x<-lm(Socio_Var$RR_var ~ Socio_Var$fty_25Km)
abline(x)

hist(Hist_Socio$sa_50Km, 100)
mean(Hist_Socio$sa_50Km)#0.59
plot(Socio_Var$RR_var ~ Socio_Var$sa_50Km)
x<-lm(Socio_Var$RR_var ~ Socio_Var$sa_50Km)
abline(x)

hist(Hist_Socio$strictlyPA_100Km, 100)
plot(Socio_Var$RR_var ~ Socio_Var$strictlyPA_100Km)
x<-lm(Socio_Var$RR_var ~ Socio_Var$strictlyPA_100Km)
abline(x)
