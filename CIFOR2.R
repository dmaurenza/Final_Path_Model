###### CIFOR2########
# Loading Packages
install.packages("tidyverse", dependencies = T)
install.packages("nlme", dependencies = T)
install.packages("piecewiseSEM", dependencies = T)
install.packages("MuMIn", dependencies = T)
install.packages("corrplot", dependencies = T)
install.packages("car", dependencies = T)
install.packages("MASS")
library(tidyverse)
library(nlme)
library(MuMIn)
library(piecewiseSEM)
library(corrplot)
library(car)
library(MASS)
#


# Reading datasets --------------------------------------------------------
datafull <- read_csv("database.full.csv")
buffer <- read_csv("Biodiversity_Revision.csv") %>% 
  mutate(Site = as.character(Site))

## Biodiversity data
datafull <- datafull %>% 
  filter(Mammals == 1| Birds == 1| Herpetofauna == 1| Plants == 1| Invertebrates == 1,
         Latitude < 35 & Latitude >-35 & Restoration_activity == "passive") %>% 
  mutate(Study = as.character(Study),
         Site = as.character(Site)) ## Tropical & subtropical landscapes restored through passive regeneration
colnames(buffer)
## Data used from Crouzeilles et al. 2016 database 
datafull %>% 
  group_by(Study)
datafull %>% 
  group_by(Site)
colnames(datafull)
datafull <- datafull %>% 
  select(Study, Site, Past_disturbance, Time_restored, Plants:Mammals, RR)

## Merge biodiversity & predictor variables in a single data frame
datafull <- datafull %>% 
  inner_join(buffer, by = "Site") %>% 
  select(-`system:index`, -RR_var, -latitude_209564535, -longitude_209564535, -.geo)
colnames(datafull)  

# Selecting landscape with > 2 RR values
datafull <- datafull %>% 
  group_by(Site) %>% 
  filter(n()>2) %>% 
  ungroup() %>% 
  filter(!is.na(Time_restored)) # Eliminating NA values in Time Restored

# Calculating RR var and RR mean
data <- datafull
## Script for RRvar
data$N<-1
#data2<-na.omit(data)
N<-as.data.frame(tapply(data$N, data$Site, sum))
colnames(N)<-"N"
data$RRcor<-data$RR^2
RR<-as.data.frame(tapply(data$RRcor, data$Site, sum))
RR_var<-RR/(N-1)
colnames(RR_var)<-"RR_var"
RR_var$Site<-rownames(RR_var)
data3<-merge(data, RR_var, by="Site")
colnames(data3)

datafull <- as.tibble(data3)
colnames(datafull)

# Calculating RR metrics
datafull <- datafull %>% 
  group_by(Site) %>%
  mutate(RRmean = mean(RR),
    RRmean = abs(RRmean),
    Time_mean = mean(Time_restored)) %>% 
  ungroup()

datafull %>% 
  select(1:2, RR, RRmean, RR_var, Time_restored, Time_mean)


# Inclusão da nova catecogia taxon_group
# Verificar o que fazer com Sites que possuem varios grupos taxonômicos.


# Defining taxon group
datafull <- datafull %>% 
  mutate(Taxon_group = as.character(NA),
    Taxon_group = if_else(Plants == 1, true = "Plants", false = Taxon_group, missing = "NA"), 
    Taxon_group = if_else(Invertebrates == 1, true = "Invertebrates", false = Taxon_group, missing = "NA"),
    Taxon_group = if_else(Birds == 1, true = "Vertebrates", false = Taxon_group, missing = "NA"),
    Taxon_group = if_else(Herpetofauna == 1, true = "Vertebrates", false = Taxon_group, missing = "NA"),
    Taxon_group = if_else(Mammals == 1, true = "Vertebrates", false = Taxon_group, missing = "NA"))
    

# Descarting studies. Once same sites were sampled in more than 1 study, we decide to eliminate those studies with lowest n = Response Ratio. 

datafull <- datafull %>% 
  filter(!Study == "69" & !Study == "190" & !Study == "226" )

datafull %>% 
  group_by(Study)
datafull %>% 
  group_by(Site)
colnames(datafull)
# Scale -------------------------------------------------------------------


# Scale function. Eliminating some colluns to apply scale function. Data became NaN because excess of "0" values.

datafull <- datafull %>% 
  select( -urb_05Km, -urb_100Km, -urb_10Km, -urb_25Km, -urb_50Km, -urb_75Km)

datafull[,10:321] <- scale(datafull[,10:321])


# colnames(datafull)
# datafull %>% 
#    group_by(Study)
# datafull %>% 
#    group_by(Site)
# datafull %>% 
#   select(RR)
# x %>% 
#   select(RR)
# print(datafull$RR)
# 
# 
# # Variable with NA values
# 
# variables <- colnames(datafull)
# list_NA <- list()
# for(i in 1:length(variables)){
#   #i = 1
#   variables_NA <- is.na(datafull[,i])
#   x <- unique(variables_NA[,1])
#   list_NA[[i]] <- x
# }


# Final table -------------------------------------------------------------
data <- datafull %>% 
  distinct(Site, .keep_all = T) %>% 
  select(Site, Past_disturbance, Plants:Mammals, Taxon_group, RR_var, RRmean, Time_mean, AnualTemp_05Km:wfire_75Km)



# # media em absoluto
# # não utiliza grupo taxonomico
# # excluir estudo na mesma paisagem com menor numero de RR 1
# # GLM por paisagem
# # Predito grupo taxonomico em 3 categoria (vertebrado, invertebrado e plantas) 2 
# # inflation factor 
# # regressão multipla usando o stepwise
# # oredem> buffer (agrupamentos) 3
# # mostrar todas as variáveris no grupo
# # inflaction ou correlação (Variance inflation factor) 4
# # stepwise
# 
# colnames(data)
# # Dataframe for Eco and Socio variable ------------------------------------
# Eco <- data %>%
#   select(Site, RR_var, RRmean, Time_mean, AnualTemp_05Km : AnualTemp_75Km, CEC_05Km: CEC_75Km, DriestQuartes_05Km: ForestUntl10_75Km, NPP0010_05Km : NPP03_75Km, PrecSeanslty_05Km : PrecSeanslty_75Km, WDeficit03_05Km: bldfie_75Km, elevation_05Km: f2003_75Km, phihox_05Km: phikcl_75Km, slope_05Km: sndppt_75Km, treecover2000_05Km: treecover2000_75Km)
# colnames(Eco)
# 
# Socio <- data %>% 
#   select(Site, RR_var, RRmean, BAI1317_05Km: BAI1317_75Km, Cropland_05Km: Cropland_75Km, GrossDef_05Km: NBR1317_75Km, OpCostIISMean_05Km: PercUrbArea09_75Km, RuralPop_05Km:TotalRoadDensity_75Km, cdd_05Km: deltaGPWDensity0010_75Km, fty_05Km: pastoIIS_75Km, sa_05Km: sa_75Km, strictlyPA_05Km: strictlyPA_75Km, wfire_05Km: wfire_75Km)      
#     
#     
# colnames(Socio)
# 
# 
# # Model selection ---------------------------------------------------------
# # Eco
# 
# list_variable_Eco <- colnames(Eco[5:118])
# lm_variable_Eco <- as.data.frame(Eco)
# 
# # Using Variance
# 
# Listagem_Var_Eco <- list()
# for(i in 1:length(list_variable_Eco)){
#   #i = 1
#   Listagem_Var_Eco [[i]] <- lm(RR_var ~ lm_variable_Eco[,i], data = Eco)
# }
# names(Listagem_Var_Eco) <- list_variable_Eco
# 
# # Using mean
# 
# Listagem_Mean_Eco <- list()
# for(i in 1:length(list_variable_Eco)){
#   #i = 1
#   Listagem_Mean_Eco[[i]] <- lm(RRmean ~ lm_variable_Eco[,i], data = Eco)
# }
# names(Listagem_Mean_Eco) <- list_variable_Eco
# 
# # Socio
# 
# list_variable_Socio <- colnames(Socio[4:195])
# lm_variable_Socio <- as.data.frame(Socio)
# 
# # Using Variance
# 
# Listagem_Var_Socio <- list()
# for(i in 1:length(list_variable_Socio)){
#   #i = 1
#   Listagem_Var_Socio [[i]] <- lm(RR_var ~ lm_variable_Socio[,i], data = Socio)
# }
# names(Listagem_Var_Socio) <- list_variable_Socio
# 
# # Using mean
# 
# Listagem_Mean_Socio <- list()
# for(i in 1:length(list_variable_Socio)){
#   #i = 1
#   Listagem_Mean_Socio[[i]] <- lm(RRmean ~ lm_variable_Socio[,i], data = Socio)
# }
# names(Listagem_Mean_Socio) <- list_variable_Socio
# 
# # AIC, Variance, Eco
# 
# 
# x <- Listagem_Var_Eco$WDeficit8910_10Km
# plot(x)
# 
# 
# names(Listagem_Var_Eco)
# AnualTemp <- model.sel(Listagem_Var_Eco[3:6]) # AnualTemp_100Km # doen't run including buffer 05km and 100km
# CEC <- model.sel(Listagem_Var_Eco[7:12]) # CEC_25Km
# DriestQuartes <- model.sel(Listagem_Var_Eco[13:18]) # DriestQuartes_50Km
# ForestUntl10 <- model.sel(Listagem_Var_Eco[19:24]) # ForestUntl10_75Km
# NPP0010 <- model.sel(Listagem_Var_Eco[25:30]) # NPP0010_25Km
# NPP03 <- model.sel(Listagem_Var_Eco[31:36]) # NPP03_10Km
# PrecSeanslty <- model.sel(Listagem_Var_Eco[37:42]) # PrecSeanslty_50Km
# WDeficit03 <- model.sel(Listagem_Var_Eco[43:48]) #WDeficit03_50Km
# WDeficit8910 <- model.sel(Listagem_Var_Eco[49:54]) # WDeficit8910_50Km
# WDeficit99 <- model.sel(Listagem_Var_Eco[55:60]) # WDeficit99_05Km
# YrlPrec <- model.sel(Listagem_Var_Eco[61:66]) # YrlPrec_05Km
# bldfie <- model.sel(Listagem_Var_Eco[67:72]) # bldfie_75Km
# elevation <- model.sel(Listagem_Var_Eco[73:78]) # elevation_75Km
# f2003 <- model.sel(Listagem_Var_Eco[79:84]) # f2003_25Km
# phihox <- model.sel(Listagem_Var_Eco[85:90]) # phihox_25Km
# phikcl <- model.sel(Listagem_Var_Eco[91:96]) # phikcl_100Km
# slope <- model.sel(Listagem_Var_Eco[97:102]) # slope_75Km
# sndppt <- model.sel(Listagem_Var_Eco[103:108]) # sndppt_50Km
# treecover2000 <- model.sel(Listagem_Var_Eco[109:114]) # treecover2000_05Km 
# 
# # AIC, Variance, Socio
# Listagem_Var_Socio$BAI1317_10Km
# names(Listagem_Var_Socio)
# BAI1317 <- model.sel(Listagem_Var_Socio[3:6]) # BAI1317_10Km # doen't run including buffer 05km and 100km
# Cropland <- model.sel(Listagem_Var_Socio[7:12]) # Cropland_05Km
# GrossDef <- model.sel(Listagem_Var_Socio[13:18]) # GrossDef_10Km 
# HFPrint <- model.sel(Listagem_Var_Socio[19:24]) # HFPrint_50Km
# IDH03 <- model.sel(Listagem_Var_Socio[25:30]) # IDH03_05Km 
# IDH9010 <- model.sel(Listagem_Var_Socio[31:36]) # IDH9010_05Km
# NBR1317 <- model.sel(Listagem_Var_Socio[37:42]) # NBR1317_05Km
# OpCostIISMean <- model.sel(Listagem_Var_Socio[43:48]) # OpCostIISMean_75Km
# OpCostIIS <- model.sel(Listagem_Var_Socio[49:54]) # OpCostIIS_75Km_SUM
# PastAgric <- model.sel(Listagem_Var_Socio[55:60]) # PastAgric_50Km
# PercUrbArea09 <- model.sel(Listagem_Var_Socio[61:66]) # PercUrbArea09_100Km
# RuralPop <- model.sel(Listagem_Var_Socio[67:72]) # RuralPop_100Km
# RuralPvty <- model.sel(Listagem_Var_Socio[73:78]) # RuralPvty_100Km
# SustainablePA <- model.sel(Listagem_Var_Socio[79:84]) # SustainablePA_25Km
# T1RoadDensity <- model.sel(Listagem_Var_Socio[85:90]) # T1RoadDensity_50Km
# T2RoadDensity <- model.sel(Listagem_Var_Socio[91:96]) # T2RoadDensity_100Km
# T3RoadDensity <- model.sel(Listagem_Var_Socio[97:102]) # T3RoadDensity_10Km
# T4RoadDensity <- model.sel(Listagem_Var_Socio[103:108]) # T4RoadDensity_10Km
# T5RoadDensity <- model.sel(Listagem_Var_Socio[109:114]) # T5RoadDensity_75Km
# TotalRoadDensity <- model.sel(Listagem_Var_Socio[115:120]) # TotalRoadDensity_05Km
# cdd <- model.sel(Listagem_Var_Socio[121:126]) # cdd_05Km
# cropIIS <- model.sel(Listagem_Var_Socio[127:132]) # cropIIS_25Km
# croppastureIIS <- model.sel(Listagem_Var_Socio[133:138]) # croppastureIIS_05Km
# deltaGHSL9015 <- model.sel(Listagem_Var_Socio[139:144]) # deltaGHSL9015_50Km
# deltaGPWCount0010 <- model.sel(Listagem_Var_Socio[145:150]) # deltaGPWCount0010_50Km
# deltaGPWDensity0010 <- model.sel(Listagem_Var_Socio[151:156]) # deltaGPWDensity0010_50Km
# fty <- model.sel(Listagem_Var_Socio[157:162]) # fty_100Km
# govrnce <- model.sel(Listagem_Var_Socio[163:168]) # govrnce_25Km
# pastoIIS <- model.sel(Listagem_Var_Socio[169:174]) # pastoIIS_05Km
# sa <- model.sel(Listagem_Var_Socio[175:180]) # sa_50Km
# strictlyPA <- model.sel(Listagem_Var_Socio[181:186]) # strictlyPA_10Km
# wfire <- model.sel(Listagem_Var_Socio[187:192]) # wfire_25Km
# 
# 
# # AIC, Mean, Eco
# names(Listagem_Mean_Eco)
# AnualTemp <- model.sel(Listagem_Mean_Eco[1:6]) # AnualTemp_100Km
# CEC <- model.sel(Listagem_Mean_Eco[7:12]) # CEC_100Km
# DriestQuartes <- model.sel(Listagem_Mean_Eco[13:18]) # DriestQuartes_25Km
# ForestUntl10 <- model.sel(Listagem_Mean_Eco[19:24]) # ForestUntl10_100Km
# NPP0010 <- model.sel(Listagem_Mean_Eco[25:30]) # NPP0010_50Km
# NPP03 <- model.sel(Listagem_Mean_Eco[31:36]) # NPP03_25Km
# PrecSeanslty <- model.sel(Listagem_Mean_Eco[37:42]) # PrecSeanslty_10Km
# WDeficit03 <- model.sel(Listagem_Mean_Eco[43:48]) # WDeficit03_10Km
# WDeficit8910 <- model.sel(Listagem_Mean_Eco[49:54]) # WDeficit8910_25Km
# WDeficit99 <- model.sel(Listagem_Mean_Eco[55:60]) # WDeficit99_10Km
# YrlPrec <- model.sel(Listagem_Mean_Eco[61:66]) # YrlPrec_25Km
# bldfie <- model.sel(Listagem_Mean_Eco[67:72]) # bldfie_75Km
# elevation <- model.sel(Listagem_Mean_Eco[73:78]) # elevation_100Km
# phihox <- model.sel(Listagem_Mean_Eco[79:84]) # phihox_25Km
# phikcl <- model.sel(Listagem_Mean_Eco[85:90]) # phikcl_25Km
# slope <- model.sel(Listagem_Mean_Eco[91:96]) # slope_100Km
# sndppt <- model.sel(Listagem_Mean_Eco[97:102]) # sndppt_05Km
# treecover2000 <- model.sel(Listagem_Mean_Eco[103:108]) # treecover2000_100Km
# 
# # AIC, Mean, Socio
# 
# names(Listagem_Mean_Socio)
# BAI1317 <- model.sel(Listagem_Mean_Socio[1:6]) # BAI1317_25Km
# Cropland <- model.sel(Listagem_Mean_Socio[7:12]) # Cropland_75Km
# GrossDef <- model.sel(Listagem_Mean_Socio[13:18]) # GrossDef_100Km
# HFPrint <- model.sel(Listagem_Mean_Socio[19:24]) # HFPrint_25Km
# IDH03 <- model.sel(Listagem_Mean_Socio[25:30]) # IDH03_25Km
# IDH9010 <- model.sel(Listagem_Mean_Socio[31:36]) # IDH9010_25Km
# NBR1317 <- model.sel(Listagem_Mean_Socio[37:42]) # NBR1317_75Km
# OpCostIISMean <- model.sel(Listagem_Mean_Socio[43:48]) # OpCostIISMean_10Km
# OpCostIIS <- model.sel(Listagem_Mean_Socio[49:54]) # OpCostIIS_10Km_SUM
# PastAgric <- model.sel(Listagem_Mean_Socio[55:60]) # PastAgric_50Km
# PercUrbArea09 <- model.sel(Listagem_Mean_Socio[61:66]) # PercUrbArea09_50Km
# RuralPop <- model.sel(Listagem_Mean_Socio[67:72]) # RuralPop_100Km
# RuralPvty <- model.sel(Listagem_Mean_Socio[73:78]) # RuralPvty_50Km
# SustainablePA <- model.sel(Listagem_Mean_Socio[79:84]) # SustainablePA_05Km
# T1RoadDensity <- model.sel(Listagem_Mean_Socio[85:90]) # T1RoadDensity_50Km
# T2RoadDensity <- model.sel(Listagem_Mean_Socio[91:96]) # T2RoadDensity_75Km
# T3RoadDensity <- model.sel(Listagem_Mean_Socio[97:102]) # T3RoadDensity_75Km
# T4RoadDensity <- model.sel(Listagem_Mean_Socio[103:108]) # T4RoadDensity_10Km
# T5RoadDensity <- model.sel(Listagem_Mean_Socio[109:114]) # T5RoadDensity_25Km
# TotalRoadDensity <- model.sel(Listagem_Mean_Socio[115:120]) # TotalRoadDensity_25Km
# cdd <- model.sel(Listagem_Mean_Socio[121:126]) # cdd_100Km
# cropIIS <- model.sel(Listagem_Mean_Socio[127:132]) # cropIIS_05Km
# croppastureIIS <- model.sel(Listagem_Mean_Socio[133:138]) # croppastureIIS_10Km
# deltaGHSL9015 <- model.sel(Listagem_Mean_Socio[139:144]) # deltaGHSL9015_100Km
# deltaGPWCount0010 <- model.sel(Listagem_Mean_Socio[145:150]) # deltaGPWCount0010_100Km
# deltaGPWDensity0010 <- model.sel(Listagem_Mean_Socio[151:156]) # deltaGPWDensity0010_100Km
# fty <- model.sel(Listagem_Mean_Socio[157:162]) # fty_100Km
# govrnce <- model.sel(Listagem_Mean_Socio[163:168]) # govrnce_25Km
# pastoIIS <- model.sel(Listagem_Mean_Socio[169:174]) # pastoIIS_25Km
# sa <- model.sel(Listagem_Mean_Socio[175:180]) # sa_05Km
# strictlyPA <- model.sel(Listagem_Mean_Socio[181:186]) # strictlyPA_100Km
# 
# wfire <- model.sel(Listagem_Mean_Socio[191:196]) # wfire_50Km
# 
# 
# # Combining variables  ----------------------------------------------------
# # Using Variance
# # Eco
# WD <- model.sel(c(Listagem_Var_Eco[43:48], Listagem_Var_Eco[49:54], Listagem_Var_Eco[55:60])) # WDeficit99_05Km
# NPP <- model.sel(c(Listagem_Var_Eco[25:30], Listagem_Var_Eco[31:36])) # NPP03_10Km
# PREC <- model.sel(c(Listagem_Var_Eco[37:42], Listagem_Var_Eco[61:66], Listagem_Var_Eco[13:18])) # PrecSeanslty_50Km
# Cover <-  model.sel(c(Listagem_Var_Eco[19:24], Listagem_Var_Eco[109:114], Listagem_Var_Eco[79:84])) #treecover2000_05Km
# PH <- model.sel(c(Listagem_Var_Eco[85:90], Listagem_Var_Eco[91:96])) # phihox_25Km
# 
# # Socio
# RoadDensity <- model.sel(c(Listagem_Var_Socio[85:90], Listagem_Var_Socio[91:96], Listagem_Var_Socio[97:102], Listagem_Var_Socio[103:108], Listagem_Var_Socio[109:114], Listagem_Var_Socio[115:120])) # T3RoadDensity_10Km
# 
# Commodity <- model.sel(c(Listagem_Var_Socio[121:126], Listagem_Var_Socio[157:162], Listagem_Var_Socio[175:180], Listagem_Var_Socio[187:192])) # cdd_05Km = Igual resultado do artigo.
# 
# IDH <- model.sel(c(Listagem_Var_Socio[25:30], Listagem_Var_Socio[31:36])) # IDH03_05Km
# PA <- model.sel(c(Listagem_Var_Socio[79:84], Listagem_Var_Socio[181:186])) #SustainablePA_25Km 
# NBR_BAI <- model.sel(c(Listagem_Var_Socio[3:6], Listagem_Var_Socio[37:42])) # BAI1317_10Km
# POP <- model.sel(c(Listagem_Var_Socio[139:144], Listagem_Var_Socio[145:150], Listagem_Var_Socio[151:156])) # deltaGHSL9015_50Km
# 
# IDH_Gov <- model.sel(c(Listagem_Var_Socio[25:30], Listagem_Var_Socio[31:36], Listagem_Var_Socio[163:168])) # IDH03_05Km
# 
# Agropec <- model.sel(c(Listagem_Var_Socio[127:132], Listagem_Var_Socio[133:138], Listagem_Var_Socio[169:174], Listagem_Var_Socio[55:60], Listagem_Var_Socio[7:12], Listagem_Var_Socio[175:180])) # pastoIIS_05Km
# #Combinação de CropIIS, CropPastureIIS, PastoIIS, PastAgric, Crop, SADD
# 
# Crop_Pasture <- model.sel(c(Listagem_Var_Socio[133:138], Listagem_Var_Socio[55:60])) #PastAgric_50Km
# #Combinacao decrop.pastureISS_50km   PastAgric_25km
# 
# Crop <- model.sel(c(Listagem_Var_Socio[7:12], Listagem_Var_Socio[127:132])) # Cropland_05Km
# # Combinacao Cropland e crop IIS
# 
# 
# # PastAgric = Percentage of cropland OR agriculture;
# # warning Seems to be wrong. As the link shows, the layer is just for pasture;
# #
# 
# # Using Mean
# # Eco
# WD <- model.sel(c(Listagem_Mean_Eco[43:48], Listagem_Mean_Eco[49:54], Listagem_Mean_Eco[55:60])) # WDeficit8910_25Km
# NPP <- model.sel(c(Listagem_Mean_Eco[25:30], Listagem_Mean_Eco[31:36])) # NPP0010_50Km
# PREC <- model.sel(c(Listagem_Mean_Eco[37:42], Listagem_Mean_Eco[61:66], Listagem_Mean_Eco[13:18])) # DriestQuartes_25Km
# Cover <- model.sel(c(Listagem_Mean_Eco[19:24], Listagem_Mean_Eco[103:108])) # ForestUntl10_100Km
# 
# 
# # Socio
# RoadDensity <- model.sel(c(Listagem_Mean_Socio[85:90], Listagem_Mean_Socio[91:96], Listagem_Mean_Socio[97:102], Listagem_Mean_Socio[103:108], Listagem_Mean_Socio[109:114], Listagem_Mean_Socio[115:120])) # T1RoadDensity_50Km
# Commodity <- model.sel(c(Listagem_Mean_Socio[121:126], Listagem_Mean_Socio[157:162], Listagem_Mean_Socio[175:180], Listagem_Mean_Socio[191:196])) # Falta URB - wfire_50Km
# IDH <- model.sel(c(Listagem_Mean_Socio[25:30], Listagem_Mean_Socio[31:36])) # IDH9010_25Km
# PA <- model.sel(c(Listagem_Mean_Socio[79:84], Listagem_Mean_Socio[181:186])) #SustainablePA_05Km
# NBR_BAI <- model.sel(c(Listagem_Mean_Socio[1:6], Listagem_Mean_Socio[37:42])) # BAI1317_25Km
# POP <- model.sel(c(Listagem_Mean_Socio[139:144], Listagem_Mean_Socio[145:150], Listagem_Mean_Socio[151:156])) # deltaGPWCount0010_100Km
# 
# ######################Parei de criar em outro script###########################
# 
# # Using Variance Inflation Factor -----------------------------------------
# # Using Var
# 
# Eco_Var <- lm(RR_var ~ Time_mean + AnualTemp_100Km + CEC_100Km + PrecSeanslty_10Km +  ForestUntl10_100Km + NPP0010_50Km + WDeficit8910_10Km +  bldfie_75Km + elevation_100Km + phihox_25Km + phikcl_25Km + slope_100Km + sndppt_05Km, data = data)
# 
# 
# 
# 
# 
# 
# Eco_Var <- c("Time_mean", "AnualTemp_100Km", "CEC_100Km", "ForestUntl10_100Km", "NPP03_10Km", "PrecSeanslty_10Km", "WDeficit8910_10Km", "elevation_100Km", "slope_05Km", "sndppt_05Km")
# 
# correlacao_Eco <- corrplot(cor(data[Eco_Var]))
# 
# step.model_Eco_Var <- stepAIC(Eco_Var, direction = "both", trace = F)
# 
# 
# # Using mean
# # Model selection using Dredge function -----------------------------------
# demo(dredge.subset)
# #<Return>
# 
# # Subsseting function
# options(na.action = "na.fail")
# is.correlated <- function(i, j, data, conf.level = .95, cutoff = .6, ...) {
#   if(j >= i) return(NA)
#   ct <- cor.test(data[, i], data[, j], conf.level = conf.level, ...)
#   ct$p.value > (1 - conf.level) || abs(ct$estimate) <= cutoff
# }
# 
# vCorrelated <- Vectorize(is.correlated, c("i", "j"))
# 
# # Using RR_var ------------------------------------------------------------
# # Eco
# df_Eco_Var <- data.frame(data$RR_var, data$Time_mean, data$AnualTemp_100Km, data$CEC_100Km, data$ForestUntl10_100Km, data$NPP0010_50Km, data$PrecSeanslty_10Km, data$WDeficit8910_10Km, data$sndppt_05Km, data$elevation_100Km, data$slope_100Km, data$treecover2000_100Km)
# 
# nm <- colnames(df_Eco_Var[-1]) %>% 
#   gsub(pattern = "data.", replacement = "")
# 
# smat <- outer(1:11, 1:11, vCorrelated, data = df_Eco_Var)
# dimnames(smat) <- list(nm, nm)
# 
# Eco_Var <- lm(RR_var ~ Time_mean + AnualTemp_100Km + CEC_100Km + ForestUntl10_100Km + NPP0010_50Km + PrecSeanslty_10Km + WDeficit8910_10Km + sndppt_05Km + elevation_100Km + slope_100Km + treecover2000_100Km, data = data)
# Dredge_Eco_Var <- dredge(Eco_Var, subset = smat)
# 
# # Socio
# df_Socio_Var <- data.frame(data$RR_var, data$BAI1317_25Km, data$Cropland_75Km, data$GrossDef_100Km, data$HFPrint_25Km, data$IDH9010_25Km, data$OpCostIISMean_10Km, data$PastAgric_50Km, data$PercUrbArea09_50Km, data$RuralPop_100Km, data$RuralPvty_50Km, data$T2RoadDensity_75Km, data$wfire_50Km, data$cropIIS_05Km, data$croppastureIIS_10Km, data$deltaGHSL9015_100Km, data$govrnce_25Km, data$pastoIIS_25Km, data$SustainablePA_05Km)
#   
# nm <- colnames(df_Socio_Var[-1]) %>% 
#   gsub(pattern = "data.", replacement = "")
# 
# smat <- outer(1:18, 1:18, vCorrelated, data = df_Socio_Var)
# dimnames(smat) <- list(nm, nm)
# 
# Socio_Var <- lm(RR_var ~ BAI1317_25Km + Cropland_75Km + GrossDef_100Km + HFPrint_25Km + IDH9010_25Km + OpCostIISMean_10Km + PastAgric_50Km + PercUrbArea09_50Km + RuralPop_100Km + RuralPvty_50Km + T2RoadDensity_75Km + wfire_50Km + cropIIS_05Km + croppastureIIS_10Km + deltaGHSL9015_100Km + govrnce_25Km + pastoIIS_25Km + SustainablePA_05Km, data = data)
#     
# Dredge_Socio_Var <- dredge(Socio_Var, subset = smat)
# 
# # Geral 
# df_Geral_Var <- data.frame(data$RR_var, data$Time_mean, data$AnualTemp_100Km, data$CEC_100Km, data$ForestUntl10_100Km, data$NPP0010_50Km, data$PrecSeanslty_10Km, data$WDeficit8910_10Km, data$sndppt_05Km, data$elevation_100Km, data$slope_100Km, data$treecover2000_100Km, data$BAI1317_25Km, data$Cropland_75Km, data$GrossDef_100Km, data$HFPrint_25Km, data$IDH9010_25Km, data$OpCostIISMean_10Km, data$PastAgric_50Km, data$PercUrbArea09_50Km, data$RuralPop_100Km, data$RuralPvty_50Km, data$T2RoadDensity_75Km, data$wfire_50Km, data$cropIIS_05Km, data$croppastureIIS_10Km, data$deltaGHSL9015_100Km, data$govrnce_25Km, data$pastoIIS_25Km, data$SustainablePA_05Km)
# 
# nm <- colnames(df_Geral_Var[-1]) %>% 
#   gsub(pattern = "data.", replacement = "")
# 
# smat <- outer(1:29, 1:29, vCorrelated, data = df_Geral_Var)
# dimnames(smat) <- list(nm, nm)
# 
# Geral_Var <- lm(RR_var ~ Time_mean + Time_mean + AnualTemp_100Km + CEC_100Km + ForestUntl10_100Km + NPP0010_50Km + PrecSeanslty_10Km + WDeficit8910_10Km + sndppt_05Km + elevation_100Km + slope_100Km + treecover2000_100Km + BAI1317_25Km + Cropland_75Km + GrossDef_100Km + HFPrint_25Km + IDH9010_25Km + OpCostIISMean_10Km + PastAgric_50Km + PercUrbArea09_50Km + RuralPop_100Km + RuralPvty_50Km + T2RoadDensity_75Km + wfire_50Km + cropIIS_05Km + croppastureIIS_10Km + deltaGHSL9015_100Km + govrnce_25Km + pastoIIS_25Km + SustainablePA_05Km, data = data)
# Dredge_Geral_Var <- dredge(Geral_Var, subset = smat)
# 
# 
# # Using RR_mean ------------------------------------------------------------
# # Eco
# df_Eco_Mean <- data.frame(data$RRmean, data$Time_mean, data$AnualTemp_100Km, data$CEC_100Km, data$DriestQuartes_25Km, data$ForestUntl10_100Km, data$NPP0010_50Km, data$WDeficit8910_25Km, data$sndppt_05Km, data$elevation_100Km, data$slope_100Km, data$treecover2000_100Km)
#   
# nm <- colnames(df_Eco_Mean[-1]) %>% 
#   gsub(pattern = "data.", replacement = "")
# 
# smat <- outer(1:11, 1:11, vCorrelated, data = df_Eco_Mean)
# dimnames(smat) <- list(nm, nm)
# 
# Eco_Mean <- lm(RRmean ~ Time_mean + AnualTemp_100Km + CEC_100Km + DriestQuartes_25Km + ForestUntl10_100Km + NPP0010_50Km + WDeficit8910_25Km + sndppt_05Km + elevation_100Km + slope_100Km + treecover2000_100Km, data = data)
# Dredge_Eco_Mean <- dredge(Eco_Mean, subset = smat)
# 
# # Socio
# df_Socio_Mean <- data.frame(data$RRmean,data$BAI1317_25Km, data$Cropland_75Km, data$GrossDef_100Km, data$HFPrint_25Km, data$IDH9010_25Km, data$OpCostIISMean_10Km, data$PastAgric_50Km, data$PercUrbArea09_50Km, data$RuralPop_100Km, data$RuralPvty_50Km, data$SustainablePA_05Km, data$T1RoadDensity_50Km, data$wfire_50Km, data$cropIIS_05Km, data$croppastureIIS_10Km, data$deltaGPWCount0010_100Km, data$govrnce_25Km, data$pastoIIS_25Km)
# 
# nm <- colnames(df_Socio_Mean[-1]) %>% 
#   gsub(pattern = "data.", replacement = "")
# 
# smat <- outer(1:18, 1:18, vCorrelated, data = df_Socio_Mean)
# dimnames(smat) <- list(nm, nm)
# 
# Socio_Var <- lm(RRmean ~ BAI1317_25Km + Cropland_75Km + GrossDef_100Km + HFPrint_25Km + IDH9010_25Km + OpCostIISMean_10Km + PastAgric_50Km + PercUrbArea09_50Km + RuralPop_100Km + RuralPvty_50Km + SustainablePA_05Km + T1RoadDensity_50Km + wfire_50Km + cropIIS_05Km + croppastureIIS_10Km + deltaGPWCount0010_100Km + govrnce_25Km + pastoIIS_25Km, data = data)
# Dredge_Socio_Mean <- dredge(Socio_Mean, subset = smat)
# 
# # Geral 
# df_Geral_Mean <- data.frame(data$RRmean, data$Time_mean, data$AnualTemp_100Km, data$CEC_100Km, data$DriestQuartes_25Km, data$ForestUntl10_100Km, data$NPP0010_50Km, data$WDeficit8910_25Km, data$sndppt_05Km, data$elevation_100Km, data$slope_100Km, data$treecover2000_100Km, data$BAI1317_25Km, data$Cropland_75Km, data$GrossDef_100Km, data$HFPrint_25Km, data$IDH9010_25Km, data$OpCostIISMean_10Km, data$PastAgric_50Km, data$PercUrbArea09_50Km, data$RuralPop_100Km, data$RuralPvty_50Km, data$SustainablePA_05Km, data$T1RoadDensity_50Km, data$wfire_50Km, data$cropIIS_05Km, data$croppastureIIS_10Km, data$deltaGPWCount0010_100Km, data$govrnce_25Km, data$pastoIIS_25Km)
# 
# nm <- colnames(df_Geral_Mean[-1]) %>% 
#   gsub(pattern = "data.", replacement = "")
# 
# smat <- outer(1:29, 1:29, vCorrelated, data = df_Geral_Mean)
# dimnames(smat) <- list(nm, nm)
# 
# Geral_Mean <- lm(RRmean ~ RRmean ~ Time_mean + AnualTemp_100Km + CEC_100Km + DriestQuartes_25Km + ForestUntl10_100Km + NPP0010_50Km + WDeficit8910_25Km + sndppt_05Km + elevation_100Km + slope_100Km + treecover2000_100Km + BAI1317_25Km + Cropland_75Km + GrossDef_100Km + HFPrint_25Km + IDH9010_25Km + OpCostIISMean_10Km + PastAgric_50Km + PercUrbArea09_50Km + RuralPop_100Km + RuralPvty_50Km + SustainablePA_05Km + T1RoadDensity_50Km + wfire_50Km + cropIIS_05Km + croppastureIIS_10Km + deltaGPWCount0010_100Km + govrnce_25Km + pastoIIS_25Km, data = data)
# 
# Dredge_Geral_Mean <- dredge(Geral_Mean, subset = smat)
# 
# 
# # # Correlação --------------------------------------------------------------
# # 
# # Eco_Var <- c("AnualTemp_100Km", "CEC_100Km", "ForestUntl10_100Km", "NPP03_10Km", "PrecSeanslty_10Km", "WDeficit8910_10Km", "elevation_100Km", "slope_05Km", "sndppt_05Km", "treecover2000_100Km")
# # # Foram eliminadas por combinação as variaveis "WDeficit03", "WDeficit99", "bldfie", "phihox", "phikcl", "NPP0010", "DriestQuartes", "YrlPrec"
# # 
# # Socio_Var <- c("BAI1317_25Km", "Cropland_75Km", "GrossDef_10Km", "HFPrint_25Km", "IDH9010_25Km", "OpCostIISMean_10Km", "PastAgric_05Km", "PercUrbArea09_50Km", "RuralPop_50Km", "RuralPvty_10Km", "T2RoadDensity_75Km", "cropIIS_100Km", "croppastureIIS_100Km", "deltaGHSL9015_100Km", "deltaGPWCount0010_25Km", "deltaGPWDensity0010_25Km", "fty_100Km", "govrnce_25Km", "pastoIIS_10Km", "strictlyPA_25Km")
# # # Foram eliminadas por combinação as variaveis T1RoadDensity, T3RoadDensity, T4RoadDensity, T5RoadDensity, TotalRoadDensity, sa, wfire, cdd, IDH03, SustainablePA, NBR1317
# # correlacao_Eco <- corrplot(cor(data[Eco_Var]))
# # correlacao_Socio <- corrplot(cor(data[Socio_Var]))
# # 
# # # Ecological
# # cor.variable <- colnames(correlacao_Eco)
# # correlacao_Eco <- correlacao_Eco %>%  
# #   tbl_df() %>% 
# #   mutate(variable = cor.variable)
# # cor.results <- list()
# # 
# # for(i in 1:length(cor.variable)){
# #   result.i <- correlacao_Eco %>%
# #     select(i, variable)
# #   df.i <- data.frame(result.i)
# #   cor.results[[i]] <- df.i[df.i[,1] > 0.6 | df.i[,1] < -0.6,]
# # }
# # names(cor.results) <-  cor.variable
# # 
# # # Socio
# # cor.variable <- colnames(correlacao_Socio)
# # correlacao_Socio <- correlacao_Socio %>%  
# #   tbl_df() %>% 
# #   mutate(variable = cor.variable)
# # cor.results <- list()
# # 
# # for(i in 1:length(cor.variable)){
# #   result.i <- correlacao_Socio %>%
# #     select(i, variable)
# #   df.i <- data.frame(result.i)
# #   cor.results[[i]] <- df.i[df.i[,1] > 0.6 | df.i[,1] < -0.6,]
# # }
# # names(cor.results) <-  cor.variable
# # 
# # 
# # Eco_Var <- c("CEC_100Km", "ForestUntl10_100Km", "NPP03_10Km", "PrecSeanslty_10Km", "WDeficit8910_10Km", "elevation_100Km", "slope_05Km", "sndppt_05Km")
# # #Variaveis eliminadas por correlação, treecover2000, AnualTemp
# # 
# # Socio_Var <- c("BAI1317_25Km", "Cropland_75Km", "GrossDef_10Km", "OpCostIISMean_10Km", "PercUrbArea09_50Km", "RuralPvty_10Km", "T2RoadDensity_75Km", "cropIIS_100Km", "croppastureIIS_100Km", "deltaGHSL9015_100Km", "deltaGPWCount0010_25Km", "fty_100Km", "govrnce_25Km", "pastoIIS_10Km", "strictlyPA_25Km")
# # # Variaveis eliminadas por correlação, HFPrint, PastAgric, RuralPop, deltaGPWDensity0010, IDH9010
# 
# # Modeling direct effect -------------------------------------------------------
# 
# # Eco
# # Variancia
# model_eco <- lm(RR_var ~ Time_mean + WDeficit8910_10Km + sndppt_05Km + AnualTemp_100Km, data = data)
# summary(model_eco)
# 
# #Socio
# #Variancia
# model_socio <-  lm(RR_var ~ GrossDef_10Km + cropIIS_100Km + deltaGPWCount0010_25Km + fty_100Km , data = data)
# summary(model_socio)
# 
# 
# # Eco
# # média
# model_eco <- lm(RRmean ~ Time_mean + WDeficit8910_10Km + sndppt_05Km + AnualTemp_100Km, data = data)
# summary(model_eco)
# 
# #Socio
# # Media
# model_socio <-  lm(RRmean ~ GrossDef_10Km + cropIIS_100Km + deltaGPWCount0010_25Km + fty_100Km , data = data)
# summary(model_socio)
# 
# 
# 
# # Path_Model --------------------------------------------------------------
# ## Specifying control values for lme fit
# control = lmeControl(opt='optim', optimMethod = "BFGS", maxIter = 1000, msMaxIter = 1000, niterEM = 1000)
# 
# unificado <- psem(
#   direct <- lme(RR_var ~ WDeficit8910_10Km + Time_mean + cropIIS_100Km, data = data, control = control, methods = "ML"
#                 
#                 
#                 
#                 UrbArea_10km + pop_count_100km, random = ~ UrbArea_10km + pop_count_100km | Site, data = data.D, control = control, method = "ML"),
#   ind.urbarea <- lme(UrbArea_10km ~ RoadDensity_100km + IDH03_5km , random = ~ RoadDensity_100km + IDH03_5km | Site, data = data.D, control = control, method = "ML"),
#   ind.migration <- lme(pop_count_100km ~ IDH03_5km  + RoadDensity_100km + UrbArea_10km, random = ~ IDH03_5km  + RoadDensity_100km + UrbArea_10km | Site, data = data.D, control = control, method = "ML")
# )
# summary (unificado6)
