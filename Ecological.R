# Ecological -----
install.packages("tidyverse", dependencies = T)
library(tidyverse)
library(MuMIn)

# Reading dataset----
data <- read_csv("RData/Datafull2.csv")

# Dataframe for Eco Variables  ------------------------------------
Eco <- data %>%
  select(Site, RR_var, RRmean, Time_mean, AnualTemp_05Km : AnualTemp_75Km, CEC_05Km: CEC_75Km, DriestQuartes_05Km: ForestUntl10_75Km, NPP0010_05Km : NPP03_75Km, PrecSeanslty_05Km : PrecSeanslty_75Km, WDeficit03_05Km: bldfie_75Km, elevation_05Km: f2003_75Km, phihox_05Km: phikcl_75Km, slope_05Km: sndppt_75Km, treecover2000_05Km: treecover2000_75Km, Taxon)
colnames(Eco)


# Model selection ---------------------------------------------------------

list_variable_Eco <- colnames(Eco[5:118])
lm_variable_Eco <- as.data.frame(Eco[5:118])

# Using Variance

Listagem_Var_Eco <- list()
for(i in 1:length(list_variable_Eco)){
  #i = 1
  Listagem_Var_Eco [[i]] <- lm(RR_var ~ lm_variable_Eco[,i], data = Eco)
}
names(Listagem_Var_Eco) <- list_variable_Eco


# Model.sel Function ----
# AIC, Variance, Eco
names(Listagem_Var_Eco)
AnualTemp <- model.sel(Listagem_Var_Eco[1:6]) # AnualTemp_05Km
CEC <- model.sel(Listagem_Var_Eco[7:12]) # CEC_05Km
#DriestQuartes <- model.sel(Listagem_Var_Eco[13:18]) # DriestQuartes_10Km 
#ForestUntl10 <- model.sel(Listagem_Var_Eco[19:24]) # ForestUntl10_100Km
#NPP0010 <- model.sel(Listagem_Var_Eco[25:30]) # NPP0010_05Km 
#NPP03 <- model.sel(Listagem_Var_Eco[31:36]) # NPP03_100Km
#PrecSeanslty <- model.sel(Listagem_Var_Eco[37:42]) # PrecSeanslty_10Km
#WDeficit03 <- model.sel(Listagem_Var_Eco[43:48]) # WDeficit03_05Km
#WDeficit8910 <- model.sel(Listagem_Var_Eco[49:54]) # WDeficit8910_100Km
#WDeficit99 <- model.sel(Listagem_Var_Eco[55:60]) # WDeficit99_100Km
#YrlPrec <- model.sel(Listagem_Var_Eco[61:66]) # YrlPrec_25Km
bldfie <- model.sel(Listagem_Var_Eco[67:72]) # bldfie_75Km
elevation <- model.sel(Listagem_Var_Eco[73:78]) # elevation_05Km
#f2003 <- model.sel(Listagem_Var_Eco[79:84]) # f2003_100Km 
phihox <- model.sel(Listagem_Var_Eco[85:90]) # phihox_25Km
#phikcl <- model.sel(Listagem_Var_Eco[91:96]) # phikcl_05Km
slope <- model.sel(Listagem_Var_Eco[97:102]) # slope_50Km
sndppt <- model.sel(Listagem_Var_Eco[103:108]) # sndppt_100Km
#treecover2000 <- model.sel(Listagem_Var_Eco[109:114]) # treecover2000_100Km

# Combinações Var
WD <- model.sel(c(Listagem_Var_Eco[43:48], Listagem_Var_Eco[49:54], Listagem_Var_Eco[55:60])) # WDeficit99_05Km 
NPP <- model.sel(c(Listagem_Var_Eco[25:30], Listagem_Var_Eco[31:36])) # NPP0010_50Km
PREC <- model.sel(c(Listagem_Var_Eco[37:42], Listagem_Var_Eco[61:66], Listagem_Var_Eco[13:18])) # DriestQuartes_25Km
Cover <-  model.sel(c(Listagem_Var_Eco[19:24], Listagem_Var_Eco[109:114], Listagem_Var_Eco[79:84])) # ForestUntl10_25Km
PH <- model.sel(c(Listagem_Var_Eco[85:90], Listagem_Var_Eco[91:96])) # phihox_25Km

#Final table ----
Eco_Var <- data %>% 
  select(Site, RR_var,Time_mean, AnualTemp_05Km, CEC_05Km, DriestQuartes_25Km, ForestUntl10_25Km, NPP0010_50Km, WDeficit99_05Km , bldfie_75Km, elevation_05Km, phihox_25Km, slope_50Km, sndppt_100Km)

write_csv(Eco_Var, "RData/Eco_Var2.csv")

#### TERMINA AQUI #####
#Hist----- chamamos datafull_base2 do pré-processing antes de scalonar e tiramos os outlyers

Hist_Eco <- datafull_base2
#Eco_Var<-Hist_Eco

hist(Hist_Eco$AnualTemp_05Km,100)
mean(Hist_Eco$AnualTemp_05Km)##229
plot(Eco_Var$RR_var ~ Eco_Var$AnualTemp_05Km)
x<-lm(Eco_Var$RR_var ~ Eco_Var$AnualTemp_05Km)
abline(x)

hist(Hist_Eco$CEC_05Km,100)
plot(Eco_Var$RR_var ~ Eco_Var$CEC_05Km)
x<-lm(Eco_Var$RR_var ~ Eco_Var$CEC_05Km)
abline(x)

hist(Hist_Eco$WDeficit99_05Km,100)
mean(Hist_Eco$WDeficit99_05Km) ##213.5
plot(Eco_Var$RR_var ~ Eco_Var$WDeficit99_05Km)
x<-lm(Eco_Var$RR_var ~ Eco_Var$WDeficit99_05Km)
abline(x)

hist(Hist_Eco$ForestUntl10_25Km,100)
mean(Hist_Eco$ForestUntl10_25Km)##58% - maioria entre 30 e 70%
plot(Eco_Var$RR_var ~ Eco_Var$ForestUntl10_25Km)
x<-lm(Eco_Var$RR_var ~ Eco_Var$ForestUntl10_25Km)
abline(x)

hist(Hist_Eco$NPP0010_50Km,100)
plot(Eco_Var$RR_var ~ Eco_Var$NPP0010_50Km)
x<-lm(Eco_Var$RR_var ~ Eco_Var$NPP0010_50Km)
abline(x)

hist(Hist_Eco$DriestQuartes_25Km,100)
mean(Hist_Eco$DriestQuartes_25Km)##228
plot(Eco_Var$RR_var ~ Eco_Var$DriestQuartes_25Km)
x<-lm(Eco_Var$RR_var ~ Eco_Var$DriestQuartes_25Km)
abline(x)

hist(Hist_Eco$bldfie_75Km,100)
plot(Eco_Var$RR_var ~ Eco_Var$bldfie_75Km)
x<-lm(Eco_Var$RR_var ~ Eco_Var$bldfie_75Km)
abline(x)

hist(Hist_Eco$elevation_05Km,100)
mean(Hist_Eco$elevation_05Km)##661
plot(Eco_Var$RR_var ~ Eco_Var$elevation_05Km)
x<-lm(Eco_Var$RR_var ~ Eco_Var$elevation_05Km)
abline(x)

hist(Hist_Eco$phihox_25Km,100)
plot(Eco_Var$RR_var ~ Eco_Var$phihox_25Km)
x<-lm(Eco_Var$RR_var ~ Eco_Var$phihox_25Km)
abline(x)

hist(Hist_Eco$slope_50Km,100)
mean(Hist_Eco$slope_50Km)##3.6
plot(Eco_Var$RR_var ~ Eco_Var$slope_50Km)
x<-lm(Eco_Var$RR_var ~ Eco_Var$slope_50Km)
abline(x)

hist(Hist_Eco$sndppt_100Km,100)
plot(Eco_Var$RR_var ~ Eco_Var$sndppt_100Km)
x<-lm(Eco_Var$RR_var ~ Eco_Var$sndppt_100Km)
abline(x)

