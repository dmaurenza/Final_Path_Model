library(tidyverse)
library(car)
library(corrplot)
# Global

Eco_Var <- read_csv("RData/Eco_Var2.csv")
Socio_Var <- read_csv("RData/Socio_Var2.csv")
Global_Var <- left_join(Eco_Var, Socio_Var, by = "Site") %>% 
  select(-RR_var.y) %>% 
  rename(RR_var = RR_var.x)
colnames(Global_Var)

Pearson <- cor.test(data$RRmean, data$RR_var)

Global_Var_VIF <- vif(mod = lm(RR_var ~ Time_mean + AnualTemp_05Km + CEC_05Km + DriestQuartes_25Km + ForestUntl10_25Km + NPP0010_50Km + WDeficit99_05Km + bldfie_75Km + elevation_05Km + phihox_25Km + slope_50Km + sndppt_100Km + BAI1317_10Km + cropIIS_05Km + GrossDef_100Km + IDH03_25Km + OpCostIIS_100Km_SUM + pastoIIS_10Km + PercUrbArea09_50Km + RuralPop_50Km + RuralPvty_05Km + T1RoadDensity_50Km + cdd_100Km + deltaGPWDensity0010_25Km + fty_25Km + sa_50Km + strictlyPA_100Km, data = Global_Var))

## VERSÃO FINAL!!!!!!!!!!!!!!!!!!
#Não espero efeito direto - IDH03_25Km  
#VIF -  AnualTemp_05Km + cdd_100Km + DriestQuartes_25Km + CEC_05Km +  phihox_25Km + sa_50Km + T1RoadDensity_50Km + elevation_05Km + WDeficit99_05Km + 
Global_Var_VIF <- vif(mod = lm(RR_var ~ Time_mean +ForestUntl10_25Km + NPP0010_50Km + bldfie_75Km +   slope_50Km + sndppt_100Km + BAI1317_10Km + cropIIS_05Km + GrossDef_100Km +  OpCostIIS_100Km_SUM + pastoIIS_10Km + PercUrbArea09_50Km + RuralPop_50Km + RuralPvty_05Km +  deltaGPWDensity0010_25Km + fty_25Km +  strictlyPA_100Km, data = Global_Var))
sort(Global_Var_VIF)

# Stepwise ---
step.model_Global_Var <- lm(RR_var ~ Time_mean +ForestUntl10_25Km + NPP0010_50Km + bldfie_75Km +   slope_50Km + sndppt_100Km + BAI1317_10Km + cropIIS_05Km + GrossDef_100Km +  OpCostIIS_100Km_SUM + pastoIIS_10Km + PercUrbArea09_50Km + RuralPop_50Km + RuralPvty_05Km +  deltaGPWDensity0010_25Km + fty_25Km +  strictlyPA_100Km, data = Global_Var)

summary(step.model_Global_Var)
step.model_Results <- MASS::stepAIC(step.model_Global_Var, direction = "both", trace = F)

# LM Final 
Global_Var_Final <- lm(formula = RR_var ~ Time_mean + ForestUntl10_25Km + bldfie_75Km + slope_50Km + cropIIS_05Km + OpCostIIS_100Km_SUM + PercUrbArea09_50Km + RuralPop_50Km + RuralPvty_05Km + deltaGPWDensity0010_25Km +  fty_25Km + strictlyPA_100Km, data = Global_Var)

summary(Global_Var_Final)






























## VERSÃO FINAL!!!!!!!!!!!!!!!!!!
# TESTE 2 -----
# PercUrbArea09_05Km + T1RoadDensity_50Km + cdd_100Km + fty_25Km + strictlyPA_75Km + IDH03_50Km +               AnualTemp_25Km + elevation_05Km + DriestQuartes_25Km + PastAgric_10Km + phihox_10Km + GrossDef_100Km  + f2003_100Km +  RuralPvty_100Km

Global_Var_VIF <- vif(mod = lm(RR_var ~ Time_mean + WDeficit99_100Km + CEC_10Km   + NPP03_100Km +  bldfie_05Km   + slope_25Km + sndppt_50Km + BAI1317_10Km + cropIIS_100Km +  OpCostIIS_100Km_SUM   + RuralPop_75Km +  deltaGHSL9015_25Km + sa_25Km, data = Global_Var))

step.model_Global_Var <- lm(RR_var ~ Time_mean + WDeficit99_100Km + CEC_10Km   + NPP03_100Km +  bldfie_05Km   + slope_25Km + sndppt_50Km + BAI1317_10Km + cropIIS_100Km +  OpCostIIS_100Km_SUM   + RuralPop_75Km +  deltaGHSL9015_25Km + sa_25Km  , data = Global_Var)


summary(step.model_Global_Var)
step.model_Results <- MASS::stepAIC(step.model_Global_Var, direction = "both", trace = F)

# LM Final 
Global_Var_Final <- lm(formula = RR_var ~ NPP03_100Km + bldfie_05Km + slope_25Km + sndppt_50Km + BAI1317_10Km + OpCostIIS_100Km_SUM + sa_25Km, data = Global_Var)

summary(Global_Var_Final)


### TESTE 2 Correlacao-----


Global_Var_Variables <- c("CEC_10Km",   "NPP03_100Km", "WDeficit99_100Km", "bldfie_05Km", "slope_25Km", "sndppt_50Km", "BAI1317_10Km", "cropIIS_100Km",  "OpCostIIS_100Km_SUM", "RuralPop_75Km", "deltaGHSL9015_25Km", "sa_25Km")

correlacao_Global_Var <- corrplot(cor(Global_Var[Global_Var_Variables]))


# Valores de Correlação ---

cor.variable <- colnames(correlacao_Global_Var)
correlacao_Global_Var <- correlacao_Global_Var %>% 
  tbl_df() %>% 
  mutate(variable = cor.variable)
cor.results <- list()

for(i in 1:length(cor.variable)){
  results.i <- correlacao_Global_Var %>% 
    select(i, variable)
  df.i <- data.frame(results.i)
  cor.results[[i]] <- df.i[df.i[,1] > 0.6 | df.i[,1] < -0.6,]
}
names(cor.results) <- cor.variable


# Teste 3 - -----
# Eliminando as variáveis com distribuição dos pontos ruins + PercUrbArea09_05Km + T1RoadDensity_50Km + cdd_100Km + fty_05Km + strictlyPA_75Km + IDH03_50Km ## NESSA SAIU FOGO SÓ PARA VER O QUE ACONTECE
# Eliminados pro VIF alto - AnualTemp_25Km + DriestQuartes_25Km + phihox_10Km +elevation_05Km + WDeficit99_100Km + PastAgric_10Km + 
#Global_Var_VIF <- vif(mod = lm(RR_var ~ Time_mean  + CEC_10Km +  f2003_100Km + NPP03_100Km +  bldfie_05Km +  slope_25Km + sndppt_50Km + cropIIS_100Km + GrossDef_100Km +  OpCostIIS_100Km_SUM +  RuralPop_75Km + RuralPvty_100Km +  deltaGHSL9015_25Km +  sa_05Km, data = Global_Var))

# Stepwise ---
#step.model_Global_Var <- lm(RR_var ~ Time_mean  + CEC_10Km +  f2003_100Km + NPP03_100Km +  bldfie_05Km +  slope_25Km + sndppt_50Km + cropIIS_100Km + GrossDef_100Km +  OpCostIIS_100Km_SUM +  RuralPop_75Km + RuralPvty_100Km +  deltaGHSL9015_25Km +  sa_05Km, data = Global_Var)

#summary(step.model_Global_Var)
#step.model_Results <- MASS::stepAIC(step.model_Global_Var, direction = "both", trace = F)

# LM Final 
#Global_Var_Final <- lm(formula = RR_var ~ NPP03_100Km + bldfie_05Km + slope_25Km + 
#                         sndppt_50Km + GrossDef_100Km + OpCostIIS_100Km_SUM + sa_05Km, 
 #                      data = Global_Var)

#summary(Global_Var_Final)

#Teste 1 - -----
#Eliminando as variáveis com distribuição dos pontos ruins + PercUrbArea09_05Km + T1RoadDensity_50Km + cdd_100Km + fty_05Km + strictlyPA_75Km + IDH03_50Km ##
# Eliminados pro VIF alto - + RuralPvty_50Km + elevation_05Km + phihox_10Km + WDeficit99_100Km + DriestQuartes_10Km + OpCostIIS_100Km_SUM + AnualTemp_25Km + 
Global_Var_VIF <- vif(mod = lm(RR_var ~ Time_mean + CEC_10Km +   NPP03_100Km +  bldfie_05Km +   slope_25Km + sndppt_50Km + BAI1317_10Km + cropIIS_100Km +  RuralPop_75Km +  deltaGHSL9015_25Km + sa_25Km +  PastAgric_05Km  +GrossDef_100Km  + f2003_100Km , data = Global_Var))

sort(Global_Var_VIF)
# Stepwise ---
step.model_Global_Var <- lm(RR_var ~ Time_mean + CEC_10Km +   NPP03_100Km +  bldfie_05Km +   slope_25Km + sndppt_50Km + BAI1317_10Km + cropIIS_100Km +  RuralPop_75Km +  deltaGHSL9015_25Km + sa_25Km +  PastAgric_05Km  +GrossDef_100Km  + f2003_100Km , data = Global_Var)

summary(step.model_Global_Var)
step.model_Results <- MASS::stepAIC(step.model_Global_Var, direction = "both", trace = F)

# LM Final 
Global_Var_Final <- lm(formula = RR_var ~ NPP03_100Km + bldfie_05Km + slope_25Km + sndppt_50Km + BAI1317_10Km + sa_25Km, data = Global_Var)

summary(Global_Var_Final)
#plot(Global_Var$RR_var ~ Global_Var$BAI1317_10Km)


# TESTE 4 FINAL SÓ VER SE ALGUMA VARIÁVEL DOIDA AFETARIA -----
# PercUrbArea09_05Km + cdd_100Km + fty_25Km + strictlyPA_75Km +  AnualTemp_25Km + elevation_05Km + PastAgric_10Km + phihox_10Km + GrossDef_100Km  + Ruralpop...
#+ waterde.... + DriestQuartes_25Km 

Global_Var_VIF <- vif(mod = lm(RR_var ~ Time_mean + f2003_100Km + CEC_10Km   + NPP03_100Km +  bldfie_05Km   + slope_25Km + sndppt_50Km + BAI1317_10Km + cropIIS_100Km +  OpCostIIS_100Km_SUM   + RuralPvty_100Km +  deltaGHSL9015_25Km + sa_25Km  + T1RoadDensity_50Km + IDH03_50Km , data = Global_Var))

step.model_Global_Var <- lm(RR_var ~ Time_mean + f2003_100Km + CEC_10Km   + NPP03_100Km +  bldfie_05Km   + slope_25Km + sndppt_50Km + BAI1317_10Km + cropIIS_100Km +  OpCostIIS_100Km_SUM   + RuralPvty_100Km +  deltaGHSL9015_25Km + sa_25Km  + T1RoadDensity_50Km + IDH03_50Km , data = Global_Var)

summary(step.model_Global_Var)
step.model_Results <- MASS::stepAIC(step.model_Global_Var, direction = "both", trace = F)

# LM Final 
Global_Var_Final <- lm(formula = RR_var ~ NPP03_100Km + bldfie_05Km + slope_25Km + 
                         sndppt_50Km + BAI1317_10Km + RuralPvty_100Km + sa_25Km, data = Global_Var)

summary(Global_Var_Final)

##### baseado em fatores que achamos relevantes
plot(Global_Var$RR_var ~ Global_Var$BAI1317_10Km)
plot(Global_Var$RR_var ~ Global_Var$NPP03_100Km)
plot(Global_Var$RR_var ~ Global_Var$bldfie_05Km)
plot(Global_Var$RR_var ~ Global_Var$sa_25Km)
plot(Global_Var$RR_var ~ Global_Var$slope_25Km)



#rm(list=ls())







############################################################
library(MuMIn)


Global_Var_VIF <- (mod = lm(RR_var ~ Time_mean + AnualTemp_25Km + CEC_10Km + DriestQuartes_25Km + f2003_100Km + NPP03_100Km + WDeficit99_100Km + bldfie_05Km + elevation_05Km + phihox_10Km + slope_25Km + sndppt_50Km + BAI1317_10Km + cropIIS_100Km + GrossDef_100Km +  OpCostIIS_100Km_SUM + PastAgric_10Km +  RuralPop_75Km + RuralPvty_100Km + deltaGHSL9015_25Km + sa_25Km, data = Global_Var))

m<-dredge(Global_Var_VIF)
