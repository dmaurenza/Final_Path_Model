#Indirect effects

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

Global_Var_VIF <- vif(mod = lm(RR_var ~ Time_mean + AnualTemp_05Km + CEC_05Km + DriestQuartes_25Km + ForestUntl10_25Km + NPP0010_50Km + WDeficit99_05Km + bldfie_75Km + elevation_05Km + phihox_25Km + slope_50Km + sndppt_100Km + BAI1317_10Km + cropIIS_05Km + GrossDef_100Km + IDH03_25Km + OpCostIIS_100Km_SUM + pastoIIS_10Km + PercUrbArea09_50Km + RuralPop_50Km + RuralPvty_05Km + T1RoadDensity_50Km + cdd_100Km + deltaGPWDensity0010_25Km + fty_25Km + sa_50Km + strictlyPA_100Km, data = Global_Var))


# Forest ----
#  PercUrbArea09_50Km + cropIIS_05Km + NPP0010_50Km + elevation_05Km +DriestQuartes_25Km + cdd_100Km + CEC_05Km +phihox_25Km +   IDH03_25Km +
Global_Var_VIF <- vif(mod = lm(ForestUntl10_25Km  ~ Time_mean +   WDeficit99_05Km +   BAI1317_10Km +  GrossDef_100Km + OpCostIIS_100Km_SUM + pastoIIS_10Km +  RuralPop_50Km + RuralPvty_05Km + T1RoadDensity_50Km + deltaGPWDensity0010_25Km +  strictlyPA_100Km  +  AnualTemp_05Km  + sa_50Km + sndppt_100Km +bldfie_75Km + slope_50Km  , data = Global_Var))

sort(Global_Var_VIF)

step.model_Global_Var <- lm(ForestUntl10_25Km  ~ Time_mean +   WDeficit99_05Km +   BAI1317_10Km +  GrossDef_100Km + OpCostIIS_100Km_SUM + pastoIIS_10Km +  RuralPop_50Km + RuralPvty_05Km + T1RoadDensity_50Km + deltaGPWDensity0010_25Km +  strictlyPA_100Km  +  AnualTemp_05Km  + sa_50Km + sndppt_100Km +bldfie_75Km + slope_50Km  , data = Global_Var)

summary(step.model_Global_Var)
step.model_Results <- MASS::stepAIC(step.model_Global_Var, direction = "both", trace = F)

# LM Final 
Global_Var_Final <- lm(formula = ForestUntl10_25Km ~ Time_mean + WDeficit99_05Km + BAI1317_10Km + GrossDef_100Km + OpCostIIS_100Km_SUM + RuralPvty_05Km + T1RoadDensity_50Km + strictlyPA_100Km + AnualTemp_05Km +  slope_50Km, data = Global_Var)

summary(Global_Var_Final)


#Global_Var_Variables <- c("Time_mean", "CEC_10Km", "DriestQuartes_25Km", "f2003_100Km", "WDeficit99_100Km", "elevation_05Km", "slope_25Km", "sndppt_50Km", "BAI1317_10Km", "cropIIS_100Km", "PastAgric_10Km", "RuralPop_75Km", "RuralPvty_100Km", "deltaGHSL9015_25Km", "sa_25Km")

#correlacao_Global_Var <- corrplot(cor(Global_Var[Global_Var_Variables]))


# Valores de Correlação ---

#cor.variable <- colnames(correlacao_Global_Var)
#correlacao_Global_Var <- correlacao_Global_Var %>% 
 # tbl_df() %>% 
  #mutate(variable = cor.variable)
#cor.results <- list()

#for(i in 1:length(cor.variable)){
 # results.i <- correlacao_Global_Var %>% 
  #  select(i, variable)
  #df.i <- data.frame(results.i)
  #cor.results[[i]] <- df.i[df.i[,1] > 0.6 | df.i[,1] < -0.6,]
#}
#names(cor.results) <- cor.variable



# CROP ----
# ForestUntl10_25Km + PercUrbArea09_50Km +Time_mean + AnualTemp_05Km + cdd_100Km +DriestQuartes_25Km +phihox_25Km +sa_50Km + IDH03_25Km + sndppt_100Km + slope_50Km +
Global_Var_VIF <- vif(mod = lm(cropIIS_05Km ~ CEC_05Km +  NPP0010_50Km + WDeficit99_05Km + bldfie_75Km + elevation_05Km +   BAI1317_10Km +  GrossDef_100Km + OpCostIIS_100Km_SUM + pastoIIS_10Km +  RuralPop_50Km + RuralPvty_05Km + T1RoadDensity_50Km +  deltaGPWDensity0010_25Km + fty_25Km + strictlyPA_100Km, data = Global_Var))

sort(Global_Var_VIF)

step.model_Global_Var <- lm(cropIIS_05Km ~ CEC_05Km +  NPP0010_50Km + WDeficit99_05Km + bldfie_75Km + elevation_05Km +   BAI1317_10Km +  GrossDef_100Km + OpCostIIS_100Km_SUM + pastoIIS_10Km +  RuralPop_50Km + RuralPvty_05Km + T1RoadDensity_50Km +  deltaGPWDensity0010_25Km + fty_25Km + strictlyPA_100Km, data = Global_Var)

step.model_Results <- MASS::stepAIC(step.model_Global_Var, direction = "both", trace = F)

# LM Final 
Global_Var_Final <-lm(formula = cropIIS_05Km ~ BAI1317_10Km + GrossDef_100Km + RuralPop_50Km +  deltaGPWDensity0010_25Km, data = Global_Var)

summary(Global_Var_Final)


# Urban area -----
# ForestUntl10_25Km +cropIIS_05Km +Time_mean + AnualTemp_05Km + CEC_05Km + DriestQuartes_25Km +  NPP0010_50Km + WDeficit99_05Km + bldfie_75Km + phihox_25Km + BAI1317_10Km + OpCostIIS_100Km_SUM +pastoIIS_10Km +RuralPop_50Km + RuralPvty_05Km + cdd_100Km + fty_25Km + sa_50Km + 

Global_Var_VIF <- vif(mod = lm(PercUrbArea09_50Km ~ elevation_05Km + slope_50Km + sndppt_100Km +  GrossDef_100Km + IDH03_25Km +   T1RoadDensity_50Km + deltaGPWDensity0010_25Km + strictlyPA_100Km, data = Global_Var))

sort(Global_Var_VIF)

step.model_Global_Var <- lm(PercUrbArea09_50Km ~ elevation_05Km + slope_50Km + sndppt_100Km +  GrossDef_100Km + IDH03_25Km +   T1RoadDensity_50Km + deltaGPWDensity0010_25Km + strictlyPA_100Km, data = Global_Var)

step.model_Results <- MASS::stepAIC(step.model_Global_Var, direction = "both", trace = F)

# LM Final 
Global_Var_Final<-lm(formula = PercUrbArea09_50Km ~ T1RoadDensity_50Km + deltaGPWDensity0010_25Km,  data = Global_Var)

summary(Global_Var_Final)


