install.packages("tidyverse")
install.packages("car")
install.packages("corrplot")
install.packages("MASS")
library(tidyverse)
library(car)
library(corrplot)

# Global
setwd(dir = "/Users/NILTON/Desktop/Daniel/")
Eco_Var <- read_csv("RData/Eco_Var.csv")
Socio_Var <- read_csv("RData/Socio_Var.csv")
Global_Var <- left_join(Eco_Var, Socio_Var, by = "Site") %>% 
  select(-RR_var.y) %>% 
  rename(RR_var = RR_var.x)
colnames(Global_Var)



# Sem HFPrint_25Km + elevation_100Km + phihox_25Km + + bldfie_75Km +  slope_100Km + IDH9010_25Km  + DriestQuartes_25Km + PercUrbArea09_50Km + RuralPvty_50Km + cdd_100Km  + RuralPop_100Km + ForestUntl10_100Km +
Global_Var_VIF <- vif(mod = lm(RR_var ~ Time_mean + AnualTemp_100Km + CEC_100Km  +  NPP0010_50Km + WDeficit8910_25Km + sndppt_05Km + BAI1317_25Km + Cropland_75Km + OpCostIISMean_10Km + PastAgric_50Km   + SustainablePA_05Km + T1RoadDensity_50Km  + deltaGPWCount0010_100Km + fty_100Km + sa_05Km + govrnce_25Km, data = Global_Var))



Global_Var_Variables <- c("Time_mean", "AnualTemp_100Km", "CEC_100Km", "govrnce_25Km", "NPP0010_50Km", "WDeficit8910_25Km", "sndppt_05Km", "BAI1317_25Km", "Cropland_75Km", "OpCostIISMean_10Km", "PastAgric_50Km", "SustainablePA_05Km", "T1RoadDensity_50Km", "deltaGPWCount0010_100Km", "fty_100Km", "sa_05Km")

correlacao_Global_Var <- corrplot(cor(Global_Var[Global_Var_Variables])) %>% as.data.frame()



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


# Stepwise ---
step.model_Global_Var <- lm(RR_var ~ Time_mean + AnualTemp_100Km + CEC_100Km  +  NPP0010_50Km + WDeficit8910_25Km + sndppt_05Km + BAI1317_25Km + Cropland_75Km + OpCostIISMean_10Km + PastAgric_50Km   + SustainablePA_05Km + T1RoadDensity_50Km  + deltaGPWCount0010_100Km + fty_100Km + sa_05Km + govrnce_25Km, data = Global_Var)

summary(step.model_Global_Var)
step.model_Results <- MASS::stepAIC(step.model_Global_Var, direction = "both", trace = F)

# LM Final 
Global_Var_Final <- lm(formula = RR_var ~ WDeficit8910_25Km + BAI1317_25Km + sa_05Km + 
                         govrnce_25Km, data = Global_Var)


summary(Global_Var_Final)
