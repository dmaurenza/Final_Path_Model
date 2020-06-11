# Testes

# Variance ---- + phihox_25Km slope_100Km +
Eco_Var_VIF.PH <- vif(mod = lm(RR_var ~ Time_mean + phihox_25Km +  AnualTemp_100Km + CEC_100Km + DriestQuartes_25Km + ForestUntl10_100Km + NPP0010_50Km + WDeficit8910_25Km + bldfie_75Km + elevation_100Km + slope_100Km + sndppt_05Km, data = Eco_Var))




Eco_Var_VIF.PH <- vif(mod = lm(RR_var ~ Time_mean + AnualTemp_100Km + CEC_100Km + DriestQuartes_25Km + ForestUntl10_100Km + NPP0010_50Km + WDeficit8910_25Km + bldfie_75Km + elevation_100Km + slope_100Km + sndppt_05Km, data = Eco_Var))



Eco_Var_VIF.slope <- vif(mod = lm(RR_var ~ Time_mean + AnualTemp_100Km + CEC_100Km + DriestQuartes_25Km + ForestUntl10_100Km + NPP0010_50Km + WDeficit8910_25Km + bldfie_75Km + elevation_100Km +  sndppt_05Km, data = Eco_Var))


Eco_Var_VIF.elevation <- vif(mod = lm(RR_var ~ Time_mean + AnualTemp_100Km + CEC_100Km + DriestQuartes_25Km + ForestUntl10_100Km + NPP0010_50Km + WDeficit8910_25Km + slope_100Km + bldfie_75Km + sndppt_05Km, data = Eco_Var))

Eco_Var_VIF.Anual <- vif(mod = lm(RR_var ~ Time_mean + CEC_100Km + DriestQuartes_25Km + ForestUntl10_100Km + NPP0010_50Km + WDeficit8910_25Km + slope_100Km + elevation_100Km + bldfie_75Km + sndppt_05Km, data = Eco_Var))


#########################
Eco_Var_Variables <- c("Time_mean", "AnualTemp_100Km", "phihox_25Km","CEC_100Km","DriestQuartes_25Km", "ForestUntl10_100Km", "NPP0010_50Km", "WDeficit8910_25Km", "bldfie_75Km", "elevation_100Km", "slope_100Km", "sndppt_05Km")


correlacao_Eco_Var <- corrplot(cor(Eco_Var[Eco_Var_Variables]))


# Stepwise ---
step.model_Eco_Var <- lm(RR_var ~ Time_mean + AnualTemp_100Km + CEC_100Km + DriestQuartes_25Km + ForestUntl10_100Km + NPP0010_50Km + WDeficit8910_25Km + bldfie_75Km + elevation_100Km + phihox_25Km +  sndppt_05Km, data = Eco_Var)

step.model_Results <- stepAIC(step.model_Eco_Var, direction = "both", trace = F)