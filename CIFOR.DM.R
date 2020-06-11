install.packages("tidyverse", dependencies = T)
install.packages("piecewiseSEM", dependencies = T)
install.packages("MuMIn", dependencies = T)
install.packages("corrplot", dependencies = T)
library(tidyverse)
library(nlme)
library(MuMIn)
library(piecewiseSEM)
library(corrplot)


## Literatura
# Para interpretar os dados, ler Shipley, pg 115.
# https://jonlefcheck.net/2014/07/06/piecewise-structural-equation-modeling-in-ecological-research/
# http://www.bodowinter.com/tutorial/bw_LME_tutorial2.pdf

# Load data ---------------------------------------------------------------
datafull.D <- read_csv("database.full.csv")
head(datafull.D)
## Biodiversity data
data.bio.D <- datafull.D %>% 
  filter(Mammals == 1| Birds == 1| Herpetofauna == 1| Plants == 1| Invertebrates == 1) %>% 
  mutate(Study = as.character(Study),
    Site = as.character(Site))


unique(data.bio.D$Restoration_activity)
length(data.bio.D$ID)
## Tropical & subtropical landscapes restored through passive regeneration
data.bio.trop.D <- data.bio.D %>% 
  filter(Latitude < 35 & Latitude >-35 & Restoration_activity == "passive")

## How many landscapes in the subset
distinct(.data = data.bio.trop.D, Site)

## Variance values between sites
data.bio.trop.D <- data.bio.trop.D %>% 
  group_by(Site) %>% 
  mutate(RRvar = var(RR))
## Absolute values of Response Ratios
data.bio.trop.D <- data.bio.trop.D %>% 
  mutate(RRabs = abs(RR))

## Data used from Crouzeilles et al. 2016 database 

colnames(data.bio.trop.D)
data.bio.trop.D<-data.bio.trop.D[,c(2,3,10,16:21,31:33)] 
# Ecological & socioeconomic factors measured at five distinct buf --------

buffer <- read_csv("buffer.csv") %>% 
  mutate(Study = as.character(Study))
colnames(buffer)
buffer$wfiredd_5km
length(buffer$Study)
names(buffer) 

# Adding Protected Area (strict + sustain
buffer <- buffer %>% 
  mutate(PA_5km = SustainPA_5km + strictPA_5km, PA_10km = SustainPA_10km + strictPA_10km, PA_25km = SustainPA_25km + strictPA_25km, PA_50km = SustainPA_50km + strictPA_50km, PA_100km = SustainPA_100km + strictPA_100km )


## Merge biodiversity & predictor variables in a single data frame
data <- data.bio.trop.D %>% 
  inner_join(buffer, by = "Study") 
colnames(data)  
# data.D %>% 
#   select(Study, Site, Plants, Birds,cec_5km, prec_5km,  RR, RRabs, RRvar)
# Selection by conditions - Final dataset

data <- data %>% 
  drop_na() %>% 
  group_by(Site) %>% 
  filter(n()>=5) %>% 
  ungroup() %>% 
  distinct(Site, .keep_all = T)
colnames(data)

# Scale function ----------------------------------------------------------
# List of variable and transformation of data using scale function
data[,10:242] <- scale(data[,10:242])
  

# list.variable <- colnames(data[c(13:242, 4)])
# for(i in 1:length(list.variable)){
#   data[,list.variable[i]] <- scale(data[,list.variable[i]])
# }

# LME function for all variable and buffers------------------------------------------------------------
## Specifying control values for lme fit
control = lmeControl(opt='optim', optimMethod = "BFGS", maxIter = 1000, msMaxIter = 1000, niterEM = 1000)

# LME function for RRabs
x <- lm(RRvar ~ list_variable[1], data = df)
colnames(data)




list_variable <- colnames(data[,13:242])
lm.variable <- as.data.frame(data[c(13:242)])
listagem <- list()
for(i in 1:length(list.variable)){
  #i = 1
  variable <- lm.variable[,i]
  listagem[[i]] <- lm(RRvar ~ variable, data = data)
}

data %>% 
  select(1:15) %>% 
  view()


df.variable <- as.data.frame(data[c(13:240, 4)])
lme.variable <- list()
length(df.variable)
for(i in 1:length(df.variable)){
  variable <- df.variable[,i]
  lme.variable[[i]] <- lme(RRabs ~ variable, data=data, control=control, method="ML")
}

names(lme.variable) <- colnames(df.variable)

# Combination of variables and Buffer selection ---------------------------------------
# separating each variable and combination to select the better variable and  buffer size
# Ecological variable
CEC <- model.sel(lme.variable[1:5]) # cec_50km 
MPDQ <- model.sel(lme.variable[6:10]) # mpdq_5km
NPP.0010 <- model.sel(lme.variable[11:15]) # npp.0010_5km
NPP.03 <- model.sel(lme.variable[16:20]) # npp.03_5km
PREC.SEAS <- model.sel(lme.variable[21:25]) #prec.seas_100km
WD.03 <- model.sel(lme.variable[26:30]) # wd.03_5km 
WD.8910 <- model.sel(lme.variable[31:35]) # wd.8910_5km
PREC <- model.sel(lme.variable[36:40]) # prec_5km 
TREE.COVER <- model.sel(lme.variable[41:45]) # tree.cover_50km
FOREST.03 <- model.sel(lme.variable[46:50]) # forest.03_10km
FOREST.0010 <- model.sel(lme.variable[51:55]) # forest.0010_10km
ELEVATION <- model.sel(lme.variable[101:105]) # Elevation_5km
ELEVATION_SD <- model.sel(lme.variable[106:110]) # ElevationSD_50km
SLOPE <- model.sel(lme.variable[111:115]) # Slope_5km
TEMP <- model.sel(lme.variable[116:120]) # temp_100km
BULK <- model.sel(lme.variable[146:150]) # bulk_50km
PH <- model.sel(lme.variable[176:180]) # pH_100km
SAND <- model.sel(lme.variable[201:205]) # sand_100km

# Eliminating Ecological variable by model selection
NPP <- model.sel(lme.variable[11:20]) # npp.03_5km
WD <- model.sel(lme.variable[26:35]) # wd.03_5km  
COVER <- model.sel(lme.variable[41:55]) # forest.03_10km

# Socioeconomic variable
OPP_COST <- model.sel(lme.variable[56:60]) # OppCost_100km 
OP_COST_IIS_MASKED <- model.sel(lme.variable[136:140]) #OpCostISSmasked_50km
HFPRINT <- model.sel(lme.variable[61:65]) # HFPrint_10km
RURAL_POP <- model.sel(lme.variable[66:70]) # RuralPop_5km
ROAD_DENSITY <- model.sel(lme.variable[71:75]) # RoadDensity_10km
URB_AREA <- model.sel(lme.variable[76:80]) # UrbArea_10km
CROP <- model.sel(lme.variable[81:85]) # Crop_100km
CROP_IIS <- model.sel(lme.variable[156:160]) # cropIIS_10km
IDH03 <- model.sel(lme.variable[86:90]) # IDH03_50km
IDH9010 <- model.sel(lme.variable[91:95]) # IDH9010_50km
PEST_AGRIC <- model.sel(lme.variable[96:100]) # PastAgric_25km
CROP_PASTURE_IIS <- model.sel(lme.variable[161:165]) # crop.pastureISS_50km 
BAI <- model.sel(lme.variable[121:125]) # BAI.1317_100km 
NBR <- model.sel(lme.variable[131:135]) # NBR.1317_5km
GROSS_DEF <- model.sel(lme.variable[126:130]) # GrossDef_100km
SUSTEIN_PA <- model.sel(lme.variable[141:145]) # SustainPA_25km
STRICT_PA <- model.sel(lme.variable[206:210]) # strictPA_100km
PA <- model.sel(lme.variable[226:230]) # PA_25km # Sum of Sustain and strict PA
PASTO_IIS <- model.sel(lme.variable[171:175]) # pastoIIS_100km
POP.COUNT <- model.sel(lme.variable[181:185]) # pop.count_100km 
POP_DENSITY <- model.sel(lme.variable[186:190]) # pop.density_100km
POP_COUNT <- model.sel(lme.variable[191:195]) # pop_count_100km
COMODD <- model.sel(lme.variable[151:155]) # commodd_50km 
FORESTRYDD <- model.sel(lme.variable[166:170]) # forestrydd_10km
SADD <- model.sel(lme.variable[196:200]) # sadd_50km 
URBDD <- model.sel(lme.variable[211:215]) # urbdd_50km 
WFIREDD <- model.sel(lme.variable[216:220]) # wfiredd_100km
GOVCE <- model.sel(lme.variable[221:225]) # govce_100km

# Eliminating Socioeconomic variable by model selection
OppCost.OppCost_IIS <- model.sel(c(lme.variable[56:60], lme.variable[136:140])) # OpCostISSmasked_50km 
CROP<- model.sel(c(lme.variable[81:85], lme.variable[156:160]) ) # cropIIS_10km 
CropPasture <- model.sel(c(lme.variable[96:100], lme.variable[161:165])) # crop.pastureISS_50km 
IDH <- model.sel(lme.variable[86:95]) # IDH03_50km
Population <- model.sel(lme.variable[181:195]) # pop_count_100km
Fire <- model.sel(c(lme.variable[121:125], lme.variable[131:135])) # BAI.1317_100km 
PA <- model.sel(c(lme.variable[141:145], lme.variable[206:210], lme.variable[216:220])) # strictPA_100km
Agropecuaria <- model.sel(lme.variable[c(196:200, 81:85, 156:160, 161:165, 96:100, 171:175)]) #cropIIS_10km - Combinação de CropIIS, CropPastureIIS, PastoIIS, PastAgric, Crop, SADD
IDH.GOV <- model.sel(lme.variable[c(221:225, 86:95)]) # IDH03_50km, combinação de IDH03, IDH9010, Governance
DD <- model.sel(c(lme.variable[166:170], lme.variable[151:155], lme.variable[196:200],lme.variable[211:220])) # sadd_50km 
DD.Gross <- model.sel(c(lme.variable[166:170], lme.variable[151:155], lme.variable[196:200],lme.variable[211:220], lme.variable[126:130])) #sadd_50km 
IIS <- model.sel(lme.variable[c(156:160, 161:165, 171:175)]) # cropIIS_10km

# Final Variable selected by model.sel function, considering the combination of variables
Ecological <- c("cec_50km", "mpdq_5km", "npp.03_5km", "prec.seas_100km", "wd.03_5km", "prec_5km", "forest.03_10km", "Elevation_5km", "ElevationSD_50km", "Slope_5km", "temp_100km", "bulk_50km", "pH_100km", "sand_100km")

Socioeconomic <- c("OpCostISSmasked_50km", "HFPrint_10km", "RuralPop_5km", "RoadDensity_10km", "UrbArea_10km", "cropIIS_10km", "crop.pastureISS_50km", "IDH03_50km", "BAI.1317_100km", "NBR.1317_5km", "strictPA_100km", "pop_count_100km", "sadd_50km", "govce_100km" )

# Correlacao --------------------------------------------------------------
# Com base nas variaveis selecionadas (combinações por model.sel) e tamanho de buffer. Correlação entre as variáveis
# Plot das correlações
correlacao_Eco<- corrplot(cor(data.D[c("cec_50km", "mpdq_5km", "npp.03_5km", "prec.seas_100km", "wd.03_5km", "prec_5km", "forest.03_10km", "Elevation_5km", "ElevationSD_50km", "Slope_5km", "temp_100km", "bulk_50km", "pH_100km", "sand_100km")]))

correlacao_Socio <- corrplot(cor(data.D[c("OpCostISSmasked_50km", "HFPrint_10km", "RuralPop_5km", "RoadDensity_10km", "UrbArea_10km", "cropIIS_10km", "crop.pastureISS_50km", "IDH03_50km", "BAI.1317_100km", "NBR.1317_5km", "strictPA_100km", "pop_count_100km", "sadd_50km", "govce_100km")]))

# Selecting variables correlated between - 0.6 and 0.6
# Ecological
cor.variable <- colnames(correlacao_Eco)
correlacao_Eco <- correlacao_Eco %>%  
  tbl_df() %>% 
  mutate(variable = cor.variable)
cor.variable <- colnames(correlacao_Eco)

write_csv(correlacao_Eco, "CorEco_Table.csv")

cor.results <- list()
for(i in 1:length(cor.variable)){
  result.i <- correlacao_Eco %>%
    select(i, variable)
  df.i <- data.frame(result.i)
  cor.results[[i]] <- df.i[df.i[,1] > 0.6 | df.i[,1] < -0.6,]
}
names(cor.results) <-  cor.variable

# Decidimos eliminar as variáveis MPDK, Elevation, Elevation SD e PH. 
# Permaneceram as variáveis cec_50km, prec_5km, npp.03_5km, wd.03_5km, prec.seas_100km, forest.03_10km, Slope_5km, temp_100km, sand_100km, bulk_50km


# Socioeconomic
cor.variable <- colnames(correlacao_Socio)
correlacao_Socio <- correlacao_Socio %>%  
  tbl_df() %>% 
  mutate(variable = cor.variable)
cor.variable <- colnames(correlacao_Socio)

write_csv(correlacao_Socio, "CorSocio_Table.csv")

cor.results <- list()
for(i in 1:length(cor.variable)){
  result.i <- correlacao_Socio %>%
    select(i, variable)
  df.i <- data.frame(result.i)
  cor.results[[i]] <- df.i[df.i[,1] > 0.6 | df.i[,1] < -0.6,]
}
names(cor.results) <-  cor.variable

# Decidimos eliminar as variáveis HFPrint, crop.pastureISS_50km, IDH03_50km, NBR.1317_5km e "govce_100km".
# Permaneceram as variáveis OpCostISSmasked_50km, RuralPop_5km, RoadDensity_10km, UrbArea_10km, cropIIS_10km, BAI.1317_100km, strictPA_100km, pop_count_100km, sadd_50km

# Modelagem dos efeitos diretos (Multiple Linear Mixed Model) ------------------------------------------------------------------

eco.model <- lme(RRabs ~ cec_50km + prec_5km + npp.03_5km + wd.03_5km  + prec.seas_100km + forest.03_10km + Slope_5km + temp_100km +  sand_100km + bulk_50km, random = ~ cec_50km + prec_5km + npp.03_5km + wd.03_5km  + prec.seas_100km + forest.03_10km + Slope_5km + temp_100km + sand_100km + bulk_50km | Site, data = data.D, control = control, method = "ML")

socio.model <- lme(RRabs ~ OpCostISSmasked_50km +  RuralPop_5km + RoadDensity_10km + UrbArea_10km +  cropIIS_10km  + BAI.1317_100km + strictPA_100km + pop_count_100km + sadd_50km, random = ~ OpCostISSmasked_50km +  RuralPop_5km + RoadDensity_10km + UrbArea_10km +  cropIIS_10km  + BAI.1317_100km + strictPA_100km + pop_count_100km + sadd_50km | Site, data = data.D, control = control, method = "ML") 


summary(socio.model)

# Linear mixed-effects model fit by maximum likelihood
# Data: data.D 
# AIC      BIC    logLik
# 1544.568 1765.282 -727.2839
# 
# Random effects:
#   Formula: ~OpCostISSmasked_50km + RuralPop_5km + RoadDensity_10km + UrbArea_10km +      cropIIS_10km + strictPA_100km + IDH03_50km | Site
# Structure: General positive-definite, Log-Cholesky parametrization
# StdDev     Corr                                            
# (Intercept)          0.02069545 (Intr) OCISS_ RrlP_5 RdD_10 UrA_10 cIIS_1 sPA_10
# OpCostISSmasked_50km 0.06331018 -0.620                                          
# RuralPop_5km         0.20172354 -0.622  0.996                                   
# RoadDensity_10km     0.15142621 -0.625  0.997  0.999                            
# UrbArea_10km         0.03670944  0.829 -0.903 -0.911 -0.914                     
# cropIIS_10km         0.29204604  0.617 -0.996 -1.000 -1.000  0.909              
# strictPA_100km       0.18265067 -0.566  0.995  0.996  0.995 -0.880 -0.996       
# IDH03_50km           0.03194752 -0.369  0.934  0.942  0.940 -0.764 -0.943  0.955
# Residual             0.48323847                                                 
# 
# Fixed effects: RRabs ~ OpCostISSmasked_50km + RuralPop_5km + RoadDensity_10km +      UrbArea_10km + cropIIS_10km + strictPA_100km + IDH03_50km 
# Value  Std.Error  DF   t-value p-value
# (Intercept)           0.4130732 0.02431498 929 16.988427  0.0000
# OpCostISSmasked_50km -0.0100565 0.02661438 929 -0.377861  0.7056
# RuralPop_5km         -0.0079907 0.04466027 929 -0.178922  0.8580
# RoadDensity_10km      0.0569108 0.03897209 929  1.460297  0.1445
# UrbArea_10km          0.0654316 0.03503769 929  1.867464  0.0622
# cropIIS_10km         -0.0861599 0.05920062 929 -1.455388  0.1459
# strictPA_100km        0.0631148 0.04048186 929  1.559089  0.1193
# IDH03_50km           -0.0617073 0.03061550 929 -2.015557  0.0441
# Correlation: 
#   (Intr) OCISS_ RrlP_5 RdD_10 UrA_10 cIIS_1 sPA_10
# OpCostISSmasked_50km -0.043                                          
# RuralPop_5km         -0.049  0.150                                   
# RoadDensity_10km      0.091  0.038  0.332                            
# UrbArea_10km          0.188 -0.029 -0.151 -0.387                     
# cropIIS_10km          0.130 -0.359 -0.809 -0.478  0.184              
# strictPA_100km        0.055  0.253  0.609  0.447 -0.162 -0.641       
# IDH03_50km            0.085 -0.300  0.253  0.209 -0.333 -0.004  0.134
# 
# Standardized Within-Group Residuals:
#   Min         Q1        Med         Q3        Max 
# -2.9199446 -0.5247525 -0.2260366  0.2701909  5.7987826 
# 
# Number of Observations: 997
# Number of Groups: 61 

# Path-Model --------------------------------------------------------------

# Unificado com efeito direto de Migration e Urban Area - Fiz alguma coisa antes que esta dando resultados diferentes de C Fisher e P
# Refazer a análise usando os dados do github. Provavelmente eu fiz modificações e salvei como Data.D em cima do anterior. A versão antiga esta no github.
unificado6 <- psem(
  direct <- lme(RRabs ~ UrbArea_10km + pop_count_100km, random = ~ UrbArea_10km + pop_count_100km | Site, data = data.D, control = control, method = "ML"),
  ind.urbarea <- lme(UrbArea_10km ~ RoadDensity_100km + IDH03_5km , random = ~ RoadDensity_100km + IDH03_5km | Site, data = data.D, control = control, method = "ML"),
  ind.migration <- lme(pop_count_100km ~ IDH03_5km  + RoadDensity_100km + UrbArea_10km, random = ~ IDH03_5km  + RoadDensity_100km + UrbArea_10km | Site, data = data.D, control = control, method = "ML")
)
summary (unificado6)


# Resultados dos caminhos  -------------------------------------------------------------

# 1 - Road Density > Delta Pop > RR = -0.0472112
# 2 - Road Density > Urban Area > RR = -0.0408213
# 3 - Road Density > Urban Area > Delta Pop > RR = 0.005091149
# 4 - HDI > Delta Pop > RR = 0.02813824
# 5 - HDI > Urban Area > RR = 0.05814666
# 6 - HDI > Urban Area > Delta Pop > RR = -0.007251932
# 
# 0.7765 * -0.0608
# -0.5815 * 0.0702
# -0.5815 * 0.1440 * -0.0608
# (-0.4628) * (-0.0608)
# 0.8283 * 0.0702
# 0.8283 * 0.1440 * (-0.0608)
# 0.0702 + (-0.0608) + (-0.0472112) + (-0.0408213) + 0.005091149 + 0.02813824 +  0.05814666 + (-0.007251932)
# 
# 
# 7 - Road Density > RR = -0.08294135
# (-0.0472112) + (-0.0408213) + 0.005091149
# 
# 8 - HDI > RR = 0.07903297
# 0.02813824 + 0.05814666 + (-0.007251932)
# 
# 9 - Efeito direto (urban Area + Delta Pop > RR) = 0.0094
# 0.0702 + (-0.0608)
# 
# 10 - Efeito Indireto (Road Density + Delta Pop) = -0.003908383
# (-0.0472112) + (-0.0408213) + 0.005091149 + 0.02813824 +  0.05814666 + (-0.007251932)
# 
# 11 - Efeito Total (soma de efeitos diretos e indiretos) = 0.005491617
# 

# Interpretação dos dados -------------------------------------------------
# Deve-se calcular os DPs de todas as variáveis antes da transformação em z - função scale

DP_UrbArea_10km <- sd(data.D$UrbArea_10km) %>% print() # 0.9893051
DP_pop_count_100km <- sd(data.D$pop_count_100km) %>% print() # 2.075397
DP_IDH03_5km <- sd(data.D$IDH03_5km) %>% print() # 0.1445916
DP_RoadDensity_100km <- sd(data.D$RoadDensity_100km) %>% print() # 446.2219
DP_RRabs <- sd(data.D$RRabs) %>% print() # 0.5617746


#This means that if we were to increase the value of x1
# by one standard deviation while holding the value for x2 constant at its mean
# value, the value of y1 would increase by 0.64 times its standard deviation. As
# we can see, there is only one directed path connecting x1 with y1. However,
# the case for the total effect of x1 on y2 is different
# Quando se aumenta uma unidade do desvio padrão (antes da função scale) do eixo X, corresponde ao aumento de uma unidade do desvio padrão do eixo Y * pelo valor Estimate
# Interpretação de 1 exemplo (Urb_Area) - quando aumenta a urb area em 1 DP, vc aumenta a RRabs em 0.0702 DP. Ou seja, quando aumenta urban area em 0.98 (que eh o DP da UA), vc aumenta o RRabs em 0.0702*0.056 = 0.0039312 (sendo 0.056 o DP do RRabs)
# Artigo para estudar a interpretação desses dados <- https://www.researchgate.net/publication/229892514_Structural_Equation_Modeling_for_Observational_Studies
# Referencia que explica a distinção entre rsquare marginal e condicional - O que diferencia é a consideração da variável aleatória (Site) <- # https://jonlefcheck.net/2013/03/13/r2-for-linear-mixed-effects-models/ - 
# Referencia para esplicação do Fisher's C - Shipley 2013 - Ecology - https://esajournals.onlinelibrary.wiley.com/doi/10.1890/12-0976.1

# Duvida que permanece - É possível calcular o caminho indireto de variáveis que tem setas chegando? Por exemplo, Urban Area > Population > RRabs. Faz-se necessário fazer o teste. Reescrever o modelo sem incluir as setas que chegam em Urban Area e verificar se os valores de Estimate mudificam.

# A apresentação dos dados esta correta (estimate (Erro Padrão) e P)

# Não existe Total de Caminhos indiretos (total de Road Density + HDI), Apenas o total indireto separado. Resultado 10 não existe.

# Artigo para estudar os caminhos indiretos > https://onlinelibrary.wiley.com/doi/abs/10.1111/oik.01948

# Outras propostas de modelo unificado ------------------------------------
#Unificado desconsiderando o Migration, ou seja, efeito direto apenas com Urban Area

unificado2 <- psem(
  direct <- lme(RRabs ~ UrbArea_10km, random = ~ UrbArea_10km | Site, data = data.D, control = control, method = "ML"),
  ind.urbarea <- lme(UrbArea_10km ~ RoadDensity_100km + IDH03_5km , random = ~ RoadDensity_100km + IDH03_5km  | Site, data = data.D, control = control, method = "ML")
)
summary (unificado2)



results <- summary(unificado)

# Unificado com efeito direto de Migration e Urban Area e sem efeito indireto de IDH
unificado3 <- psem(
  direct <- lme(RRabs ~ UrbArea_10km + pop_count_100km, random = ~ UrbArea_10km + pop_count_100km | Site, data = data.D, control = control, method = "ML"),
  ind.urbarea <- lme(UrbArea_10km ~ RoadDensity_100km, random = ~ RoadDensity_100km | Site, data = data.D, control = control, method = "ML"),
  ind.migration <- lme(pop_count_100km ~ RoadDensity_100km + UrbArea_10km, random = ~ RoadDensity_100km + UrbArea_10km | Site, data = data.D, control = control, method = "ML")
)
summary (unificado3)


# Unificado com efeito direto de Migration e Urban Area e sem efeito indireto de IDH em Migration
unificado4 <- psem(
  direct <- lme(RRabs ~ UrbArea_10km + pop_count_100km, random = ~ UrbArea_10km + pop_count_100km | Site, data = data.D, control = control, method = "ML"),
  ind.urbarea <- lme(UrbArea_10km ~ RoadDensity_100km + IDH03_5km , random = ~ RoadDensity_100km + IDH03_5km  | Site, data = data.D, control = control, method = "ML"),
  ind.migration <- lme(pop_count_100km ~ RoadDensity_100km + UrbArea_10km, random = ~ RoadDensity_100km + UrbArea_10km | Site, data = data.D, control = control, method = "ML")
)
summary (unificado4)

# Unificado com efeito direto de Migration e Urban Area e sem efeito indireto de IDH em Area Urbana
unificado5 <- psem(
  direct <- lme(RRabs ~ UrbArea_10km + pop_count_100km, random = ~ UrbArea_10km + pop_count_100km | Site, data = data.D, control = control, method = "ML"),
  ind.urbarea <- lme(UrbArea_10km ~ RoadDensity_100km , random = ~ RoadDensity_100km | Site, data = data.D, control = control, method = "ML"),
  ind.migration <- lme(pop_count_100km ~ IDH03_5km  + RoadDensity_100km + UrbArea_10km, random = ~ IDH03_5km  + RoadDensity_100km + UrbArea_10km | Site, data = data.D, control = control, method = "ML")
)
summary (unificado5)






# rm(list=ls())
