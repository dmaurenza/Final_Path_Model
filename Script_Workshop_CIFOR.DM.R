library(nlme)
library(lme4)
library(lavaan)
library(piecewiseSEM)
library(visreg)
library(ncf)
library(MuMIn)
library(rgdal)
library(tidyverse)

##Load data
datafull.D <- read_csv("database.full.csv")
head(datafull.D)
## Biodiversity data
data.bio.D <- datafull.D %>% 
  filter(Mammals == 1| Birds == 1| Herpetofauna == 1| Plants == 1| Invertebrates == 1)

unique(data.bio.D$Restoration_activity)
length(data.bio.D$ID)
## Tropical & subtropical landscapes restored through passive regeneration
data.bio.trop.D <- data.bio.D %>% 
  filter(Latitude < 35 & Latitude >-35 & Restoration_activity == "passive")

## How many landscapes in the subset
distinct(.data = data.bio.trop.D, Site)

## Absolute values of Response Ratios
data.bio.trop.D <- data.bio.trop.D %>% 
  mutate(RRabs = abs(RR))

## Data used from Crouzeilles et al. 2016 database 
data.bio.trop.D<-data.bio.trop.D[,c(2,3,10,16:21,32)]
# Ecological & socioeconomic factors measured at five distinct buf --------

buffer <- read_csv("buffer.csv")
colnames(buffer)

length(buffer$Study)

## Merge biodiversity & predictor variables in a single data frame
data.D <- data.bio.trop.D %>% 
  inner_join(buffer, by = "Study")

# Selection by conditions - Final dataset
data.D <- data.D %>% 
  drop_na() %>% 
  filter(RRabs<4) %>% 
  group_by(Site) %>% 
  filter(n()>=5)



# Scale function ----------------------------------------------------------
# List of variable and transformation of data using scale function

list.variable <- colnames(data.D[11:235])
for(i in 1:length(list.variable)){
  data.D[,list.variable[i]] <- scale(data.D[,list.variable[i]])
}

# LME function ------------------------------------------------------------
## Specifying control values for lme fit
control = lmeControl(opt='optim', optimMethod = "BFGS", maxIter = 1000, msMaxIter = 1000, niterEM = 1000)

## Running loop function to estimate lme for each variable/buffer

df.variable <- as.data.frame(data.D[11:235])
lme.variable <- list()

for(i in 1:length(df.variable)){
  variable <- df.variable[,i]
  lme.variable[[i]] <- lme(RRabs ~ variable, random = ~ variable | Site, data=data.D, control=control, method="ML")
}

names(lme.variable) <- list.variable



# Model Selection for each variable ---------------------------------------
# gethering the variables and buffers
CEC <- lme.variable[1:5]
MPDQ <- lme.variable[6:10]
NPP.0010 <- lme.variable[11:15] # juntar com NPP.03 
NPP.03 <- lme.variable[16:20]
PREC.SEAS <- lme.variable[21:25]
WD.03 <- lme.variable[26:30] # juntar WD
WD.8910 <- lme.variable[31:35]
PREC <- lme.variable[36:40]
TREE.COVER <- lme.variable[41:45] # juntar covers
FOREST.03 <- lme.variable[46:50]
FOREST.0010 <- lme.variable[51:55]
OPP_COST <- lme.variable[56:60] # JUNTAR COM OP_COST_IIS_MASKED
HFPRINT <- lme.variable[61:65]
RURAL_POP <- lme.variable[66:70]
ROAD_DENSITY <- lme.variable[71:75]
URB_AREA <- lme.variable[76:80]
CROP <- lme.variable[81:85] # CROP_IIS
IDH03 <- lme.variable[86:90] # IDH JUNTAR IDH03 E IDH9010
IDH9010 <- lme.variable[91:95]
PEST_AGRIC <- lme.variable[96:100] # JUNTAR COM CROP_PASTURE
ELEVATION <- lme.variable[101:105]
ELEVATION_SD <- lme.variable[106:110]
SLOPE <- lme.variable[111:115]
TEMP <- lme.variable[116:120]
BAI <- lme.variable[121:125] # JUNTAR COM NBR
GROSS_DEF <- lme.variable[126:130]
NBR <- lme.variable[131:135]
OP_COST_IIS_MASKED <- lme.variable[136:140]
SUSTEIN_PA <- lme.variable[141:145] # testar se junta as 2 categorias de AP. Somar as dias variaveis antes de "scale". 3 variaveis para AP. 2 separadas + soma
BULK <- lme.variable[146:150]
COMODD <- lme.variable[151:155]
CROP_IIS <- lme.variable[156:160]
CROP_PASTURE_IIS <- lme.variable[161:165]
FORESTRYDD <- lme.variable[166:170]
PASTO_IIS <- lme.variable[171:175]
PH <- lme.variable[176:180]
POP.COUNT <- lme.variable[181:185]# + POP_DENSITY <- lme.variable[186:190] + POP_COUNT <- lme.variable[191:195]
POP_DENSITY <- lme.variable[186:190]
POP_COUNT <- lme.variable[191:195]
SADD <- lme.variable[196:200]
SAND <- lme.variable[201:205]
STRICT_PA <- lme.variable[206:210]
URBDD <- lme.variable[211:215]
WFIREDD <- lme.variable[216:220]
GOVCE <- lme.variable[221:225]

#Selecting models

model.sel(CEC) # cec_50km
model.sel(MPDQ) # mpdq_5km
model.sel(c(NPP.0010, NPP.03)) # npp.03_5km
model.sel(PREC.SEAS) # prec.seas_100km
model.sel(c(WD.03, WD.8910)) # wd.03_5km
model.sel(PREC) # prec_5km
model.sel(c(TREE.COVER, FOREST.03, FOREST.0010)) # forest.03_10km
model.sel(c(OPP_COST, OP_COST_IIS_MASKED)) # OpCostIISmasked_50km
model.sel(HFPRINT) # HFPrint_10km
model.sel(RURAL_POP) # Rural.Pop_5km
model.sel(ROAD_DENSITY) # RoadDensity_10km
model.sel(URB_AREA) # UrbArea_10km
model.sel(c(CROP, CROP_IIS)) # cropIIS_10km
model.sel(c(IDH03, IDH9010)) # IDH03_50km
model.sel(c(PEST_AGRIC, CROP_PASTURE)) # crop.pastureISS_50km
model.sel(ELEVATION) # Elevation_5km
model.sel(ELEVATION_SD) # ElevationSD_50km
model.sel(SLOPE) # Slope_5km
model.sel(TEMP) # temp_100km
model.sel(c(NBR, BAI)) # BAI.1317_100km
model.sel(GROSS_DEF) # GrossDef_100km
model.sel(SUSTEIN_PA) #SustaisPA_25km
model.sel(STRICT_PA) # strictPA_100km
model.sel(c(SUSTEIN_PA, STRICT_PA)) # strictPA_1000km
model.sel(BULK) #bulk_50km
model.sel(COMODD) # commodd_50km
model.sel(FORESTRYDD) # forestrydd_10km
model.sel(SADD) # sadd_50km
model.sel(URBDD) # urbdd_50km
model.sel(WFIREDD) # wfiredd_100km
model.sel(c(COMODD, FORESTRYDD, SADD, URBDD, WFIREDD)) # sadd_50km
model.sel(PH) #  pH_100km
model.sel(POP.COUNT) # pop.count_100km
model.sel(POP_DENSITY) # pop.demsity_100km
model.sel(POP_COUNT) # pop_count_100km
model.sel(c(POP.COUNT, POP_DENSITY, POP_COUNT)) # pop_count_10km
model.sel(SAND) # sand_100km
model.sel(GOVCE) # govce_100km


# Path Analysis -----------------------------------------------------------
teste <- lme(RRabs ~ forest.03_10km + strictPA_100km + IDH03_50km + govce_100km + RoadDensity_10km + UrbArea_10km + RuralPop_5km + Slope_5km + crop.pastureISS_50km + wfiredd_100km + OpCostISSmasked_50km + commodd_50km + sand_100km + cec_50km + mpdq_5km + prec_5km + prec.seas_100km, random = ~forest.03_10km + strictPA_100km + IDH03_50km + govce_100km + RoadDensity_10km + UrbArea_10km + RuralPop_5km + Slope_5km + crop.pastureISS_50km + wfiredd_100km + OpCostISSmasked_50km + commodd_50km + sand_100km + cec_50km + mpdq_5km + prec_5km + prec.seas_100km | Site, data = data.D, control = control, method = "ML")



