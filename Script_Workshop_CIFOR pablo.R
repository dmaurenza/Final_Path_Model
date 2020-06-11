library(nlme)
library(lme4)
library(lavaan)
library(piecewiseSEM)
library(visreg)
library(ncf)
library(MuMIn)
library(rgdal)

setwd("C:/Users/pablo_000/Desktop/P?s-doc/CSRio")

##Load data
datafull<- read.csv("database.full.csv", h=T)
head(datafull)
colnames(datafull)
levels(datafull$Past_disturbance)

## Biodiversity data
data.bio<-subset(datafull, datafull$Mammals==1 | 
                   datafull$Birds==1 | 
                   datafull$Herpetofauna==1 | 
                   datafull$Plants==1 | 
                   datafull$Invertebrates==1)

levels(data.bio$Restoration_activity)
length(data.bio.D$ID)
## Tropical & subtropical landscapes restored through passive regeneration
data.bio.trop<-subset(data.bio, data.bio$Latitude<35 & 
                        data.bio$Latitude>-35 & 
                        data.bio$Restoration_activity=="passive")

## How many landscapes in the subset
length(unique(data.bio.trop$Site))


## Absolute values of Response Ratios
data.bio.trop$RRabs<-abs(data.bio.trop$RR)



## Data used from Crouzeilles et al. 2016 database 
colnames(data.bio.trop)

## Ecological & socioeconomic factors measured at five distinct buffer sizes
buffer<-read.table("buffer.csv", h=T, sep=",")
colnames(buffer)

length(buffer$Study)
  na.omit(buffer)


## Merge biodiversity & predictor variables in a single data frame
data<-merge(data.bio.trop, buffer, by="Study")

teste <- data %>% 
  arrange(Study)
View(head(teste, n = 50))








length(data.D$Study)

write.csv(data, file = "dadosCIFOR.csv")

length(data)
length(data$Site)

sort(data$RRabs)
View(head(data, n = 20))

teste <- drop_na(data)
teste <- na.omit(data)


teste <- data %>% 
  arrange(Study)
View(head(teste, n = 50))


## Remove NAs
data<-na.omit(data)

## Extreme values (Outlyers)
hist(data$RRabs, breaks=30)
tail(sort(data$RRabs), 50)
data<-subset(data, data$RRabs<4)

hist(data$RRabs, breaks=15)
frequencia <- as.data.frame(table(data$Site)) #conta quantos sites tem por estudo

#eliminar estudos com < Response ratios 
menor_que_5 <- which(frequencia$Freq <5) #quais estudos tem menos que 5 sites?
Site_menor_5 <- frequencia$Var1[menor_que_5] #nomes dos ID2 dos estudos que t?m menos que 5 sites
Site_menor_5 <- as.numeric(as.character(Site_menor_5)) #como numero, pra poder usar abaixo
linhas.excluir <- which(data$Site %in% Site_menor_5) # quais linhas representam os estudos (ID2) que possuem menos que 5 sites?
linhas.excluir
data <- data[-linhas.excluir,]
length(unique(data$Site))
frequencia<-as.data.frame(table(data$Site))
hist(frequencia$Freq, breaks=20)

data2<-data
####################-CEC-######################### 
data2$cec_100km<-scale(data2$cec_100km)
data2$cec_50km<-scale(data2$cec_50km)
data2$cec_25km<-scale(data2$cec_25km)
data2$cec_10km<-scale(data2$cec_10km)
data2$cec_5km<-scale(data2$cec_5km)

#################-Precipitation-#########################
## Mean precipitation of the driest quarter (mpdq)
data2$mpdq_100km<-scale(data2$mpdq_100km)
data2$mpdq_50km<-scale(data2$mpdq_50km)
data2$mpdq_25km<-scale(data2$mpdq_25km)
data2$mpdq_10km<-scale(data2$mpdq_10km)
data2$mpdq_5km<-scale(data2$mpdq_5km)

## Water deficit 2003 (wd.03)
data2$wd.03_100km<-scale(data2$wd.03_100km)
data2$wd.03_50km<-scale(data2$wd.03_50km)
data2$wd.03_25km<-scale(data2$wd.03_25km)
data2$wd.03_10km<-scale(data2$wd.03_10km)
data2$wd.03_5km<-scale(data2$wd.03_5km)

## Water deficit 1989-2010 (wd.8910)
data2$wd.8910_100km<-scale(data2$wd.8910_100km)
data2$wd.8910_50km<-scale(data2$wd.8910_50km)
data2$wd.8910_25km<-scale(data2$wd.8910_25km)
data2$wd.8910_10km<-scale(data2$wd.8910_10km)
data2$wd.8910_5km<-scale(data2$wd.8910_5km)

## Precipitation seasonality (prec.seas)
data2$prec.seas_100km<-scale(data2$prec.seas_100km)
data2$prec.seas_50km<-scale(data2$prec.seas_50km)
data2$prec.seas_25km<-scale(data2$prec.seas_25km)
data2$prec.seas_10km<-scale(data2$prec.seas_10km)
data2$prec.seas_5km<-scale(data2$prec.seas_5km)

## Annual precipitation (prec)
data2$prec_100km<-scale(data2$prec_100km)
data2$prec_50km<-scale(data2$prec_50km)
data2$prec_25km<-scale(data2$prec_25km)
data2$prec_10km<-scale(data2$prec_10km)
data2$prec_5km<-scale(data2$prec_5km)

## Net primary productivity 2000-2010 (npp.0010)
data2$npp.0010_100km<-scale(data2$npp.0010_100km)
data2$npp.0010_50km<-scale(data2$npp.0010_50km)
data2$npp.0010_25km<-scale(data2$npp.0010_25km)
data2$npp.0010_10km<-scale(data2$npp.0010_10km)
data2$npp.0010_5km<-scale(data2$npp.0010_5km)

## Net primary production 2003 (npp.03)
data2$npp.03_100km<-scale(data2$npp.03_100km)
data2$npp.03_50km<-scale(data2$npp.03_50km)
data2$npp.03_25km<-scale(data2$npp.03_25km)
data2$npp.03_10km<-scale(data2$npp.03_10km)
data2$npp.03_5km<-scale(data2$npp.03_5km)

## Tree cover (tree.cover)
data2$tree.cover_100km<-scale(data2$tree.cover_100km)
data2$tree.cover_50km<-scale(data2$tree.cover_50km)
data2$tree.cover_25km<-scale(data2$tree.cover_25km)
data2$tree.cover_10km<-scale(data2$tree.cover_10km)
data2$tree.cover_5km<-scale(data2$tree.cover_5km)

## Forest 2003 (forest.03)
data2$forest.03_100km<-scale(data2$forest.03_100km)
data2$forest.03_50km<-scale(data2$forest.03_50km)
data2$forest.03_25km<-scale(data2$forest.03_25km)
data2$forest.03_10km<-scale(data2$forest.03_10km)
data2$forest.03_5km<-scale(data2$forest.03_5km)

##Forest 2000-2010 (forest.0010)
data2$forest.0010_100km<-scale(data2$forest.0010_100km)
data2$forest.0010_50km<-scale(data2$forest.0010_50km)
data2$forest.0010_25km<-scale(data2$forest.0010_25km)
data2$forest.0010_10km<-scale(data2$forest.0010_10km)
data2$forest.0010_5km<-scale(data2$forest.0010_5km)

# Opportunity Cost (OppCost)
data2$OppCost_100km<-scale(data2$OppCost_100km)
data2$OppCost_50km<-scale(data2$OppCost_50km)
data2$OppCost_25km<-scale(data2$OppCost_25km)
data2$OppCost_10km<-scale(data2$OppCost_10km)
data2$OppCost_5km<-scale(data2$OppCost_5km)

## Rural poverty (RuralPvty)
data2$RuralPvty_100km<-scale(data2$RuralPvty_100km)
data2$RuralPvty_50km<-scale(data2$RuralPvty_50km)
data2$RuralPvty_25km<-scale(data2$RuralPvty_25km)
data2$RuralPvty_10km<-scale(data2$RuralPvty_10km)
data2$RuralPvty_5km<-scale(data2$RuralPvty_5km)

## Human Footprint (HFPrint)
data2$HFPrint_100km<-scale(data2$HFPrint_100km)
data2$HFPrint_50km<-scale(data2$HFPrint_50km)
data2$HFPrint_25km<-scale(data2$HFPrint_25km)
data2$HFPrint_10km<-scale(data2$HFPrint_10km)
data2$HFPrint_5km<-scale(data2$HFPrint_5km)

## Rural Population (RuralPop)
data2$RuralPop_100km<-scale(data2$RuralPop_100km)
data2$RuralPop_50km<-scale(data2$RuralPop_50km)
data2$RuralPop_25km<-scale(data2$RuralPop_25km)
data2$RuralPop_10km<-scale(data2$RuralPop_10km)
data2$RuralPop_5km<-scale(data2$RuralPop_5km)

## Urban area (UrbArea)
data2$UrbArea_100km<-scale(data2$UrbArea_100km)
data2$UrbArea_50km<-scale(data2$UrbArea_50km)
data2$UrbArea_25km<-scale(data2$UrbArea_25km)
data2$UrbArea_10km<-scale(data2$UrbArea_10km)
data2$UrbArea_5km<-scale(data2$UrbArea_5km)

## Road density (RoadDensity)
data2$RoadDensity_100km<-scale(data2$RoadDensity_100km)
data2$RoadDensity_50km<-scale(data2$RoadDensity_50km)
data2$RoadDensity_25km<-scale(data2$RoadDensity_25km)
data2$RoadDensity_10km<-scale(data2$RoadDensity_10km)
data2$RoadDensity_5km<-scale(data2$RoadDensity_5km)

## Specifying control values for lme fit
control = lmeControl(opt='optim', optimMethod = "BFGS", maxIter = 1000, msMaxIter = 1000, niterEM = 1000)

cec100<-lme(RRabs~cec_100km, random = ~cec_100km | Site, data=data2, control=control, method="ML")
cec50<-lme(RRabs~cec_50km, random = ~cec_50km | Site, data=data2, control=control, method="ML")
cec25<-lme(RRabs~cec_25km, random = ~cec_25km | Site, data=data2, control=control, method="ML")
cec10<-lme(RRabs~cec_10km, random = ~cec_10km | Site, data=data2, control=control, method="ML")
cec5<-lme(RRabs~cec_5km, random = ~cec_5km | Site, data=data2, control=control, method="ML")
model.sel(cec100,cec50,cec25,cec10,cec5) 
## cec_100km

mpdq100<-lme(RRabs~mpdq_100km, random = ~mpdq_100km | Site, data=data2, control=control, method="ML")
mpdq50<-lme(RRabs~mpdq_50km, random = ~mpdq_50km | Site, data=data2, control=control, method="ML")
mpdq25<-lme(RRabs~mpdq_25km, random = ~mpdq_25km | Site, data=data2, control=control, method="ML")
mpdq10<-lme(RRabs~mpdq_10km, random = ~mpdq_10km | Site, data=data2, control=control, method="ML")
mpdq5<-lme(RRabs~mpdq_5km, random = ~mpdq_5km | Site, data=data2, control=control, method="ML")
model.sel(mpdq100,mpdq50,mpdq25,mpdq10,mpdq5)
## mpdq_100km
  
wd.03.100<-lme(RRabs~wd.03_100km, random = ~wd.03_100km | Site, data=data2, control=control, method="ML")
wd.03.50<-lme(RRabs~wd.03_50km, random = ~wd.03_50km | Site, data=data2, control=control, method="ML")
wd.03.25<-lme(RRabs~wd.03_25km, random = ~wd.03_25km | Site, data=data2, control=control, method="ML")
wd.03.10<-lme(RRabs~wd.03_10km, random = ~wd.03_10km | Site, data=data2, control=control, method="ML")
wd.03.5<-lme(RRabs~wd.03_5km, random = ~wd.03_5km | Site, data=data2, control=control, method="ML")
model.sel(wd.03.100,wd.03.50,wd.03.25,wd.03.10,wd.03.5)
## wd.03_5km

wd.8910.100<-lme(RRabs~wd.8910_100km, random = ~wd.8910_100km | Site, data=data2, control=control, method="ML")
wd.8910.50<-lme(RRabs~wd.8910_50km, random = ~wd.8910_50km | Site, data=data2, control=control, method="ML")
wd.8910.25<-lme(RRabs~wd.8910_25km, random = ~wd.8910_25km | Site, data=data2, control=control, method="ML")
wd.8910.10<-lme(RRabs~wd.8910_10km, random = ~wd.8910_10km | Site, data=data2, control=control, method="ML")
wd.8910.5<-lme(RRabs~wd.8910_5km, random = ~wd.8910_5km | Site, data=data2, control=control, method="ML")
model.sel(wd.8910.100,wd.8910.50,wd.8910.25,wd.8910.10,wd.8910.5)
## wd.8910_5km

prec.s100<-lme(RRabs~prec.seas_100km, random = ~prec.seas_100km | Site, data=data2, control=control, method="ML")
prec.s50<-lme(RRabs~prec.seas_50km, random = ~prec.seas_50km | Site, data=data2, control=control, method="ML")
prec.s25<-lme(RRabs~prec.seas_25km, random = ~prec.seas_25km | Site, data=data2, control=control, method="ML")
prec.s10<-lme(RRabs~prec.seas_10km, random = ~prec.seas_10km | Site, data=data2, control=control, method="ML")
prec.s5<-lme(RRabs~prec.seas_5km, random = ~prec.seas_5km | Site, data=data2, control=control, method="ML")
model.sel(prec.s100,prec.s50,prec.s25,prec.s10,prec.s5)
## prec.seas_100km

prec100<-lme(RRabs~prec_100km, random = ~prec_100km | Site, data=data2, control=control, method="ML")
prec50<-lme(RRabs~prec_50km, random = ~prec_50km | Site, data=data2, control=control, method="ML")
prec25<-lme(RRabs~prec_25km, random = ~prec_25km | Site, data=data2, control=control, method="ML")
prec10<-lme(RRabs~prec_10km, random = ~prec_10km | Site, data=data2, control=control, method="ML")
prec5<-lme(RRabs~prec_5km, random = ~prec_5km | Site, data=data2, control=control, method="ML")
model.sel(prec100,prec50,prec25,prec10,prec5)
## = prec_100km

npp.03.100<-lme(RRabs~npp.03_100km, random = ~npp.03_100km | Site, data=data2, control=control, method="ML")
npp.03.50<-lme(RRabs~npp.03_50km, random = ~npp.03_50km | Site, data=data2, control=control, method="ML")
npp.03.25<-lme(RRabs~npp.03_25km, random = ~npp.03_25km | Site, data=data2, control=control, method="ML")
npp.03.10<-lme(RRabs~npp.03_10km, random = ~npp.03_10km | Site, data=data2, control=control, method="ML")
npp.03.5<-lme(RRabs~npp.03_5km, random = ~npp.03_5km | Site, data=data2, control=control, method="ML")
model.sel(npp.03.100,npp.03.50,npp.03.25,npp.03.10,npp.03.5)
## npp.03_50km

npp.0010.100<-lme(RRabs~npp.0010_100km, random = ~npp.0010_100km | Site, data=data2, control=control, method="ML")
npp.0010.50<-lme(RRabs~npp.0010_50km, random = ~npp.0010_50km | Site, data=data2, control=control, method="ML")
npp.0010.25<-lme(RRabs~npp.0010_25km, random = ~npp.0010_25km | Site, data=data2, control=control, method="ML")
npp.0010.10<-lme(RRabs~npp.0010_10km, random = ~npp.0010_10km | Site, data=data2, control=control, method="ML")
npp.0010.5<-lme(RRabs~npp.0010_5km, random = ~npp.0010_5km | Site, data=data2, control=control, method="ML")
model.sel(npp.0010.100,npp.0010.50,npp.0010.25,npp.0010.10,npp.0010.5)
## = npp.0010_100km

treecover.100<-lme(RRabs~tree.cover_100km, random = ~tree.cover_100km | Site, data=data2, control=control, method="ML")
treecover.50<-lme(RRabs~tree.cover_50km, random = ~tree.cover_50km | Site, data=data2, control=control, method="ML")
treecover.25<-lme(RRabs~tree.cover_25km, random = ~tree.cover_25km | Site, data=data2, control=control, method="ML")
treecover.10<-lme(RRabs~tree.cover_10km, random = ~tree.cover_10km | Site, data=data2, control=control, method="ML")
treecover.5<-lme(RRabs~tree.cover_5km, random = ~tree.cover_5km | Site, data=data2, control=control, method="ML")
model.sel(treecover.100,treecover.50,treecover.25,treecover.10,treecover.5)
## tree.cover_100km

forest03.100<-lme(RRabs~forest.03_100km, random = ~forest.03_100km | Site, data=data2, control=control, method="ML")
forest03.50<-lme(RRabs~forest.03_50km, random = ~forest.03_50km | Site, data=data2, control=control, method="ML")
forest03.25<-lme(RRabs~forest.03_25km, random = ~forest.03_25km | Site, data=data2, control=control, method="ML")
forest03.10<-lme(RRabs~forest.03_10km, random = ~forest.03_10km | Site, data=data2, control=control, method="ML")
forest03.5<-lme(RRabs~forest.03_5km, random = ~forest.03_5km | Site, data=data2, control=control, method="ML")
model.sel(forest03.100,forest03.50,forest03.25,forest03.10,forest03.5)
## forest.03_100km

forest0010.100<-lme(RRabs~forest.0010_100km, random = ~forest.0010_100km | Site, data=data2, control=control, method="ML")
forest0010.50<-lme(RRabs~forest.0010_50km, random = ~forest.0010_50km | Site, data=data2, control=control, method="ML")
forest0010.25<-lme(RRabs~forest.0010_25km, random = ~forest.0010_25km | Site, data=data2, control=control, method="ML")
forest0010.10<-lme(RRabs~forest.0010_10km, random = ~forest.0010_10km | Site, data=data2, control=control, method="ML")
forest0010.5<-lme(RRabs~forest.0010_5km, random = ~forest.0010_5km | Site, data=data2, control=control, method="ML")
model.sel(forest0010.100,forest0010.50,forest0010.25,forest0010.10,forest0010.5)
## forest.0010_100km

## Exploratory plots
par(mfrow=c(2,3))
# cec_100km
plot(data2$cec_100km, data2$RRabs, xlab="Cation exchange capacity 100km", ylab="RRabs")
cor.test(data2$RRabs, data2$cec_100km) # rela??o inversa
abline(lm(data2$RRabs~data2$cec_100km))
# mpdq_100km
plot(data2$mpdq_100km, data2$RRabs, xlab="Precipitation of the driest quarter 100km", ylab="RRabs")
cor.test(data2$RRabs, data2$mpdq_100km) # rela??o inversa
abline(lm(data2$RRabs~data2$mpdq_100km))
# wd.03_5km
plot(data2$wd.03_5km, data2$RRabs, xlab="Water deficit 2003 5km", ylab="RRabs")
cor.test(data2$RRabs, data2$wd.03_5km) # rela??o direta
abline(lm(data2$RRabs~data2$wd.03_5km))
# wd.8910_5km
plot(data2$wd.8910_5km, data2$RRabs, xlab="Water deficit 1989-2010 5km", ylab="RRabs")
cor.test(data2$RRabs, data2$wd.8910_5km) # rela??o inversa - SEM SENTIDO
abline(lm(data2$ RRabs~data2$wd.8910_5km))
# prec.seas_100 km
plot(data2$prec.seas_100km, data2$RRabs, xlab=" Precipitation seasonality 100km", ylab="RRabs")
cor.test(data2$RRabs, data2$prec.seas_100km) # rela??o direta                                                    
abline(lm(data2$RRabs~data2$prec.seas_100km))                                                                                                     
# prec_100km 
plot(data2$prec_100km, data2$RRabs, xlab="Precipitation 100km", ylab="RRabs")
cor.test(data2$RRabs, data2$prec_100km) # rela??o inversa
abline(lm(data2$RRabs~data2$prec_100km))
# npp.03_50km     
plot(data2$npp.03_50km, data2$RRabs, xlab="NPP 2003 50km", ylab="RRabs")                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       
cor.test(data2$RRabs, data2$npp.03_50km)  # rela??o inversa
abline(lm(data2$RRabs~data2$npp.03_50km))
# npp.0010_100km
plot(data2$npp.0010_100km, data2$RRabs, xlab="NPP 2000-2010 100km", ylab="RRabs")
cor.test(data2$RRabs, data2$npp.0010_100km) # rela??o direta - SEM SENTIDO
abline(lm(data2$RRabs~data2$npp.0010_100km))
# tree.cover_100km
plot(data2$tree.cover_100km, data2$RRabs, xlab="Tree cover 100km", ylab="RRabs")
cor.test(data2$RRabs, data2$tree.cover_100km)  # rela??o inversa
abline(lm(data2$RRabs~data2$tree.cover_100km))
# forest.03_100km
plot(data2$forest.03_100km, data2$RRabs, xlab="Forest cover 100km", ylab="RRabs")
cor.test(data2$RRabs, data2$forest.03_100km) # rela??o inversa
abline(lm(data2$RRabs~data2$forest.03_100km))
# forest.0010_100km
plot(data2$forest.0010_100km, data2$RRabs, xlab="Forest cover 2000-2010", ylab="RRabs")
cor.test(data2$RRabs, data2$forest.0010_100km) # rela??o inversa
abline(lm(data2$RRabs~data2$forest.0010_100km))


##### Socio-economic factors #####
oppcost.100<-lme(RRabs~OppCost_100km, random = ~OppCost_100km | Site, data=data2, control=control, method="ML")
oppcost.50<-lme(RRabs~OppCost_50km, random = ~OppCost_50km | Site, data=data2, control=control, method="ML")
oppcost.25<-lme(RRabs~OppCost_25km, random = ~OppCost_25km | Site, data=data2, control=control, method="ML")
oppcost.10<-lme(RRabs~OppCost_10km, random = ~OppCost_10km | Site, data=data2, control=control, method="ML")
oppcost.5<-lme(RRabs~OppCost_5km, random = ~OppCost_5km | Site, data=data2, control=control, method="ML")
model.sel(oppcost.100,oppcost.50,oppcost.25,oppcost.10,oppcost.5)
## OppCost_100km

ruralpvty.100<-lme(RRabs~RuralPvty_100km, random = ~RuralPvty_100km | Site, data=data2, control=control, method="ML")
ruralpvty.50<-lme(RRabs~RuralPvty_50km, random = ~RuralPvty_50km | Site, data=data2, control=control, method="ML")
ruralpvty.25<-lme(RRabs~RuralPvty_25km, random = ~RuralPvty_25km | Site, data=data2, control=control, method="ML")
ruralpvty.10<-lme(RRabs~RuralPvty_10km, random = ~RuralPvty_10km | Site, data=data2, control=control, method="ML")
ruralpvty.5<-lme(RRabs~RuralPvty_5km, random = ~RuralPvty_5km | Site, data=data2, control=control, method="ML")
model.sel(ruralpvty.100,ruralpvty.50,ruralpvty.25,ruralpvty.10,ruralpvty.5)
## RuralPvty_100km

model.sel(oppcost.100,oppcost.50,oppcost.25,oppcost.10,oppcost.5,ruralpvty.100,ruralpvty.50,ruralpvty.25,ruralpvty.10,ruralpvty.5)
## = RuralPvty_100km

hfprint.100<-lme(RRabs~HFPrint_100km, random = ~HFPrint_100km | Site, data=data2, control=control, method="ML")
hfprint.50<-lme(RRabs~HFPrint_50km, random = ~HFPrint_50km | Site, data=data2, control=control, method="ML")
hfprint.25<-lme(RRabs~HFPrint_25km, random = ~HFPrint_25km | Site, data=data2, control=control, method="ML")
hfprint.10<-lme(RRabs~HFPrint_10km, random = ~HFPrint_10km | Site, data=data2, control=control, method="ML")
hfprint.5<-lme(RRabs~HFPrint_5km, random = ~HFPrint_5km | Site, data=data2, control=control, method="ML")
model.sel(hfprint.100,hfprint.50,hfprint.25,hfprint.10,hfprint.5)
## = HFPrint_5km

ruralpop.100<-lme(RRabs~RuralPop_100km, random = ~RuralPop_100km | Site, data=data2, control=control, method="ML")
ruralpop.50<-lme(RRabs~RuralPop_50km, random = ~RuralPop_50km | Site, data=data2, control=control, method="ML")
ruralpop.25<-lme(RRabs~RuralPop_25km, random = ~RuralPop_25km | Site, data=data2, control=control, method="ML")
ruralpop.10<-lme(RRabs~RuralPop_10km, random = ~RuralPop_10km | Site, data=data2, control=control, method="ML")
ruralpop.5<-lme(RRabs~RuralPop_5km, random = ~RuralPop_5km | Site, data=data2, control=control, method="ML")
model.sel(ruralpop.100,ruralpop.50,ruralpop.25,ruralpop.10,ruralpop.5)
## = RuralPop_10km however RuralPop_100km had a similar performance (deltaAICc = 0 and 0.02, respectively)

urbarea.100<-lme(RRabs~UrbArea_100km, random = ~UrbArea_100km | Site, data=data2, control=control, method="ML")
urbarea.50<-lme(RRabs~UrbArea_50km, random = ~UrbArea_50km | Site, data=data2, control=control, method="ML")
urbarea.25<-lme(RRabs~UrbArea_25km, random = ~UrbArea_25km | Site, data=data2, control=control, method="ML")
urbarea.10<-lme(RRabs~UrbArea_10km, random = ~UrbArea_10km | Site, data=data2, control=control, method="ML")
urbarea.5<-lme(RRabs~UrbArea_5km, random = ~UrbArea_5km | Site, data=data2, control=control, method="ML")
model.sel(urbarea.100,urbarea.50,urbarea.25,urbarea.10,urbarea.5)
## UrbArea_100km

roads.100<-lme(RRabs~RoadDensity_100km, random = ~RoadDensity_100km | Site, data=data2, control=control, method="ML")
roads.50<-lme(RRabs~RoadDensity_50km, random = ~RoadDensity_50km | Site, data=data2, control=control, method="ML")
roads.25<-lme(RRabs~RoadDensity_25km, random = ~RoadDensity_25km | Site, data=data2, control=control, method="ML")
roads.10<-lme(RRabs~RoadDensity_10km, random = ~RoadDensity_10km | Site, data=data2, control=control, method="ML")
roads.5<-lme(RRabs~RoadDensity_5km, random = ~RoadDensity_5km | Site, data=data2, control=control, method="ML")
model.sel(roads.100,roads.50,roads.25,roads.10,roads.5)
## RoadDensity_100km

model.sel(urbarea.100,urbarea.50,urbarea.25,urbarea.10,urbarea.5,roads.100,roads.50,roads.25,roads.10,roads.5)
## = UrbArea_100km

## Exploratory plots
# OppCost_100km
par(mfrow=c(2,3))
plot(data2$OppCost_100km, data2$RRabs, xlab="Opportunity cost 100km", ylab="RRabs")
cor.test(data2$RRabs, data2$OppCost_100km) # rela??o inversa (?)
abline(lm(data2$RRabs~data2$OppCost_100km))
# RuralPvty_100km
plot(data2$RuralPvty_100km, data2$RRabs, xlab="Rural poverty 100km", ylab="RRabs")
cor.test(data2$RRabs, data2$RuralPvty_100km) # rela??o inversa
abline(lm(data2$RRabs~data2$RuralPvty_100km))
# HFPrint_5km
plot(data2$HFPrint_5km, data2$RRabs, xlab="Human footprint 5km", ylab="RRabs")
cor.test(data2$RRabs, data2$HFPrint_5km) # rela??o direta
abline(lm(data2$RRabs~data2$HFPrint_5km))
# RuralPop_10km
plot(data2$RuralPop_10km, data2$RRabs, xlab="Rural population 10km")
cor.test(data2$RRabs, data2$RuralPop_10km) # Rela??o inversa - SEM SENTIDO
abline(lm(data2$RRabs~data2$RuralPop_10km))
# UrbArea_100km
plot(data2$UrbArea_100km, data2$RRabs, xlab="Urban Area 100km", ylab="RRabs")
cor.test(data2$RRabs, data2$UrbArea_100km) # rela??o direta
abline(lm(data2$RRabs~data2$UrbArea_100km))
# RoadDensity_100km
plot(data2$RoadDensity_100km, data2$RRabs, ylab="RRabs", xlab="Road Density 100km")
cor.test(data2$RRabs, data2$RoadDensity_100km) # rela??o direta
abline(lm(data2$RRabs~data2$RoadDensity_100km))

## Defining the best autocorrelation structure
colnames(data2)
RRabs<-data2$RRabs
UrbArea_100km<-data2$UrbArea_100km
Site<-data2$Site

urb <- lme(RRabs ~ UrbArea_100km , random = ~ UrbArea_100km  | Site, control=control, method="ML")
urb_lin <- lme(RRabs ~ UrbArea_100km , random = ~ UrbArea_100km  | Site, control=control, correlation = corLin(form = ~ x+y|Site), method="ML")
urb_exp <- lme(RRabs ~ UrbArea_100km , random = ~ UrbArea_100km  | Site, control=control, correlation = corExp(form = ~ x+y|Site), method="ML")
urb_gaus <- lme(RRabs ~ UrbArea_100km , random = ~ UrbArea_100km  | Site, control=control, correlation = corGaus(form = ~ x+y|Site), method="ML")
urb_ratio <- lme(RRabs ~ UrbArea_100km , random = ~ UrbArea_100km  | Site, control=control, correlation = corRatio(form = ~ x+y|Site), method="ML")
urb_spher <- lme(RRabs ~ UrbArea_100km , random = ~ UrbArea_100km  | Site, control=control, correlation = corSpher(form = ~ x+y|Site), method="ML")

model.sel(m2000_riq, m2000_lin_riq, m2000_exp_riq, m2000_gaus_riq, m2000_ratio_riq, m2000_spher_riq)

################## PATH ANALYSIS ########################

## modelo final ecol?gico

m.eco.final = list(
  mod.RR<-lme(RRabs ~ cec_100km + prec.seas_100km + npp.03_50km + tree.cover_100km + disturbance + time, random = ~cec_100km + prec.seas_100km + npp.03_50km + tree.cover_100km + disturbance + time | Site, data=data2, control=control, method="ML")
  mod.cover<-lme(tree.cover_100km ~ prec.seas_100km + npp.03_50km + disturbance , random= ~prec.seas_100km + npp.03_50km + disturbance | Site, control=control, method="ML")
  mod.npp<-lme(npp.03_50km ~ cec_100km + prec.seas_100km, random = ~ cec_100km + prec.seas_100km | Site, control=control, method="ML")
  )

m.eco.final.result<-sem.fit(m.eco.final, data2, model.control = list(control))

# Explore individual model fits
sem.model.fits(m.eco.final)

m2000_final = list(
  Riqueza <- lme(Riqueza_spp_dependente2 ~ Esforco.Padronizado + PLAND_2km + ED_2km , random = ~ Esforco.Padronizado + PLAND_2km + ED_2km | ID2, data = data_sesma, control=control, correlation = corGaus(form = ~ x+y|ID2), method="ML"),
  NP <- lme(ED_2km ~ PLAND_2km, random = ~ PLAND_2km | ID2, data = data_sesma, control=control, correlation = corExp(form = ~ x+y|ID2), method="ML")
)


m2000_final_result <- sem.fit(m2000_final, data_sesma, model.control = list(control))


# Explore individual model fits
sem.model.fits(m2000_final)



final.eco.1<-lme(RRabs~cec_100km+prec.seas_100km+npp.03_50km+tree.cover_100km, 
                 random=~cec_100km+prec.seas_100km+npp.03_50km+tree.cover_100km | Site, 
                 data=data2, control=control, method="ML")
final.eco.2<-lme(RRabs~cec_100km+prec.seas_100km+npp.03_50km, 
                 random=~cec_100km+prec.seas_100km+npp.03_50km | Site, 
                 data=data2, control=control, method="ML")
final.eco.3<-lme(RRabs~cec_100km+prec.seas_100km+tree.cover_100km, 
                 random=~cec_100km+prec.seas_100km+tree.cover_100km | Site, 
                 data=data2, control=control, method="ML")
final.eco.4<-lme(RRabs~cec_100km+npp.03_50km+tree.cover_100km, 
                 random=~cec_100km+npp.03_50km+tree.cover_100km | Site, 
                 data=data2, control=control, method="ML")
final.eco.5<-lme(RRabs~prec.seas_100km+npp.03_50km+tree.cover_100km, 
                 random=~prec.seas_100km+npp.03_50km+tree.cover_100km | Site, 
                 data=data2, control=control, method="ML")
final.eco.6<-lme(RRabs~cec_100km+prec.seas_100km, 
                 random=~cec_100km+prec.seas_100km | Site, 
                 data=data2, control=control, method="ML")
final.eco.7<-lme(RRabs~cec_100km+npp.03_50km, 
                 random=~cec_100km+npp.03_50km | Site, 
                 data=data2, control=control, method="ML")
final.eco.8<-lme(RRabs~cec_100km+tree.cover_100km, 
                 random=~cec_100km+tree.cover_100km | Site, 
                 data=data2, control=control, method="ML")
final.eco.9<-lme(RRabs~prec.seas_100km+npp.03_50km, 
                 random=~prec.seas_100km+npp.03_50km | Site, 
                 data=data2, control=control, method="ML")
final.eco.10<-lme(RRabs~prec.seas_100km+tree.cover_100km, 
                 random=~prec.seas_100km+tree.cover_100km | Site, 
                 data=data2, control=control, method="ML")
final.eco.11<-lme(RRabs~npp.03_50km+tree.cover_100km, 
                 random=~npp.03_50km+tree.cover_100km | Site, 
                 data=data2, control=control, method="ML")

model.sel(cec100, prec.s100,npp.03.50,treecover.100,final.eco.1,
          final.eco.2,final.eco.3,final.eco.4,final.eco.5,final.eco.6,
          final.eco.7,final.eco.8,final.eco.9,final.eco.10,final.eco.11)
## Modelos com deltaAICc inferior a 2
#                                   random df    logLik   AICc delta weight
##final.eco.11             n.03_5+t.cv_1|S 10 -1661.526 3343.3  0.00  0.455
##npp.03.50                       n.03_5|S  6 -1665.956 3344.0  0.74  0.315
##final.eco.9               p.s_1+n.03_5|S 10 -1662.433 3345.1  1.81  0.184




Final.soc.1<-lme(RRabs~RuralPvty_100km+HFPrint_5km+RuralPop_10km+UrbArea_50km, 
                 random=~RuralPvty_100km+HFPrint_5km+RuralPop_10km+UrbArea_50km | Site, 
                 data=data2, control=control, method="ML")
Final.soc.2<-lme(RRabs~RuralPvty_100km+HFPrint_5km+RuralPop_10km, 
                 random=~RuralPvty_100km+HFPrint_5km+RuralPop_10km | Site,
                 data=data2, control=control, method="ML")
Final.soc.3<-lme(RRabs~RuralPvty_100km+HFPrint_5km+UrbArea_50km, 
                 random=~RuralPvty_100km+HFPrint_5km+UrbArea_50km | Site, 
                 data=data2, control=control, method="ML")
Final.soc.4<-lme(RRabs~RuralPvty_100km+RuralPop_10km+UrbArea_50km, 
                 random=~RuralPvty_100km+RuralPop_10km+UrbArea_50km | Site, 
                 data=data2, control=control, method="ML")
Final.soc.5<-lme(RRabs~HFPrint_5km+RuralPop_10km+UrbArea_50km, 
                 random=~HFPrint_5km+RuralPop_10km+UrbArea_50km | Site, 
                 data=data2, control=control, method="ML")
Final.soc.6<-lme(RRabs~RuralPvty_100km+HFPrint_5km, 
                 random=~RuralPvty_100km+HFPrint_5km | Site,
                 data=data2, control=control, method="ML")
Final.soc.7<-lme(RRabs~RuralPvty_100km+RuralPop_10km, 
                 random=~RuralPvty_100km+RuralPop_10km | Site,
                 data=data2, control=control, method="ML")
Final.soc.8<-lme(RRabs~RuralPvty_100km+UrbArea_50km, 
                 random=~RuralPvty_100km+UrbArea_50km | Site, 
                 data=data2, control=control, method="ML")
Final.soc.9<-lme(RRabs~HFPrint_5km+RuralPop_10km, 
                 random=~RuralPvty_100km+HFPrint_5km+RuralPop_10km | Site,
                 data=data2, control=control, method="ML")
Final.soc.10<-lme(RRabs~HFPrint_5km+UrbArea_50km, 
                 random=~HFPrint_5km+UrbArea_50km | Site, 
                 data=data2, control=control, method="ML")
Final.soc.11<-lme(RRabs~RuralPop_10km+UrbArea_50km, 
                 random=~RuralPop_10km+UrbArea_50km | Site, 
                 data=data2, control=control, method="ML")

model.sel(Final.soc.1,Final.soc.2,Final.soc.3,Final.soc.4,Final.soc.5,
          Final.soc.6,Final.soc.7,Final.soc.8,Final.soc.9,Final.soc.10,
          Final.soc.11,ruralpvty.100,hfprint.5,ruralpop.10,urbarea.50)

#                                       random df    logLik   AICc delta weight
#Final.soc.8                  RrlPv_100+U_50|S 10 -1664.019 3348.2  0.00  0.791


## Defining the best autocorrelation structure

m2000_riq <- lme(Riqueza_spp_dependente2 ~ Esforco.Padronizado + PLAND_2km + ED_2km , random = ~ Esforco.Padronizado + PLAND_2km + ED_2km  | ID2, data = data_sesma, control=control, method="ML")
m2000_lin_riq <- lme(Riqueza_spp_dependente2 ~ Esforco.Padronizado + PLAND_2km + ED_2km , random = ~ Esforco.Padronizado + PLAND_2km + ED_2km  | ID2, data = data_sesma, control=control, correlation = corLin(form = ~ x+y|ID2), method="ML")
m2000_exp_riq <- lme(Riqueza_spp_dependente2 ~ Esforco.Padronizado + PLAND_2km + ED_2km , random = ~ Esforco.Padronizado + PLAND_2km + ED_2km  | ID2, data = data_sesma, control=control, correlation = corExp(form = ~ x+y|ID2), method="ML")
m2000_gaus_riq <- lme(Riqueza_spp_dependente2 ~ Esforco.Padronizado + PLAND_2km + ED_2km , random = ~ Esforco.Padronizado + PLAND_2km + ED_2km   | ID2, data = data_sesma, control=control, correlation = corGaus(form = ~ x+y|ID2), method="ML")
m2000_ratio_riq <- lme(Riqueza_spp_dependente2 ~ Esforco.Padronizado + PLAND_2km + ED_2km , random = ~ Esforco.Padronizado + PLAND_2km + ED_2km   | ID2, data = data_sesma, control=control, correlation = corRatio(form = ~ x+y|ID2), method="ML")
m2000_spher_riq <- lme(Riqueza_spp_dependente2 ~ Esforco.Padronizado + PLAND_2km + ED_2km , random = ~ Esforco.Padronizado + PLAND_2km + ED_2km   | ID2, data = data_sesma, control=control, correlation = corSpher(form = ~ x+y|ID2), method="ML")

model.sel(m2000_riq, m2000_lin_riq, m2000_exp_riq, m2000_gaus_riq, m2000_ratio_riq, m2000_spher_riq)

################## PATH ANALYSIS ########################

## modelo final

m2000_final = list(
  Riqueza <- lme(Riqueza_spp_dependente2 ~ Esforco.Padronizado + PLAND_2km + ED_2km , random = ~ Esforco.Padronizado + PLAND_2km + ED_2km | ID2, data = data_sesma, control=control, correlation = corGaus(form = ~ x+y|ID2), method="ML"),
  NP <- lme(ED_2km ~ PLAND_2km, random = ~ PLAND_2km | ID2, data = data_sesma, control=control, correlation = corExp(form = ~ x+y|ID2), method="ML")
)


m2000_final_result <- sem.fit(m2000_final, data_sesma, model.control = list(control))


# Explore individual model fits
sem.model.fits(m2000_final)
