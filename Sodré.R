# Load data ---------------------------------------------------------------
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


length(buffer$Study)
names(buffer)
## Merge biodiversity & predictor variables in a single data frame
data.D <- data.bio.trop.D %>% 
  inner_join(buffer, by = "Study")

# Selection by conditions - Final dataset
data.D <- data.D %>% 
  mutate(PA_5km = SustainPA_5km + strictPA_5km, PA_10km = SustainPA_10km + strictPA_10km, PA_25km = SustainPA_25km + strictPA_25km, PA_50km = SustainPA_50km + strictPA_50km, PA_100km = SustainPA_100km + strictPA_100km ) %>% 
  drop_na() %>% 
  filter(RRabs<4) %>% 
  group_by(Site) %>% 
  filter(n()>=5)
colnames(data.D)


summary(data.D$pop_count_100km)
# Scale function ----------------------------------------------------------
# List of variable and transformation of data using scale function

list.variable <- colnames(data.D[c(11:240, 4)])
for(i in 1:length(list.variable)){
  data.D[,list.variable[i]] <- scale(data.D[,list.variable[i]])
}

class(data.D)


# Path Model ------------------------------------------------------------------

eco.model
eco.model <- lme(RRabs ~ cec_50km + prec_5km + npp.03_5km + wd.03_5km  + prec.seas_100km + forest.03_10km + Slope_5km + temp_100km +  sand_100km + bulk_50km, random = ~ cec_50km + prec_5km + npp.03_5km + wd.03_5km  + prec.seas_100km + forest.03_10km + Slope_5km + temp_100km + sand_100km + bulk_50km | Site, data = data.D, control = control, method = "ML") # Excluir mpdq e PH, Elevation


socio.model <- lme(RRabs ~ OpCostISSmasked_50km +  RuralPop_5km + RoadDensity_10km + UrbArea_10km +  cropIIS_10km  + BAI.1317_100km + strictPA_100km + pop_count_100km + IDH03_50km, random = ~ OpCostISSmasked_50km + RuralPop_5km + RoadDensity_10km + UrbArea_10km +  cropIIS_10km + BAI.1317_100km + strictPA_100km + pop_count_100km + IDH03_50km | Site, data = data.D, control = control, method = "ML") # excluir Governance


# Final Unificado

unificado <- psem(
  direct <- lme(RRabs ~ UrbArea_10km + pop_count_100km, random = ~ UrbArea_10km + pop_count_100km | Site, data = data.D, control = control, method = "ML"),
  ind.urbarea <- lme(UrbArea_10km ~ RoadDensity_100km + IDH03_5km , random = ~ RoadDensity_100km + IDH03_5km  | Site, data = data.D, control = control, method = "ML"),
  ind.migration <- lme(pop_count_100km ~ IDH03_5km  + RoadDensity_100km + UrbArea_10km, random = ~ IDH03_5km  + RoadDensity_100km + UrbArea_10km | Site, data = data.D, control = control, method = "ML")
)
summary (unificado)


results <- summary(unificado)


# Exploratory Plots

RRABS <- hist(data.D$RRabs, breaks = 15, main = paste("Response Ratio"), xlab = "RRabs" )
URB_AREA <- hist(data.D$UrbArea_10km, breaks = 15, main = paste("Urban Area"), xlab = "UrbArea_10km" )
POP_COUNT <- hist(data.D$pop_count_100km, breaks = 15, main = paste("Migration"), xlab = "pop_count_100km" )
ROAD_DENSITY <- hist(data.D$RoadDensity_100km, breaks = 15, main = paste("Road Density"), xlab = "RoadDensity_100km" )
IDH <- hist(data.D$IDH03_5km, breaks = 15, main = paste("HDI"), xlab = "IDH03_5km")

RRABS <- hist(data.D$RRabs, breaks = 15, main = paste("Response Ratio Scaled"), xlab = "RRabs" )
URB_AREA <- hist(data.D$UrbArea_10km, breaks = 15, main = paste("Urban Area Scaled"), xlab = "UrbArea_10km" )
POP_COUNT <- hist(data.D$pop_count_100km, breaks = 15, main = paste("Migration Scaled"), xlab = "pop_count_100km" )
ROAD_DENSITY <- hist(data.D$RoadDensity_100km, breaks = 15, main = paste("Road Density Scaled"), xlab = "RoadDensity_100km" )
IDH <- hist(data.D$IDH03_5km, breaks = 15, main = paste("HDI Scaled"), xlab = "IDH03_5km")

modelo <- lmer(RRabs ~ pop_count_100km + (pop_count_100km | Site), data.D)
modelo <- lmer(RRabs ~ UrbArea_10km + (UrbArea_10km | Site), data.D)
modelo <- lmer(UrbArea_10km ~ RoadDensity_100km + (RoadDensity_100km | Site), data.D)
modelo <- lmer(UrbArea_10km ~ IDH03_5km + (IDH03_5km | Site), data.D)
modelo <- lmer(pop_count_100km ~ IDH03_5km + (IDH03_5km | Site), data.D)
modelo <- lmer(pop_count_100km ~ UrbArea_10km + (UrbArea_10km | Site), data.D)
modelo <- lmer(pop_count_100km ~ RoadDensity_100km + (RoadDensity_100km | Site), data.D)
modelo <- lmer(RRabs ~ pop.density_100km + (pop.density_100km | Site), data.D)

summary(modelo)
plot(modelo)

plot(RRabs ~ pop.density_100km, data.D)
abline(a = 0.44236, b = -0.06897)

summary(modelo)
par(mfrow=c(5,2))
par(mar=c(3,4.5,2,3),cex.axis=1.2,cex.lab=1.2,cex.main=1.5,family="serif",las=1,tcl=0.3,mgp=c(2,0.3,0)) 
plot(riqueza~area, xlab="Área (ha)", ylab="Riqueza de espécies de\n roedores e marsupiais", cex=1.2, pch=16, bty="u")
abline(data.D$RRabs ~ data.D$UrbArea_10km)
abline(h=mean(riqueza))

dev.off()


