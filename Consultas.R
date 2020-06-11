library(tidyverse)
# Consultas
datafull_no_scale <- read_csv("RData/datafull_no_scale.csv")
datafull_no_scale$PercUrbArea09_50Km
colnames(datafull_no_scale)
range_min <- datafull_no_scale %>%  # datafull_no_scale Created fom Pre_processing 
  summarise_at(vars(c("ForestUntl10_25Km", "cropIIS_05Km", "PercUrbArea09_50Km")), min, na.rm = T)

range_max <- datafull_no_scale %>%  # datafull_no_scale Created fom Pre_processing 
  summarise_at(vars(c("ForestUntl10_25Km", "cropIIS_05Km", "PercUrbArea09_50Km")), max, na.rm = T)

range_mean <- datafull_no_scale %>%  # datafull_no_scale Created fom Pre_processing 
  summarise_at(vars(c("ForestUntl10_25Km", "cropIIS_05Km", "PercUrbArea09_50Km")), mean, na.rm = T)


range <- rbind(range_min, range_max, range_mean)

Time_range <- datafull_no_scale %>% 
  count(Time_restored)

Time_mean <- datafull_no_scale %>% 
  summarise(mean(Time_restored),sd(Time_restored),sd(Study_year),  median(Time_restored))

# Desvios Padrões
Mean <- datafull_no_scale %>% 
  summarise_at(vars(Study_year:wfire_50Km), mean, na.rm = T)
SD <- datafull_no_scale %>% 
  summarise_at(vars(Study_year:wfire_50Km), sd, na.rm = T)
Mean_SD <- rbind(Mean, SD)
Mean_SD %>% 
  select(ForestUntl10_25Km, ForestUntl10_05Km,cropIIS_05Km, PercUrbArea09_50Km)

colnames(datafull)

# Coordinates ----
long_lat <- datafull_no_scale %>% 
  select(Site, Study, Longitude, Latitude)
write_csv(long_lat, "RData/long_lat.csv")

# Categorias
Past_Disturbance <- unique(datafull$Past_disturbance)
Disturbance_type <- unique(datafull$Disturbance_type)
Forest_Conversion <- unique(datafull$Forest_conversion)
Land_use <- unique(datafull$Land_use)
Time_Disturbance <- unique(datafull$Time_disturbed)

# Forest Cover consult

datafull_no_scale %>% 
  summarise(mean = mean(ForestUntl10_25Km), sd = sd(ForestUntl10_25Km))

# RR nuumbers in percentages by taxon group

unique(data$Ecological_metric)
data %>% 
  count(Ecological_metric)
(64*100)/350
46+36+18

datafull_no_scale %>% 
  filter(ForestUntl10_05Km > 20 & ForestUntl10_05Km < 30) %>% 
  select(ForestUntl10_05Km)

datafull_no_scale %>% 
  filter(ForestUntl10_25Km > 20 & ForestUntl10_25Km < 30) %>% 
  select(ForestUntl10_25Km)

(3*100)/32

# # Dependence variables
# Global_Var$Site <- as.character(Global_Var$Site)
# Global_Var %>% 
#   select(Site, RR_var)
# 
# total <- datafull %>% #after RR calculation
#   select(Site, Ecological_metric, Taxon, RR)
# colnames(datafull)
# Reference <- datafull %>% #after line 19
#   select(Study, Site, Reference)
# Reference$Site <- as.character(Reference$Site)
# Reference <- Reference %>% 
#   distinct(Site, .keep_all = T)
# dependente <- total %>% 
#   left_join(y = Reference, by = "Site")
# #number <- dependente %>% 
# #  count(Site, Ecological_metric, Taxon)
# #dependente <- number %>% 
#   left_join(y = Reference, by = "Site")
# write_csv(dependente, "Dependentes.csv")
# dependentes <- read_csv("Dependentes.csv")
# unique(dependentes$Reference) %>% sort()
#

# test
test <- datafull
test_C_Similarity <- test %>% 
  filter(Ecological_metric == "similarity")

test_S_Similarity <- test %>% 
  filter(Ecological_metric == "abundance"| Ecological_metric == "richness" | Ecological_metric == "diversity")


# Selecting landscape with > 2 RR values
RR_2_C <- test_C_Similarity %>% 
  group_by(Site) %>% 
  filter(n()>1) %>% 
  ungroup()

RR_2_S <- test_S_Similarity %>% 
  group_by(Site) %>% 
  filter(n()>1) %>% 
  ungroup()

# Calculating RR var and RR mean
data <- RR_2_S
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

datafull_S <- as.tibble(data3)


# Calculating RR var and RR mean
data <- RR_2_C
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

datafull_C <- as.tibble(data3)
datafull_C %>% 
  distinct(Site)


unique(datafull$Reference)
