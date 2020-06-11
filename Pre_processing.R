# Github do Felipe Barros - # https://github.com/FelipeSBarros/RestorationMetaAnalysis
# Pre_processing -----
library(tidyverse)

# Reading datasets -------------------------------------------
#buffer_09_06_2020 <- read_csv("Biodiversity_Revision_09_06_2020_filtrado.csv")%>% 
#  mutate(Site = as.character(Site))
datafull <- read_csv("database.full.csv")
buffer <- read_csv("Biodiversity_Revision.csv") %>% 
  mutate(Site = as.character(Site))

## Biodiversity data
datafull <- datafull %>% 
  filter(Mammals == 1| Birds == 1| Herpetofauna == 1| Plants == 1| Invertebrates == 1,
    Latitude < 35 & Latitude >-35 & Restoration_activity == "passive") %>% 
  mutate(Study = as.character(Study),
    Site = as.character(Site)) ## Tropical & subtropical landscapes restored through passive regeneration
colnames(buffer)

## Data used from Crouzeilles et al. 2016 database 
colnames(datafull)
datafull <- datafull %>% 
  select(Study, Site, Longitude, Latitude, Past_disturbance, Forest_conversion, Study_year, Time_restored, Plants:Mammals, RR, Ecological_metric, Reference)
class(buffer$Site)
## Merge biodiversity & predictor variables in a single data frame
# datafull <- datafull %>% 
#   inner_join(buffer, by = "Site") %>% 
#   select(-`system:index`, -RR_var)
# colnames(datafull)  

datafull <- datafull %>% #Using buffer 09_06_2020 
  #inner_join(buffer_09_06_2020, by = "Site") %>% 
  inner_join(buffer, by = "Site") %>% 
  select(-`system:index`)
colnames(datafull)  
# Removing NAs from Time since restoration started
datafull <- datafull %>% 
  filter(!is.na(Time_restored)) %>% # Eliminating NA values in Time Restored 
  mutate(Taxon = NA)

datafull[datafull$Plants == 1,"Taxon"] <- "Plant"
datafull[datafull$Invertebrates == 1,"Taxon"] <- "Invertebrates"
datafull[datafull$Birds == 1,"Taxon"] <- "Birds"
datafull[datafull$Herpetofauna == 1,"Taxon"] <- "Herpetofauna"
datafull[datafull$Mammals == 1,"Taxon"] <- "Mammals"
total <- datafull

  

# We started using 423 RR values, including abundance, richness, diversity, similarity and others. After remove similarity and others, we got 360 RR values. After remove Sites with only 1 RR, and outliers points we got 350 RR to calculate RR_var. After calculating mean RR_var, we got 32 Sites.
## criterion for database - without similarity as an ecological metric
datafull <- datafull %>% 
  filter(Ecological_metric == "abundance"| Ecological_metric == "richness" | Ecological_metric == "diversity")

# Selecting landscape with > 2 RR values
datafull <- datafull %>% 
  group_by(Site) %>% 
  filter(n()>1) %>% 
  ungroup()

### check landscapes with potential problem - remover esse site desde o início do estudo - não ter ele na tabela quando publicar

hist(datafull$RR, 10)
outlyers<-boxplot(datafull$RR)
sort(outlyers$out)
datafull<-subset(datafull, datafull$RR<3 & datafull$RR>-3)

# x <- datafull %>%
#   filter(Site == "143") %>%
#   select(RR, Taxon, Ecological_metric, Site, Study)
# 
# datafull<-datafull %>% 
  # filter(!Site == "143")
# 
 #x <- data3 %>%
 #filter(Site == "158") %>%
  #select(RR, Taxon, Ecological_metric, Site, Study)
# 
#datafull<-datafull %>% 
 #  filter(!Site == "158")
# 
# x <- datafull %>%
#   filter(Site == "174") %>%
#   select(RR, Taxon, Ecological_metric, Site, Study)
# 
 #datafull<-datafull %>% 
# filter(!Site == "174")


# testing non-effect of ecological metric
# Testes #####
model <- lm(RR ~ Ecological_metric, data = datafull)
summary(model)
boxplot(RR ~ Ecological_metric, data = datafull)


# testing non-effect of taxonomic groups
# Testes #####
model <- lm(RR ~ Taxon, data = datafull)
summary(model)
boxplot(RR ~ Taxon, data = datafull)
colnames(datafull)

# Calculating RR var and RR mean
data <- datafull %>% 
  select(-RR_var) 
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
colnames(data)
datafull <- as.tibble(data3)
colnames(datafull)

summary(datafull$Time_restored)
# Calculating  metrics
datafull <- datafull %>% 
  group_by(Site) %>% # Grouping by Site to calculate metrics by Site
  mutate(RRmean = mean(RR), #Calculating RR Mean by Site
    Time_mean = mean(Time_restored)) %>% # Calculating Time Mean by Site
  ungroup() %>% # Ungroup Sites
  distinct(Site, .keep_all = T) %>%  # Unifying metrics by Study. Thats because we have more Studies than Sites. Next step will disctinc Sites. # Taxonomic groups, RRcor, N and RR colluns doesn't matter anymore
  mutate(RRmean = abs(RRmean))# Transforming RRmean in absolut RRmean

#Testing taxonomic groups per landscape
# x <- datafull %>%
#   select(Study, Site,RR, RRmean,Time_restored,Time_mean,AnualTemp_05Km, OpCostIIS_05Km_SUM, RR_var)

# x <- datafull2 %>%
#   select(Study, Site,RR, RRmean,Time_restored,Time_mean,AnualTemp_05Km, OpCostIIS_05Km_SUM, RR_var, Taxon) %>%
#   filter(Site == "137") #this site has two taxonomic groups, birds and herpetofauna, we used only herpetofauna because it has 2 RRs, while birds has 1 RR only 


# Descarting studies ----
# Once same sites were sampled in more than 1 study, we decide to eliminate those studies with lowest n = landscapes. 

#x <-datafull %>% 
 # count(Study, Site) # 3 Studies more than sites. Estudos 69, 190 e 226 possem menor quantidade de Sites. Serão descartados.
  
#datafull <- datafull %>% 
 # filter(!Study == "190" )

#datafull %>% 
 # filter(Study == "226" )#3

#datafull %>% 
  #filter(Study == "190" )#3

#datafull %>% 
 # filter(Study == "215" )#3

#datafull %>% 
 # group_by(Study)
#datafull %>% 
 # group_by(Site)
#colnames(datafull)

datafull_base<-datafull
colnames(datafull_base)
sort(datafull_base$RR_var)
#outlyers<-boxplot(datafull_base$RR_var)
#sort(outlyers$out)
#datafull<-subset(datafull, datafull$RR_var < 10)
hist(datafull$RR_var, 100)

datafull_base2<-datafull
# # Scale ----------------------VERIFICAR NUMERO DAS COLUNAS _ MODIFICADO
# # Scale function. Eliminating some "urb" colluns to apply scale function. Scaled valeus became NaN because excess of "0" values.
#datafull <- datafull %>%
#  select( -urb_05Km, -urb_100Km, -urb_10Km, -urb_25Km, -urb_50Km, -urb_75Km, latitude_209564#535, -longitude_209564535, -Latitude.x, -Latitude.y)

datafull <- datafull %>%
  select( -urb_05Km, -urb_100Km, -urb_10Km, -urb_25Km, -urb_50Km, -urb_75Km, latitude_209564535, longitude_209564535 )

colnames(datafull) 
# # Saving data before scale function
# datafull_no_scale <- datafull
#   
# write_csv(datafull_no_scale, "RData/datafull_no_scale.csv")
# 
# # Applying scale function
# colnames(datafull)
# datafull %>%
#   select(Study, Site,RR, Ecological_metric, Taxon, RRmean, RR_var)
# 
datafull[,c(17:324, 329:331)] <- scale(datafull[,c(17:324, 329:331)])
# datafull[,c(17:326, 330:333)] <- scale(datafull[,c(17:326, 330:333)])


# Final table -------------------------------------------------------------
data <- datafull %>%
  select(Study, Site, Ecological_metric, Taxon, RRmean, RR_var, Time_mean, Past_disturbance, AnualTemp_05Km:wfire_75Km)

### RR_var scale and outlyers
hist(data$RR_var, 100)
#shapiro.test(data$RR_var)
sort(data$RR_var)



# Saving Final Table ------------------------------------------------------
x <- read_csv("RData/Datafull2.csv")
colnames(x)
write_csv(data, "RData/Datafull2.csv")
#write_csv(data, "RData/Datafull2_urban.csv")
