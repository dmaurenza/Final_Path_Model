# dependent variable information

# test 1
Global_Var$Site <- as.character(Global_Var$Site)
Global_Var %>% 
  select(Site, RR_var)

total <- datafull # line 39
  
colnames(datafull)
total <- total %>% 
  select(1:17, Taxon) %>% 
  filter(!Ecological_metric == "others")
unique(total$Ecological_metric)
unique(total$Site)

Reference <- datafull %>% #after line 20
  select(Study, Site, Reference)

Reference$Site <- as.character(Reference$Site)

Reference <- Reference %>% 
  distinct(Site, .keep_all = T) 
unique(Reference$Site)
unique(Reference$Reference)
dependente <- total %>% 
  left_join(y = Reference, by = "Site")
#number <- dependente %>% 
#  count(Site, Ecological_metric, Taxon)
#dependente <- number %>% 
total %>% 
  # group_by(Ecological_metric) %>% 
  #count(Ecological_metric)
  filter(Ecological_metric == "similarity") %>% 
  summarise(mean(RR), max(RR), min(RR))
  
length(unique(total$Reference)    )
length(unique(total$Site)    )


(100*61)/421
17+17
x <- datafull_base2 %>% 
  group_by(Study_year) %>% 
  summarise(ano = n())


summary(x$Time_restored)
summary(x$Study_year)

# 147
write_csv(total, "Total.csv")


#Test 2
Global_Var$Site <- as.character(Global_Var$Site)
Global_Var %>% 
  select(Site, RR_var)

total <- datafull %>% #from Pre-processing and after RR mean absolute calculation (line 111)
  select(Site, Ecological_metric, Taxon, RRmean)
colnames(datafull)

Reference <- datafull %>% #after line 10 from pre-processing
  select(Study, Site, Reference)

Reference$Site <- as.character(Reference$Site)

Reference <- Reference %>% 
  distinct(Site, .keep_all = T)

# joing with datafull (n = 32)
dependente <- total %>% 
  left_join(y = Reference, by = "Site")

# the same of "Total" (32 lines), but  using scale values
total_scale <- read_csv("RData/Datafull2.csv")

# Graph
# After line 152
hist(datafull$RR_var, 100)

library (ggplot2)

graph <- datafull %>%
  select(Study, Site, Ecological_metric, RR_var)

Graph_1 <- ggplot(graph, aes(x=RR_var)) +
  geom_histogram(breaks=seq(0, 3, by=0.1))+
      labs( title = "Deviation in biodiversity recovery")
