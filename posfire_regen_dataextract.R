## grabbing data for Miranda & Stephanie
## written 5.31.22 - 6.2.22 by Katie Nigro

## NEED:
## plot, lat, long, name of fire, year of fire, year of sampling, 
## proportion of trees dead, proportion of trees dead that were burned,
## fire condition, presencen/absence for each species seedlings, 
## fraction of dead trees by species, fraction of alive trees by species,
## climate data, snow persistence 

### allows multiple conditions in a plot as long as the entire
### plot is in one owner group, had one or fewer disturbances/treatments
### across it, and no evidence of artificial regeneration. 

#read in packages
library(readr)
library(raster)
library(foreign)
library(sf)
library(ggplot2)
library(rgdal)
library(units)
library(spatialEco)
library(sp)
library(dplyr)
library(stringr)
library(tidyverse) 
library(data.table)
library(spatial.tools)
library(car)

## Subset data into public plots with one condition
#FIA data for AZ, CO, NM, NV, ID, UT, WY, MT

plot<- read_csv("compiled_data_annual2020/annual_plots2020.csv")
cond<- read_csv("compiled_data_annual2020/COND.csv")

tree <- read_csv("compiled_data_annual2020/TREE.csv")
seedling<- read_csv("compiled_data_annual2020/SEEDLING.csv")

####################
#goal = obtain all plots that are completely within public land & accessible (COND_STATUS_CD < 3)
public.plots<- plot %>% 
  filter(!CN %in% c(cond %>% 
                      filter(OWNCD > 40 | COND_STATUS_CD > 2) %>% 
                      pull(PLT_CN))
  )

#check that there are no private owners in here
View(cond %>% 
       filter(PLT_CN %in% public.plots$CN))
#there aren't
####################

####################
#goal = filter public plots down to those that only had one disturbance/treatment condition across all subplots
disturbance.summary<- cond %>% 
  filter(PLT_CN %in% public.plots$CN) %>% 
  group_by(PLT_CN) %>% 
  dplyr::summarise(d1 = n_distinct(DSTRBCD1,na.rm=TRUE),
                   d2 = n_distinct(DSTRBCD2,na.rm=TRUE),
                   d3 = n_distinct(DSTRBCD3,na.rm=TRUE),
                   t1 = n_distinct(TRTCD3,na.rm=TRUE),
                   t2 = n_distinct(TRTCD3,na.rm=TRUE),
                   t3 = n_distinct(TRTCD3,na.rm=TRUE),
                   p2a_d1 = n_distinct(DSTRBCD1_P2A,na.rm=TRUE),
                   p2a_d2 = n_distinct(DSTRBCD2_P2A,na.rm=TRUE),
                   p2a_d3 = n_distinct(DSTRBCD3_P2A,na.rm=TRUE),
                   p2a_t1 = n_distinct(TRTCD1_P2A,na.rm=TRUE),
                   p2a_t2 = n_distinct(TRTCD2_P2A,na.rm=TRUE),
                   p2a_t3 = n_distinct(TRTCD3_P2A,na.rm=TRUE),
  )

public.onedist.plots<- public.plots %>% 
  filter(! CN %in% c(
    disturbance.summary %>% 
      filter(if_any(.cols=d1:p2a_t3,.fns = ~.>1)) %>% 
      pull(PLT_CN) #these are plots that have different disturbances in different subplots
  )
  )

nrow(public.onedist.plots)
######################

#######################
#now do the same thing for the previously sampled plot

plot %>% 
  filter(CN %in% plot$PREV_PLT_CN) %>% 
  filter(!is.na(PREV_PLT_CN)) #there are no plots survyed more than two times



public.plots2<- public.onedist.plots %>% 
  filter(!PREV_PLT_CN %in% c(cond %>% 
                               filter(OWNCD > 40 | COND_STATUS_CD > 2) %>% 
                               pull(PLT_CN))
  )
public.onedist.plots2<- public.plots2 %>% 
  filter(! PREV_PLT_CN %in% c(
    disturbance.summary %>% 
      filter(if_any(.cols=d1:p2a_t3,.fns = ~.>1)) %>% 
      pull(PLT_CN) #these are plots that have different disturbances in different subplots
  )
  )

nrow(public.onedist.plots2)

#######################
#eliminate plots with documented artificial regen or stand origin from artificial regen
no.artregen.plots<- public.onedist.plots2 %>% 
  filter(!CN %in% c(cond %>% 
                      filter(TRTCD1 == 30 | TRTCD2 == 30 | TRTCD3 == 30 |
                               TRTCD1_P2A == 30 | TRTCD2_P2A == 30 | TRTCD3_P2A == 30 | 
                               STDORGCD ==1 ) %>% 
                      pull(PLT_CN) %>% 
                      unique())
  )
no.artregen.plots2<- no.artregen.plots %>% 
  filter(!PREV_PLT_CN %in% c(cond %>% 
                               filter(TRTCD1 == 30 | TRTCD2 == 30 | TRTCD3 == 30 |
                                        TRTCD1_P2A == 30 | TRTCD2_P2A == 30 | TRTCD3_P2A == 30 | 
                                        STDORGCD ==1 ) %>% 
                               pull(PLT_CN) %>% 
                               unique())
  )

#################

################
#summary of final plots

no.artregen.plots2 %>% 
  group_by(KINDCD) %>% 
  dplyr::summarise(n=n()) #no periodic inventory plots here

cond %>% 
  filter(PLT_CN %in% no.artregen.plots2$CN) %>% 
  group_by(PLT_CN) %>% 
  dplyr::summarise(conds = n_distinct(CONDID)) %>% 
  group_by(conds) %>% 
  dplyr::summarise(n=n())


##reduce to just the unique plot locations
unique_plots<- no.artregen.plots2 %>% 
  filter(!CN %in% no.artregen.plots2$PREV_PLT_CN) #64,286 plot locations
##

##make lat/long into shapefile
# unique_plots_sp <- unique_plots
# coordinates(unique_plots_sp)=~LON+LAT
# proj4string(unique_plots_sp)<- CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")
# 
# shapefile(unique_plots_sp, "FIA_fuzzed_locations_regenproj.shp") 
##not sure if I should use fuzzed or real coordinates to extract disturbance & climate data

##read in environmental vars
env_vars <- read_csv("env_vars_FIA_2022.csv") %>% 
  dplyr::select(-1)

############## disturbance data ###############################

## Now extract MTBS fire perimeters that intersect dry domain
## plots. Need MTBS perimeter shapefile downloaded

##spatial join - dry domain plot data with MTBS fire perims
#load packages

# points = readOGR(dsn="F:/FIA_plot_locations_multconds", layer = "FIA_plot_locations_multconds")
# polys = readOGR(dsn="C:/Users/katie/My Drive/ArcGIS/mtbs_perimeter_data", layer ="mtbs_perims_DD")
# polys = spTransform(polys,CRS("+proj=longlat +datum=NAD83 +no_defs"))
# 
# polys@data$poly.ids <- 1:nrow(polys)
# join <- point.in.poly(points, polys, sp=FALSE, duplicate=FALSE)
# jointable <- as.data.frame(join@data)
# setwd("C:/Users/katie/My Drive/FIA project/FIA_rproject")
# write.csv(jointable, "mtbs_join_2022.csv")
# write.csv(polys@data, "mtbs_data_2022.csv")

##subset plot data to include only plots with MTBS fire history
join.table<- read.csv("C:/Users/katie/My Drive/FIA project/FIA_rproject/mtbs_join_2022.csv")
mtbs_data <- read.csv("C:/Users/katie/My Drive/FIA project/FIA_rproject/mtbs_data_2022.csv")

join.table.fire <- join.table[! is.na(join.table$pid1), ]
nrow(join.table)
nrow(join.table.fire)

##grab most recent fire

joindata <- join.table.fire %>% 
  left_join(mtbs_data, by=c("pid1"="Fire_ID"))
joindata$fire1 = paste(joindata$Fire_Name, 
                       joindata$Fire_Type, sep="")
joindata$year1 = paste(joindata$Year)
joindata <-joindata %>% 
  dplyr::select("PLT_CN","pid2","pid3","pid4","pid5","pid6","fire1","year1")

joindata <- joindata %>% 
  left_join(mtbs_data, by=c("pid2"="Fire_ID"))
joindata$fire2 = paste(joindata$Fire_Name,
                       joindata$Fire_Type, sep="")
joindata$year2 = paste(joindata$Year)
joindata <- joindata %>% 
  dplyr::select(PLT_CN, pid3, pid4,pid5, pid6, fire1,year1,fire2,year2)

joindata <- joindata %>% 
  left_join(mtbs_data, by=c("pid3"="Fire_ID"))
joindata$fire3 = paste(joindata$Fire_Name,  
                       joindata$Fire_Type, sep="")
joindata$year3 = paste(joindata$Year)
joindata <- joindata %>% 
  dplyr::select(PLT_CN, pid4,pid5, pid6,fire1,year1,fire2,year2,fire3,year3)

joindata <- joindata %>% 
  left_join(mtbs_data, by=c("pid4"="Fire_ID"))
joindata$fire4 = paste(joindata$Fire_Name, 
                       joindata$Fire_Type, sep="")
joindata$year4 = paste(joindata$Year)
joindata <- joindata %>% 
  dplyr::select(PLT_CN, pid5, pid6,fire1,year1,fire2,year2,
                fire3,year3,fire4,year4)

joindata <- joindata %>% 
  left_join(mtbs_data, by=c("pid5"="Fire_ID"))
joindata$fire5 = paste(joindata$Fire_Name, 
                       joindata$Fire_Type, sep="")
joindata$year5 = paste(joindata$Year)
joindata <- joindata %>% 
  dplyr::select(PLT_CN, pid6,fire1,year1,fire2,year2,
                fire3,year3,fire4,year4,fire5,year5)

joindata <- joindata %>% 
  left_join(mtbs_data, by=c("pid6"="Fire_ID"))
joindata$fire6 = paste(joindata$Fire_Name,
                       joindata$Fire_Type, sep="")
joindata$year6 = paste(joindata$Year)
joindata <- joindata %>% 
  dplyr::select(PLT_CN,fire1,year1,fire2,year2,
                fire3,year3,fire4,year4,fire5,year5,fire6,year6)

View(joindata)
joindata[joindata=="NANANA"]<- " "

mtbs_all <- joindata %>% 
  unite("fire1",fire1:year1, sep="___", remove=TRUE) %>% 
  unite("fire2",fire2:year2, sep="___", remove=TRUE) %>% 
  unite("fire3",fire3:year3, sep="___", remove=TRUE) %>% 
  unite("fire4",fire4:year4, sep="___", remove=TRUE) %>% 
  unite("fire5",fire5:year5, sep="___", remove=TRUE) %>% 
  unite("fire6",fire6:year6, sep="___", remove=TRUE) %>% 
  pivot_longer(cols=c("fire1","fire2","fire3","fire4","fire5","fire6"), names_to = "fire_num",
               values_to="fire_name") %>% 
  separate("fire_name", into=c("fire_name","year"), sep="___", remove=TRUE) %>% 
  filter(!(fire_name == "NANA" & year == "NA"))
  
mtbs_all

View(mtbs_all %>% 
  group_by(PLT_CN) %>% 
  summarise(n_fires = n()))



##Now extract disturbance events from LANDFIRE !!
## first read in rasters and crop to western US
dist.dir <- 'F:/to use with real coordinates/US_VegDIST2014_10312018/annual_rasters'

years <- c('1999','2000','2001','2002','2003','2004','2005','2006','2007','2008','2009','2010',
           '2011','2012','2013','2014')

e = extent(-2199337.478017,99686.997269,250725,3180555)

rast.list = c()
for(j in 1:length(years)){
  cat(j, " ")
  raster<-paste(dist.dir,'/us_dist',years[j],sep="")
  rast <- raster(raster)
  #rast.crop <- crop(rast, e)
  rast.list <- append(rast.list, rast)#.crop)
}

##then extract raster info for each FIA plot 

plot.pts<-shapefile("F:/FIA_plot_locations_multconds/FIA_plot_locations_multconds.shp")
plot.pts.dbf <- read.dbf("F:/FIA_plot_locations_multconds/FIA_plot_locations_multconds.dbf")


# extract disturbances from each year's raster 
metadatapath <- 'F:/to use with real coordinates/US_VegDIST2014_10312018/annual'

dist_data<-vector()
for(j in 1:length(years)){
  ext.points <- raster::extract(rast.list[[j]], plot.pts, df=TRUE)
  dist<-ext.points[,2]
  CN<-plot.pts.dbf[,"PLT_CN"]
  year <- rep(years[j],length(ext.points[,2]))
  dist_CN<-cbind(CN,dist,year)
  metadata<- read.csv(paste(metadatapath,'/US_DIST',years[j],'/CSV_Data/US_disturb',years[j],'.csv',sep=""))
  dist_CN_meta <- merge(dist_CN, metadata[,c(1,3:10)], by.x="dist", by.y="Value")
  dist_data<-rbind(dist_data,dist_CN_meta)
  cat(j, " ")}

dist_data_clean = dist_data[complete.cases(dist_data[,4]),]
landfireHC = dist_data_clean[dist_data_clean$Type_Confidence=="High" | dist_data_clean$Type_Confidence=="Medium to High",]



######
##extract from condition data
######

cond_dist<- cond %>% 
  filter(PLT_CN %in% c(unique_plots$CN, unique_plots$PREV_PLT_CN)) %>% 
  select(PLT_CN,DSTRBCD1,DSTRBYR1,DSTRBCD2,DSTRBYR2,DSTRBCD3,DSTRBYR3,
         TRTCD1,TRTYR1,TRTCD2,TRTYR2,TRTCD3,TRTYR3,DSTRBCD1_P2A,DSTRBCD2_P2A,
         DSTRBCD3_P2A,DSTRBYR1_P2A,DSTRBYR2_P2A,DSTRBYR3_P2A) %>% 
  filter(!(is.na(DSTRBCD1)& is.na(TRTCD1))) %>% 
  filter(!(is.na(DSTRBCD1) & TRTCD1 == 0)) %>% 
  filter(!(DSTRBCD1 == 0 & is.na(TRTCD1))) %>% 
  filter(!(DSTRBCD1 == 0 & TRTCD1 == 0)) %>% 
  unite("DIST1",c(DSTRBCD1,DSTRBYR1),sep="___",remove=TRUE) %>% 
  unite("DIST2",c(DSTRBCD2,DSTRBYR2),sep="___",remove=TRUE) %>% 
  unite("DIST3",c(DSTRBCD3,DSTRBYR3),sep="___",remove=TRUE) %>% 
  unite("TRT1",c(TRTCD1,TRTYR1),sep="___",remove=TRUE) %>% 
  unite("TRT2",c(TRTCD2,TRTYR2),sep="___",remove=TRUE) %>% 
  unite("TRT3",c(TRTCD3,TRTYR3),sep="___",remove=TRUE) %>% 
  unite("DIST1_P2A",c(DSTRBCD1_P2A,DSTRBYR1_P2A),sep="___",remove=TRUE) %>% 
  unite("DIST2_P2A",c(DSTRBCD2_P2A,DSTRBYR2_P2A),sep="___",remove=TRUE) %>% 
  unite("DIST3_P2A",c(DSTRBCD3_P2A,DSTRBYR3_P2A),sep="___",remove=TRUE) %>% 
  pivot_longer(cols = DIST1:DIST3_P2A, names_to = "label", values_to = "dist_code") %>% 
  separate(dist_code, into = c("dist_code","year"), sep="___") %>% 
  filter(!dist_code %in% c("NA",0))
  
prevplots_cond<- cond_dist %>% 
  filter(PLT_CN %in% unique_plots$PREV_PLT_CN) %>% 
  left_join(unique_plots %>% dplyr::select(CN, PREV_PLT_CN), by=c("PLT_CN" = "PREV_PLT_CN")) %>% 
  mutate(PLT_CN = CN) %>% 
  dplyr::select(-CN)

cond_dist2<- cond_dist %>% 
  bind_rows(prevplots_cond) %>% 
  filter(PLT_CN %in% unique_plots$CN)

##combine all previously made tables into one
landfire_clean<- read.csv("landfire_clean.csv")
landfire_clean
cond_dist2
mtbs_all
#landfireHC

colnames(cond_dist2)
colnames(mtbs_all)
colnames(landfire_clean)
#colnames(landfireHC)

mtbs_clean <- mtbs_all %>% 
  rename("label" = "fire_num", "dist_code" = "fire_name") %>% 
  mutate(dist_type = "fire")
colnames(mtbs_clean)
# landfire_clean <- landfireHC %>% 
#   rename("label" = "dist","PLT_CN" = "CN", "dist_code" = "Dist_Type") %>% 
#   select("PLT_CN","label", "dist_code","year","Type_Confidence","Severity","Sev_Confidence","Source1",
#          "Source2","Source3","Source4") %>% 
#   mutate(PLT_CN = as.numeric(PLT_CN))

landfire_clean2<- landfire_clean %>% 
  dplyr::select(-X) %>% 
  mutate(label = as.character(label), year= as.character(year))

#write.csv(landfire_clean, "landfire_clean.csv")

all_dist<- mtbs_clean %>% 
  bind_rows(cond_dist2) %>% 
  bind_rows(landfire_clean2)

all_dist %>% filter(is.na(dist_type)) %>% 
  pull(dist_code) %>% 
  unique()

all_dist2 <- all_dist %>% 
  mutate(dist_type = dplyr::case_when(
    label %in% c("TRT1","TRT2","TRT3") & dist_code %in% c("10","40") ~ "harvest",
    label %in% c("TRT1","TRT2","TRT3") & dist_code %in% c("50") ~ "silviculture_other",
    label %in% c("TRT1","TRT2","TRT3") & dist_code %in% c("20") ~ "siteprep",
    label %in% c("TRT1","TRT2","TRT3") & dist_code %in% c("30") ~ "art_regen",
    label %in% c("DIST1","DIST2","DIST1_P2A","DIST2_P2A","DIST3","DIST3_P2A") & 
      dist_code %in% c("10","11","12","20","21","22") ~ "insect.disease",
    label %in% c("DIST1","DIST2","DIST1_P2A","DIST2_P2A","DIST3","DIST3_P2A") & 
      dist_code %in% c("30") ~ "fire",
    label %in% c("DIST1","DIST2","DIST1_P2A","DIST2_P2A","DIST3","DIST3_P2A") & 
      dist_code %in% c("31") ~ "groundfire",
    label %in% c("DIST1","DIST2","DIST1_P2A","DIST2_P2A","DIST3","DIST3_P2A") & 
      dist_code %in% c("32") ~ "crownfire",
    label %in% c("DIST1","DIST2","DIST1_P2A","DIST2_P2A","DIST3","DIST3_P2A") & 
      dist_code %in% c("40","41","42","43","44","45","46") ~ "animal",
    label %in% c("DIST1","DIST2","DIST1_P2A","DIST2_P2A","DIST3","DIST3_P2A") & 
      dist_code %in% c("50","51","52","53","54") ~ "weather",
    label %in% c("DIST1","DIST2","DIST1_P2A","DIST2_P2A","DIST3","DIST3_P2A") & 
      dist_code %in% c("60") ~ "vegetation",
    label %in% c("DIST1","DIST2","DIST1_P2A","DIST2_P2A","DIST3","DIST3_P2A") & 
      dist_code %in% c("70") ~ "unknown",
    label %in% c("DIST1","DIST2","DIST1_P2A","DIST2_P2A","DIST3","DIST3_P2A") & 
      dist_code %in% c("80") ~ "human_other",
    label %in% c("DIST1","DIST2","DIST1_P2A","DIST2_P2A","DIST3","DIST3_P2A") & 
      dist_code %in% c("90","91","92","93","94","95") ~ "geologic",
    Severity %in% c("Increased Green","Low","Unburned/Low","Nodata/Non-processing mask") &
      dist_code %in% c("Wildfire","Prescribed Fire","Wildland Fire","Wildland Fire Use") ~ "groundfire",
    Severity %in% c("Medium") &
      dist_code %in% c("Wildfire","Prescribed Fire","Wildland Fire","Wildland Fire Use") ~ "medfire",
    Severity %in% c("High") &
      dist_code %in% c("Wildfire","Prescribed Fire","Wildland Fire","Wildland Fire Use") ~ "crownfire",
    dist_code %in% c("Insects","Insects/Disease") ~ "insect.disease",
    dist_code %in% c("Clearcut","Harvest","Mastication","Other Mechanical","Thinning") ~ "harvest",
    dist_code %in% c("Chemical","Herbicide","Insecticide") ~ "chemical",
    dist_code %in% c("Unknown") ~ "unknown",
    dist_code %in% c("Weather") ~ "weather",
    TRUE ~ dist_type
  ))
# I put medium severity fire into its own category and classified high severity fire (from 
# Landfire) as "crownfire". May want to combine these but I left them separate for now. 

all_dist2 %>% pull(dist_type) %>% unique()    
#no plots with artificial regen 

##############################
## extract snow persistence ##
##############################

snow.raster<- raster("C:/Users/katie/My Drive/ArcGIS/SP_mean.tif")

##then extract raster info for each FIA plot 

plot.pts<-shapefile("F:/FIA_plot_locations_multconds/FIA_plot_locations_multconds.shp")
plot.pts.dbf <- read.dbf("F:/FIA_plot_locations_multconds/FIA_plot_locations_multconds.dbf")


# extract disturbances from each year's raster 
metadatapath <- 'F:/to use with real coordinates/US_VegDIST2014_10312018/annual'

snow.ext <- raster::extract(snow.raster, plot.pts, df=TRUE)

snowp<- data.frame(PLT_CN = plot.pts.dbf[,"PLT_CN"],SP_mean = snow.ext$SP_mean)


#################################################
######### adding tree/seedling data #############
#################################################

adults <- tree %>% 
  filter(!STATUSCD == 0) %>% 
  group_by(SPCD,PLT_CN) %>% 
  dplyr::summarise(adult_total = n()) %>% 
  filter(PLT_CN %in% unique_plots$CN)

head(adults)

seeds <- seedling %>% 
  group_by(SPCD, PLT_CN) %>% 
  dplyr::summarise(seedling_mintotal = sum(TREECOUNT_CALC)) %>% 
  filter(PLT_CN %in% unique_plots$CN)

head(seeds)

##get total # of dead or harvested trees 
dead_adults <- tree %>% 
  filter(STATUSCD == 2) %>% 
  group_by(SPCD,PLT_CN) %>% 
  dplyr::summarise(adult_dead = n()) %>% 
  filter(PLT_CN %in% unique_plots$CN) 
  #mutate(SPCD = paste(SPCD, "_dead", sep=""))
cut_adults <-tree %>% 
  filter(STATUSCD == 3) %>% 
  group_by(SPCD,PLT_CN) %>% 
  dplyr::summarise(adult_cut = n()) %>% 
  filter(PLT_CN %in% unique_plots$CN)
  #mutate(SPCD = paste(SPCD, "_cut", sep=""))
burned_adults <- tree %>% 
  filter(DAMAGE_AGENT_CD1 == 30000 |
          DAMAGE_AGENT_CD2 == 30000 |
           DAMAGE_AGENT_CD3 == 30000 ) %>% 
  group_by(SPCD,PLT_CN) %>% 
  dplyr::summarise(adult_burned = n()) %>% 
  filter(PLT_CN %in% unique_plots$CN) 
  #mutate(SPCD = paste(SPCD, "_burned", sep=""))
deadbyfire_adults<-tree %>% 
  filter(STATUSCD == 2 & AGENTCD == 30) %>% 
  group_by(SPCD,PLT_CN) %>% 
  dplyr::summarise(adult_dbyfire = n()) %>% 
  filter(PLT_CN %in% unique_plots$CN)

#compile together

tree.data<- adults %>% 
  left_join(dead_adults, by=c("PLT_CN","SPCD")) %>% 
  left_join(cut_adults, by=c("PLT_CN","SPCD")) %>% 
  left_join(burned_adults, by=c("PLT_CN","SPCD")) %>% 
  left_join(deadbyfire_adults, by=c("PLT_CN","SPCD")) %>% 
  left_join(seeds, by=c("PLT_CN","SPCD")) 
  

#combine with other attributes

all_data<- unique_plots %>% 
  select(CN, LAT, LON, ELEV, MEASYEAR, MEASMON, MEASDAY, PLOT_STATUS_CD, KINDCD,
         DESIGNCD, MANUAL, SAMP_METHOD_CD) %>% 
  left_join(all_dist2, by=c("CN" = "PLT_CN")) %>% 
  left_join(tree.data, by=c("CN" = "PLT_CN")) %>% 
  left_join(env_vars, by=c("CN" = "PLT_CN")) %>% 
  select(- c(X, RSCD, INVYR, ANN_INV, CYCLE)) %>% 
  left_join(snowp, by=c("CN" = "PLT_CN"))

View(all_data)

all_data %>% 
  group_by(dist_type) %>% 
  summarise(n_plots = n_distinct(CN)) #number of plots with different disturbances

all_data %>% 
  group_by(CN) %>% 
  summarise(n_dist = n_distinct(dist_type)) %>% 
  group_by(n_dist) %>% 
  summarise(n_plots = n()) #this is the number of plots that have 1 or more disturbances (n_dist)

View(all_data %>% filter(CN == unique(all_data$CN)[26]))

write.csv(all_data, "postfireregen_data1_060622.csv")
