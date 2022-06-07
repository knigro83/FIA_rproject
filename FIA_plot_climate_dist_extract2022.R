### this is the whole flow from narrowing down FIA plots 
### to extracting climate and disturbance data from them
### written 3/12 - 3/14/22
### allows multiple conditions in a plot as long as the entire
### plot is in one owner group, had one or fewer disturbances/treatments
### across it, and no evidence of artificial regeneration. 

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

## explore plot subset more
cond %>% 
  filter(PLT_CN %in% unique_plots$CN) %>% 
  group_by(COND_STATUS_CD) %>% 
  dplyr::summarise(n=n())

unique_plots %>% 
  group_by(PLOT_STATUS_CD) %>% 
  dplyr::summarise(n=n())

unique_plots %>% 
  group_by(DESIGNCD) %>% 
  dplyr::summarise(n=n())

View(unique_plots %>% 
  left_join(cond,by=c("CN"="PLT_CN")) %>% 
  group_by(COND_STATUS_CD,MANUAL) %>% 
  dplyr::summarise(n=n()))

unique_plots %>% 
  group_by(PLOT_STATUS_CD,SAMP_METHOD_CD) %>% 
  dplyr::summarise(n=n())

tree %>% 
  filter(PLT_CN %in% c(unique_plots %>% filter(SAMP_METHOD_CD == 2) %>% pull(CN)))
tree %>% 
  filter(PLT_CN %in% c(unique_plots %>% filter(PLOT_STATUS_CD == 2 & SAMP_METHOD_CD==1) %>% pull(CN))) %>% 
  group_by(SPCD) %>% 
  dplyr::summarise(n=n())

tree %>%
  filter(PLT_CN %in% unique_plots$CN) %>% 
  pull(PLT_CN) %>% 
  unique() %>% 
  length()

seedling %>%
  filter(PLT_CN %in% unique_plots$CN) %>% 
  pull(PLT_CN) %>% 
  unique() %>% 
  length()

################


##read in plot locations and convert to shapefile
plot.locations <- read.csv("F:/CONFIDENTIAL_for_Redmond.csv")

#need unique_plots dataframe from above
plots_needed_coords = plot.locations[which(plot.locations$PLT_CN %in% unique_plots$CN),]
nrow(plots_needed_coords)
str(plots_needed_coords)

plots_needed_coords.crs = "+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs "
plots_needed_coords.sp <- st_as_sf(plots_needed_coords, coords=c("ACTUAL_LON","ACTUAL_LAT"), crs=plots_needed_coords.crs)
st_crs(plots_needed_coords.sp)

ggplot()+
  geom_sf(data=plots_needed_coords.sp)

st_write(plots_needed_coords.sp, "FIA_plot_locations_multconds",driver="ESRI Shapefile")



### now extract climate data ###

# set location of climate data
setwd("F:/")
climdir<-"F:/to use with real coordinates/PRISM"

# create list of months of interest

months<-c('01','02','03','04','05','06','07','08','09','10','11','12')


# read in dbf file
dbffile <- ("F:/FIA_plot_locations_multconds/FIA_plot_locations_multconds.dbf")
poly<-shapefile("F:/FIA_plot_locations_multconds/FIA_plot_locations_multconds.shp")
poly.dbf <- read.dbf(dbffile)

# PRCP DATA extract raster data from overlying polygon
ppt_data<-vector()
for(j in 1:length(months)){
  raster<-paste(climdir,'/ppt/PRISM_ppt_30yr_normal_800mM2_',months[j],'_bil.bil',sep="")
  rast <- raster(raster)
  ext.poly <- raster::extract(rast, poly, fun = mean, na.rm=TRUE, df=TRUE)
  ppt<-ext.poly[,2]
  CN<-poly.dbf[,2]
  month <- rep(months[j],length(ext.poly[,2]))
  ppt_CN<-cbind(CN,ppt,month)
  ppt_data<-rbind(ppt_data,ppt_CN)}

# TMAX DATA extract raster data from overlying polygon                     
tmax_data<-vector()
for(j in 1:length(months)){
  raster<-paste(climdir,'/tmax/PRISM_tmax_30yr_normal_800mM2_',months[j],'_bil.bil',sep="")
  rast <- raster(raster)
  ext.poly <- raster::extract(rast, poly, fun = mean, na.rm=TRUE, df=TRUE)
  tmax<-ext.poly[,2]
  CN<-poly.dbf[,2]
  month <- rep(months[j],length(ext.poly[,2]))
  tmax_CN<-cbind(CN,tmax,month)
  tmax_data<-rbind(tmax_data,tmax_CN)}

# TMIN DATA extract raster data from overlying polygon
tmin_data<-vector()
for(j in 1:length(months)){
  raster<-paste(climdir,'/tmin/PRISM_tmin_30yr_normal_800mM2_',months[j],'_bil.bil',sep="")
  rast <- raster(raster)
  ext.poly <- raster::extract(rast, poly, fun = mean, na.rm=TRUE, df=TRUE)
  tmin<-ext.poly[,2]
  CN<-poly.dbf[,2]
  month <- rep(months[j],length(ext.poly[,2]))
  tmin_CN<-cbind(CN,tmin,month)
  tmin_data<-rbind(tmin_data,tmin_CN)}

# TMEAN DATA extract raster data from overlying polygon
tmean_data<-vector()
for(j in 1:length(months)){
  raster<-paste(climdir,'/tmean/PRISM_tmean_30yr_normal_800mM2_',months[j],'_bil.bil',sep="")
  rast <- raster(raster)
  ext.poly <- raster::extract(rast, poly, fun = mean, na.rm=TRUE, df=TRUE)
  tmean<-ext.poly[,2]
  CN<-poly.dbf[,2]
  month <- rep(months[j],length(ext.poly[,2]))
  tmean_CN<-cbind(CN,tmean,month)
  tmean_data<-rbind(tmean_data,tmean_CN)}

# VPD DATA extract raster data from overlying polygon
vpdmax_data<-vector()
for(j in 1:length(months)){
  raster<-paste(climdir,'/vpdmax/PRISM_vpdmax_30yr_normal_800mM2_',months[j],'_bil.bil',sep="")
  rast <- raster(raster)
  ext.poly <- raster::extract(rast, poly, fun = mean, na.rm=TRUE, df=TRUE)
  vpdmax<-ext.poly[,2]
  CN<-poly.dbf[,2]
  month <- rep(months[j],length(ext.poly[,2]))
  vpdmax_CN<-cbind(CN,vpdmax,month)
  vpdmax_data<-rbind(vpdmax_data,vpdmax_CN)}

vpdmin_data<-vector()
for(j in 1:length(months)){
  raster<-paste(climdir,'/vpdmin/PRISM_vpdmin_30yr_normal_800mM2_',months[j],'_bil.bil',sep="")
  rast <- raster(raster)
  ext.poly <- raster::extract(rast, poly, fun = mean, na.rm=TRUE, df=TRUE)
  vpdmin<-ext.poly[,2]
  CN<-poly.dbf[,2]
  month <- rep(months[j],length(ext.poly[,2]))
  vpdmin_CN<-cbind(CN,vpdmin,month)
  vpdmin_data<-rbind(vpdmin_data,vpdmin_CN)}

## COMBINE ALL OF THE CLIMATE DATA TOGETHER #####
climate_data<-cbind(tmax_data,tmin_data, tmean_data, ppt_data, vpdmax_data, vpdmin_data)

####MAKE SURE THAT THE CLIMATE VARIABLES ARE ALL LINED UP WELL #####
all(climate_data[,1]==climate_data[,4])
all(climate_data[,1]==climate_data[,7])
all(climate_data[,10]==climate_data[,13])
all(climate_data[,10]==climate_data[,16])
all(climate_data[,3]==climate_data[,6])
all(climate_data[,6]==climate_data[,9])
all(climate_data[,9]==climate_data[,12])
all(climate_data[,12]==climate_data[,15])
all(climate_data[,15]==climate_data[,18])

####make climate data into a dataframe and then remove duplicate columns ####

climate_df<-as.data.frame(climate_data)
climate_df<-cbind(climate_df[,1:3],climate_df[5],climate_df[8],climate_df[11],
                  climate_df[14],climate_df[17])
View(climate_df)

##write out a csv with climate data 
write.csv(climate_df,"FIA_30yrnormal_climate_realcoords_multconds.csv", append=FALSE)
prism_data<- read.csv("F:/FIA_30yrnormal_climate_realcoords_multconds.csv")

### extract bioclimate variables from ClimateWNA
install.packages("arcgisbinding", repos="https://r.esri.com", type="win.binary")
library(arcgisbinding)
arc.check_product()

##list out variables I want to extract
vars<- c("mcmt",	"mwmt",	"td",	"msp",	"ahm",	"shm",	"dd_0",	"DD5",
         "nffd",	"bffp",	"effp",	"pas",	"emt",	"eref",	"cmd",	"tave_wt",
         "tave_sm",	"ppt_wt",	"ppt_sm")


bioclim_vars<-as.data.frame(poly.dbf)
for(i in 1:length(vars)){
  rast <- as.raster(arc.raster(arc.open(paste("E:/Bioclimate_Layers/Bioclimate_UTM_Zone12.gdb/",vars[i],sep=""))))
  vals <- raster::extract(rast, poly, fun = mean, na.rm=TRUE, df=TRUE, sp=TRUE)
  vals <- as.data.frame(vals@data) %>% 
    dplyr::rename(!!paste(vars[i]) := Band_1)
  bioclim_vars<- bioclim_vars %>% 
    left_join(vals)
  }
head(bioclim_vars)
View(bioclim_vars)
bioclim_table<- bioclim_vars %>% 
  dplyr::select(-1)
View(bioclim_table)
options(scipen=0)
write.csv(bioclim_table, "C:/Users/Katie/Google Drive/FIA project/FIA_rproject/bioclim_vars_FIA_multconds.csv")
bioclim_table = read.csv("C:/Users/Katie/Google Drive/FIA project/FIA_rproject/bioclim_vars_FIA_multconds.csv")
View(bioclim_table)

####merge all variable tables together
head(prism_data)
prism_ppt<-aggregate(ppt~CN,prism_data,function(x)sum(x))
prism_mat<-aggregate(tmean~CN,prism_data,mean)
prism_miat<-aggregate(tmin~CN,prism_data,min)
prism_maat<-aggregate(tmax~CN,prism_data,max)
prism_vpdmax<-aggregate(vpdmax~CN,prism_data,max)
prism_vpdmin<-aggregate(vpdmin~CN,prism_data,min)
prism_annual_data<- cbind(prism_ppt,prism_mat[,2],prism_miat[,2],
                          prism_maat[,2],prism_vpdmax[,2],prism_vpdmin[,2])
head(prism_annual_data)
prism_annual_data<-as.data.frame(prism_annual_data)
colnames(prism_annual_data)=c("PLT_CN","ppt","tmean","tmin","tmax","vpdmax","vpdmin")



all_vars= bioclim_table %>% 
  left_join(prism_annual_data,by="PLT_CN")
nrow(all_vars)

options(scipen=0)
write.csv(all_vars, "env_vars_FIA_2022.csv")

###############extract disturbance data


## Now extract MTBS fire perimeters that intersect dry domain
## plots. Need MTBS perimeter shapefile downloaded

##spatial join - dry domain plot data with MTBS fire perims
#load packages

points = readOGR(dsn="F:/FIA_plot_locations_multconds", layer = "FIA_plot_locations_multconds")
polys = readOGR(dsn="C:/Users/katie/My Drive/ArcGIS/mtbs_perimeter_data", layer ="mtbs_perims_DD")
polys = spTransform(polys,CRS("+proj=longlat +datum=NAD83 +no_defs"))

polys@data$poly.ids <- 1:nrow(polys)
join <- point.in.poly(points, polys, sp=FALSE, duplicate=FALSE)
jointable <- as.data.frame(join@data)
setwd("C:/Users/katie/My Drive/FIA project/FIA_rproject")
write.csv(jointable, "mtbs_join_2022.csv")
write.csv(polys@data, "mtbs_data_2022.csv")

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

agent_table_stacked_mtbs<- joindata %>% 
  dplyr::select(PLT_CN,year1,year2,year3,year4,year5,year6)
agent_table_stacked_mtbs<-as.data.table(agent_table_stacked_mtbs)
agent_table_stacked_mtbs<- melt(agent_table_stacked_mtbs, id.vars = c("PLT_CN"),
                                measure.vars = patterns("^year"),
                                variable.name = "variable",
                                value.name = c("dist_year"))
agent_table_stacked_mtbs$Agent <- rep("fire", nrow(agent_table_stacked_mtbs))
agent_table_stacked_mtbs$source <- rep("mtbs", nrow(agent_table_stacked_mtbs))
agent_table_stacked_mtbs<-agent_table_stacked_mtbs[,c(1,2,4,3,5)]
agent_table_stacked_mtbs<-agent_table_stacked_mtbs[! agent_table_stacked_mtbs$dist_year=="NA", ]

#write csv for mtbs table
setwd("C:/Users/Katie/Google Drive/FIA project/FIA_rproject")
write.csv(agent_table_stacked_mtbs,file="agent_table_stacked_mtbs2022.csv")


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
  ext.points <- extract(rast.list[[j]], plot.pts, df=TRUE)
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
landfire.fin = cbind(landfireHC[,c(2,1,4,3)],rep("landfire",nrow(landfireHC)))
colnames(landfire.fin)=c("PLT_CN","variable","Agent","MEASYEAR","source")
write.csv(landfire.fin, "C:/Users/Katie/Google Drive/FIA project/FIA_rproject/agent_table_stacked_LANDFIRE2022.csv")

#################################
###### Extract disturbance from Condition data
#################################

## looking at species sample size
mtbs.tree<- tree[tree$PLT_CN %in% join.table.fire$PLT_CN, ]
aggregate(PLT_CN~SPCD, data=mtbs.tree, function(x) length(unique(x)))
aggregate(PLT_CN~INVYR, data=mtbs.tree, function(x) length(unique(x)))

##### create a vector with CN's of all plots with FIA fire history 
##subset condition data to dry domain plots only 
nrow(unique_plots)
ddcond <- cond[cond$PLT_CN %in% unique_plots$CN,]
length(unique(ddcond$PLT_CN))

##create table with just disturbance codes
ddcond.dist<- ddcond %>% 
  dplyr::select("PLT_CN","DSTRBCD1","DSTRBCD2","DSTRBCD3",
                "TRTCD1","TRTCD2","TRTCD3","DSTRBYR1","DSTRBYR2","DSTRBYR3","TRTYR1",
                "TRTYR2","TRTYR3","DSTRBCD1_P2A","DSTRBCD2_P2A","DSTRBCD3_P2A",
                "DSTRBYR1_P2A","DSTRBYR2_P2A","DSTRBYR3_P2A","TRTCD1_P2A","TRTCD2_P2A",
                "TRTCD3_P2A","TRTYR1_P2A","TRTYR2_P2A","TRTYR3_P2A")%>% 
  filter_at(vars(DSTRBCD1:DSTRBYR3_P2A), any_vars(.>0)) %>% 
  left_join(plot, by=c("PLT_CN"="CN")) %>% 
  dplyr::select("PLT_CN","DSTRBCD1","DSTRBCD2","DSTRBCD3",
                "TRTCD1","TRTCD2","TRTCD3","DSTRBYR1","DSTRBYR2","DSTRBYR3","TRTYR1",
                "TRTYR2","TRTYR3","DSTRBCD1_P2A","DSTRBCD2_P2A","DSTRBCD3_P2A",
                "DSTRBYR1_P2A","DSTRBYR2_P2A","DSTRBYR3_P2A","TRTCD1_P2A","TRTCD2_P2A",
                "TRTCD3_P2A","TRTYR1_P2A","TRTYR2_P2A","TRTYR3_P2A","MEASYEAR")

head(ddcond.dist)
aggregate(PLT_CN~DSTRBCD1+DSTRBCD1_P2A,data=ddcond.dist, length)

##make same table for plots measured in the previous cycle
ddcond.prev<- cond[cond$PLT_CN %in% unique_plots$PREV_PLT_CN, ]
ddcond.dist.prev<- ddcond.prev %>% 
  dplyr::select("PLT_CN","CONDPROP_UNADJ","DSTRBCD1","DSTRBCD2","DSTRBCD3",
                "TRTCD1","TRTCD2","TRTCD3","DSTRBYR1","DSTRBYR2","DSTRBYR3","TRTYR1",
                "TRTYR2","TRTYR3","DSTRBCD1_P2A","DSTRBCD2_P2A","DSTRBCD3_P2A",
                "DSTRBYR1_P2A","DSTRBYR2_P2A","DSTRBYR3_P2A") %>% 
  left_join(plot, by=c("PLT_CN"="CN")) %>% 
  dplyr::select("PLT_CN","CONDPROP_UNADJ","DSTRBCD1","DSTRBCD2","DSTRBCD3",
                "TRTCD1","TRTCD2","TRTCD3","DSTRBYR1","DSTRBYR2","DSTRBYR3","TRTYR1",
                "TRTYR2","TRTYR3","DSTRBCD1_P2A","DSTRBCD2_P2A","DSTRBCD3_P2A",
                "DSTRBYR1_P2A","DSTRBYR2_P2A","DSTRBYR3_P2A","MEASYEAR") %>% 
  filter_at(vars(DSTRBCD1:DSTRBYR3_P2A), any_vars(.>0)) %>% 
  dplyr::rename("prev_PLT_CN"="PLT_CN") %>% 
  left_join(plot, by=c("prev_PLT_CN"="PREV_PLT_CN")) %>% 
  dplyr::select("CN","DSTRBCD1","DSTRBCD2","DSTRBCD3",
                "TRTCD1","TRTCD2","TRTCD3","DSTRBYR1","DSTRBYR2","DSTRBYR3","TRTYR1",
                "TRTYR2","TRTYR3","DSTRBCD1_P2A","DSTRBCD2_P2A","DSTRBCD3_P2A",
                "DSTRBYR1_P2A","DSTRBYR2_P2A","DSTRBYR3_P2A","MEASYEAR.x") %>% 
  dplyr::rename("PLT_CN"="CN","MEASYEAR"="MEASYEAR.x")


##make same table for plots in two previous measurement periods before
unique_plots.prev <- plot[plot$CN %in% unique_plots$PREV_PLT_CN, ]
unique(unique_plots.prev$PREV_PLT_CN) ##there are none

##functions for recoding disturbance and treatment codes to names
dist.recode.fun<- function(DSTRBCD){
  ifelse(DSTRBCD %in% 10:22,"insect.disease",
         ifelse(DSTRBCD %in% 30:32,"fire",
                ifelse(DSTRBCD == 52, "wind","other")))
}

trt.recode.fun<- function(TRTCD){
  ifelse(TRTCD %in% c(10,20,50),"harvest",
         ifelse(TRTCD == 30,"art.regen", NA))
}

##combine most recent and previous plot condition data
cond.dist.table <- bind_rows(ddcond.dist, ddcond.dist.prev) %>% 
  mutate_at(.vars=c("DSTRBCD1","DSTRBCD2","DSTRBCD3","DSTRBCD1_P2A", "DSTRBCD2_P2A", "DSTRBCD3_P2A"), .funs=dist.recode.fun) %>% 
  mutate_at(.vars=c("TRTCD1", "TRTCD2", "TRTCD3", "TRTCD1_P2A", "TRTCD2_P2A","TRTCD3_P2A"), .funs=trt.recode.fun) 

cond.dist.table.long <- bind_rows(
  cond.dist.table %>% 
    dplyr::select(PLT_CN,DSTRBCD1,DSTRBYR1,MEASYEAR) %>% 
    dplyr::rename(Agent = DSTRBCD1, dist_year = DSTRBYR1),
  
  cond.dist.table %>% 
    dplyr::select(PLT_CN,DSTRBCD2,DSTRBYR2,MEASYEAR) %>% 
    dplyr::rename(Agent = DSTRBCD2, dist_year = DSTRBYR2),
  
  cond.dist.table %>% 
    dplyr::select(PLT_CN,DSTRBCD3,DSTRBYR3,MEASYEAR) %>% 
    dplyr::rename(Agent = DSTRBCD3, dist_year = DSTRBYR3),
  
  cond.dist.table %>% 
    dplyr::select(PLT_CN,DSTRBCD1_P2A,DSTRBYR1_P2A,MEASYEAR) %>% 
    dplyr::rename(Agent = DSTRBCD1_P2A, dist_year = DSTRBYR1_P2A),
  
  cond.dist.table %>% 
    dplyr::select(PLT_CN,DSTRBCD2_P2A,DSTRBYR2_P2A,MEASYEAR) %>% 
    dplyr::rename(Agent = DSTRBCD2_P2A, dist_year = DSTRBYR2_P2A),
  
  cond.dist.table %>% 
    dplyr::select(PLT_CN,DSTRBCD3_P2A,DSTRBYR3_P2A,MEASYEAR) %>% 
    dplyr::rename(Agent = DSTRBCD3_P2A, dist_year = DSTRBYR3_P2A),
  
  cond.dist.table %>% 
    dplyr::select(PLT_CN,TRTCD1,TRTYR1,MEASYEAR) %>% 
    dplyr::rename(Agent = TRTCD1, dist_year = TRTYR1),
  
  cond.dist.table %>% 
    dplyr::select(PLT_CN,TRTCD2,TRTYR2,MEASYEAR) %>% 
    dplyr::rename(Agent = TRTCD2, dist_year = TRTYR2),
  
  cond.dist.table %>% 
    dplyr::select(PLT_CN,TRTCD3,TRTYR3,MEASYEAR) %>% 
    dplyr::rename(Agent = TRTCD3, dist_year = TRTYR3),
  
  cond.dist.table %>% 
    dplyr::select(PLT_CN,TRTCD1_P2A,TRTYR1_P2A,MEASYEAR) %>% 
    dplyr::rename(Agent = TRTCD1_P2A, dist_year = TRTYR1_P2A),
  
  cond.dist.table %>% 
    dplyr::select(PLT_CN,TRTCD2_P2A,TRTYR2_P2A,MEASYEAR) %>% 
    dplyr::rename(Agent = TRTCD2_P2A, dist_year = TRTYR2_P2A),
  
  cond.dist.table %>% 
    dplyr::select(PLT_CN,TRTCD3_P2A,TRTYR3_P2A,MEASYEAR) %>% 
    dplyr::rename(Agent = TRTCD3_P2A, dist_year = TRTYR3_P2A)
) %>%  ##this appends the prev plot CN and year of previous plot measurement if applicable.
  left_join(plot,by=c("PLT_CN"="CN")) %>% 
  dplyr::select("PLT_CN","Agent","dist_year","MEASYEAR.x","PREV_PLT_CN") %>% 
  dplyr::rename("MEASYEAR"="MEASYEAR.x") %>% 
  left_join(plot,by=c("PREV_PLT_CN"="CN")) %>% 
  dplyr::select("PLT_CN","Agent","dist_year","MEASYEAR.x","PREV_PLT_CN","MEASYEAR.y") %>% 
  dplyr::rename("MEASYEAR"="MEASYEAR.x","PREV_MEASYEAR"="MEASYEAR.y") %>% 
  filter(!is.na(Agent)) %>%  #get rid of plots with no disturbances 
  #fill in missing disturbance years. According to FIA manual, disturbances were noted on plots if they had been disturbed since the previous
  #plot measurement or within the past 5 years, unless the disturbance was a harvest activity, then within the last 20 years
  #so I set the disturbance year as the earliest possible year according to these rules.
  #If the disturbance year was recorded as 9999, i just used the measurement year as the disturbance year since this means the disturbance is happening over many years
  mutate(dist_year = ifelse(is.na(dist_year)& is.na(PREV_PLT_CN) & Agent %in% c("fire","other","insect.disease","wind"),MEASYEAR - 5,dist_year)) %>% 
  mutate(dist_year = ifelse(is.na(dist_year)& is.na(PREV_PLT_CN) & Agent %in% c("harvest","art.regen"),MEASYEAR - 20,dist_year)) %>% 
  mutate(dist_year = ifelse(is.na(dist_year)& ! is.na(PREV_PLT_CN) & Agent %in% c("fire","other","insect.disease","wind"),PREV_MEASYEAR + 1,dist_year)) %>% 
  mutate(dist_year = ifelse(is.na(dist_year)& ! is.na(PREV_PLT_CN) & Agent %in% c("harvest","art.regen"),PREV_MEASYEAR + 1,dist_year)) %>% 
  mutate(dist_year = ifelse(dist_year == 9999,MEASYEAR,dist_year)) %>% 
  distinct()

#write table to csv
write.csv(cond.dist.table.long, "C:/Users/Katie/Google Drive/FIA project/FIA_rproject/dist_table_stacked_cond2022.csv")

########################
#####extract disturbance from tree table
########################

ddtree <- tree[tree$PLT_CN %in% unique_plots$CN,]
ddtree.prev <- tree[tree$PLT_CN %in% unique_plots$PREV_PLT_CN,]

#disturbance codes 
insect = c(10:19)
disease = c(20:29)
fire = c(30:39)
harvest = c(80:89)

tree.list <- list(ddtree, ddtree.prev)
agent.list <- list(insect, disease, fire, harvest)

#number of plots containing each disturbance code 
ddtree %>% 
  filter(STATUSCD %in% c(2,3)) %>%
  filter(AGENTCD %in% unlist(agent.list)) %>% 
  group_by(AGENTCD) %>% 
  dplyr::summarise(n = length(unique(PLT_CN)))

recent.plots.dist.table <- ddtree %>% 
  filter(STATUSCD %in% c(2,3)) %>%
  filter(AGENTCD %in% unlist(agent.list)) %>% 
  mutate(Agent = ifelse(AGENTCD %in% c(10:29),'insect.disease',
                        ifelse(AGENTCD %in% c(30:39), 'fire',
                               ifelse(AGENTCD %in% c(80:89),'harvest','other')))) %>% 
  group_by(PLT_CN, Agent) %>% 
  dplyr::summarise(num.trees = length(unique(CN))) %>% 
  filter(num.trees > 4) %>% 
  left_join(plot,by=c("PLT_CN"="CN")) %>% 
  dplyr::select(PLT_CN, Agent, num.trees, MEASYEAR, PREV_PLT_CN) %>% 
  dplyr::rename(dist_year = MEASYEAR)

prev.plots.dist.table <- ddtree.prev %>% 
  filter(STATUSCD %in% c(2,3)) %>%
  filter(AGENTCD %in% unlist(agent.list)) %>% 
  mutate(Agent = ifelse(AGENTCD %in% c(10:29),'insect.disease',
                        ifelse(AGENTCD %in% c(30:39), 'fire',
                               ifelse(AGENTCD %in% c(80:89),'harvest','other')))) %>% 
  group_by(PLT_CN, Agent) %>% 
  dplyr::summarise(num.trees = length(unique(CN))) %>% 
  filter(num.trees > 4) %>% 
  left_join(plot,by=c("PLT_CN"="CN")) %>% 
  dplyr::select(PLT_CN, Agent, num.trees, MEASYEAR) %>% 
  dplyr::rename(PREV_PLT_CN = PLT_CN, dist_year = MEASYEAR) %>% 
  left_join(plot, by=c("PREV_PLT_CN")) %>% 
  ungroup() %>% 
  dplyr::select(CN, Agent, num.trees,dist_year, PREV_PLT_CN) %>% 
  dplyr::rename(PLT_CN = CN)

tree.dist.table <- bind_rows(recent.plots.dist.table,prev.plots.dist.table) %>% 
  mutate(source = "tree_table")


#write table to csv so it can be combined with other sources 
write.csv(tree.dist.table, file="agent_table_stacked_tree2022.csv")

#################
###### Now put all the disturbance tables together
#################

#MAKE MASTER DATA TABLE AND SAVE TO CSV
##updated 1/24/20 for new FIA plots and real coordinates
##updated 10/28/21 to exclude plots measured in the same year as disturbance
##updated 11/1/21 for use with new disturbance extractions from tree and condition databases
##updated 3/14/22 for new multi-condition plots
#--------------------------------------------------------
##combine all previously made tables into one
cond.dist = read.csv("C:/Users/Katie/Google Drive/FIA project/FIA_rproject/dist_table_stacked_cond2022.csv")
mtbs.dist = read.csv("C:/Users/Katie/Google Drive/FIA project/FIA_rproject/agent_table_stacked_mtbs2022.csv")
tree.dist = read.csv("C:/Users/Katie/Google Drive/FIA project/FIA_rproject/agent_table_stacked_tree2022.csv")
landfire.dist = read.csv("C:/Users/Katie/Google Drive/FIA project/FIA_rproject/agent_table_stacked_LANDFIRE2022.csv")

colnames(mtbs.dist)
colnames(tree.dist)
colnames(cond.dist)
colnames(landfire.dist)

mtbs.dist=mtbs.dist[c("PLT_CN","Agent","dist_year","source")]
tree.dist=tree.dist[c("PLT_CN","Agent","dist_year","source")]
landfire.dist<- landfire.dist %>% 
  dplyr::rename(dist_year = MEASYEAR)
landfire.dist=landfire.dist[c("PLT_CN","Agent","dist_year","source")]
cond.dist$source = "cond"
cond.dist=cond.dist[c("PLT_CN","Agent","dist_year","source")]

ncol(mtbs.dist)
ncol(tree.dist)
ncol(cond.dist)
ncol(landfire.dist)

nrow(mtbs.dist)+
  nrow(tree.dist)+
  nrow(cond.dist)+
  nrow(landfire.dist)

length(unique(c(mtbs.dist$PLT_CN,tree.dist$PLT_CN,cond.dist$PLT_CN,landfire.dist$PLT_CN)))

#recode LANDFIRE disturbance types to match other databases
landfire.dist$Agent=as.character(landfire.dist$Agent)
landfire.dist$Agent[landfire.dist$Agent == "Insects" 
                    |landfire.dist$Agent == "Insects/Disease" ] <- "insect.disease"
landfire.dist$Agent[landfire.dist$Agent == "Wildfire" |
                      landfire.dist$Agent == "Prescribed Fire" |
                      landfire.dist$Agent == "Wildland Fire Use"|
                      landfire.dist$Agent == "Wildland Fire"]<- "fire"
landfire.dist$Agent[landfire.dist$Agent == "Clearcut"|
                      landfire.dist$Agent == "Harvest"|
                      landfire.dist$Agent == "Thinning"|
                      landfire.dist$Agent == "Other Mechanical"|
                      landfire.dist$Agent == "Mastication"]<- "harvest"
landfire.dist$Agent[landfire.dist$Agent == "Chemical"|
                      landfire.dist$Agent == "Herbicide"|
                      landfire.dist$Agent == "Unknown"|
                      landfire.dist$Agent == "Insecticide"|
                      landfire.dist$Agent == "Development"]<- "other"
landfire.dist$Agent[landfire.dist$Agent == "Weather"]<- "wind"

master.agent.table = rbindlist(list(mtbs.dist,tree.dist,cond.dist,landfire.dist),
                               use.names = TRUE)
head(master.agent.table)
art.regen.plots = master.agent.table[master.agent.table$Agent == "art.regen", ]$PLT_CN
#there are no plots with artifical regen

View(master.agent.table)

#write.csv(master.agent.table, "master_agent_table2022.csv")

master.agent.table.new <- master.agent.table %>% 
  left_join(plot, by=c("PLT_CN"="CN")) %>% 
  dplyr::select("PLT_CN","Agent","dist_year","source","MEASYEAR")

unique.plots.master = unique(master.agent.table.new$PLT_CN)
Result = matrix(ncol=2, nrow=length(unique.plots.master))
colnames(Result) = c("PLT_CN", "Fire.TF")

## pull plots where disturbance happened within 10 years of measurement 
## record plot CN and whether the disturbance was fire (1) or not (0)
for(i in 1: length(unique.plots.master)){
  s = master.agent.table.new[master.agent.table.new$PLT_CN == unique.plots.master[i] & 
                               master.agent.table.new$dist_year > master.agent.table.new$MEASYEAR - 11 &
                               master.agent.table.new$dist_year <= master.agent.table.new$MEASYEAR, ]
  t = 'fire' %in% s$Agent
  r = c(unique(s$PLT_CN), t)
  Result[i, ] = r
}

## limit table to just plots that experienced fire in the 10 years prior to measurement
fire.plots = as.data.table(Result[Result[,"Fire.TF"]==1,])
head(fire.plots)

## make table with disturbance info for plots that experienced fire in the past ten years. 
## Only including dist year == MEASYEAR if data came from the tree table. 
fire.plot.table <- master.agent.table.new %>% 
  filter(PLT_CN %in% fire.plots$PLT_CN) %>% 
  filter(Agent == 'fire') %>% 
  filter(MEASYEAR >= dist_year) %>% 
  filter(!(!source=='tree_table' & dist_year==MEASYEAR)) %>% 
  group_by(PLT_CN) %>% 
  arrange(desc(dist_year)) %>% 
  filter(row_number()==1)


#exlude fire plots from table
No.fire.agent.table = master.agent.table.new[! master.agent.table.new$PLT_CN
                                             %in% fire.plot.table$PLT_CN, ]
#exclude disturbances that occurred after measurement
No.fire.agent.table <- No.fire.agent.table %>% 
  filter(! dist_year > MEASYEAR )
#only exclude fires/harvests that occurred in the year of disturbance, except from the tree table source 
No.fire.agent.table <- No.fire.agent.table %>%
  filter(!(Agent %in% c('fire','harvest') & dist_year == MEASYEAR & !source == 'tree_table'))
#exclude plots disturbed >30 yrs prior to measurement
No.fire.agent.table = No.fire.agent.table[No.fire.agent.table$dist_year
                                          >= No.fire.agent.table$MEASYEAR - 30, ]

## find most recent disturbance for each plot
M = matrix(ncol=ncol(No.fire.agent.table), nrow=0)
colnames(M) = colnames(No.fire.agent.table)

for(i in 1:length(unique(No.fire.agent.table$PLT_CN))){
  new = No.fire.agent.table[No.fire.agent.table$PLT_CN == 
                              unique(No.fire.agent.table$PLT_CN)[i], ]
  max.yr = new[new$dist_year == max(new$dist_year), ]
  M = rbind(M, max.yr)
}
#recode to order disturbances in most - least important
M$Agent[M$Agent == "fire"]<- "1"
M$Agent[M$Agent == "harvest"]<- "2"
M$Agent[M$Agent == "wind"]<- "3"
M$Agent[M$Agent == "insect.disease"]<- "4"
M$Agent[M$Agent == "other"]<- "5"

#only keep most important disturbance 
M = as.data.frame(M)
M = M[order(M[,'PLT_CN'], M[,'Agent']), ]
M = M[!duplicated(M$PLT_CN),]

#checking wind plots
aggregate(PLT_CN~Agent, data=M, length)

#recode back to disturbance names
M$Agent[M$Agent == "1"]<- "fire"
M$Agent[M$Agent == "2"]<- "harvest"
M$Agent[M$Agent == "3"]<- "wind"
M$Agent[M$Agent == "4"]<- "insect.disease"
M$Agent[M$Agent == "5"]<- "other"



fire.plots2 = rbind(M[M$Agent == 'fire', ],fire.plot.table)
harvest.plots = M[M$Agent == 'harvest', ]
wind.plots = M[M$Agent == 'wind', ]
insect.disease.plots = M[M$Agent == 'insect.disease', ]
other.plots = M[M$Agent == 'other', ]
all.dist.plots =  rbind(fire.plots2, harvest.plots, wind.plots, insect.disease.plots, other.plots)
undisturbed = unique_plots[which(! unique_plots$CN %in% all.dist.plots$PLT_CN),c(2,13)]
colnames(undisturbed)=c("PLT_CN","MEASYEAR")
undisturbed$Agent = "none"
undisturbed$dist_year = NA
undisturbed$source = NA
undisturbed=undisturbed[c(1,3,4,5,2)]

write.csv(fire.plots2, "fire.plots2022.csv")
write.csv(harvest.plots, "harvest.plots2022.csv")
write.csv(wind.plots, "wind.plots2022.csv")
write.csv(insect.disease.plots, "insect.disease.plots2022.csv")
write.csv(other.plots, "other.plots2022.csv")
write.csv(undisturbed, "undisturbed.plots2022.csv")
write.csv(all.dist.plots, "all.dist.plots2022.csv")


###exploring how many disturbances each plot has recorded
agent.number <- master.agent.table.new %>% 
  filter(MEASYEAR >= dist_year) %>% 
  group_by(PLT_CN) %>% 
  dplyr::summarise(num.agents = length(unique(Agent))) 

####exploring disturbance plots
all.dist.plots %>% 
  left_join(agent.number) %>% 
  ggplot(aes(x=num.agents))+
  geom_histogram()+
  facet_wrap(~Agent)

##lets look at insect.disease
all.dist.plots %>% 
  left_join(agent.number) %>% 
  filter(Agent == "insect.disease") %>% 
  left_join(master.agent.table.new, by="PLT_CN") %>% 
  filter(num.agents>1) %>% 
  ggplot(aes(x=Agent.y)) + 
  geom_histogram(stat='count')

##10 year fire plots
#this histogram shows the number of disturbance recorded on the plots that had a fire in the most recent 10 years
#we can see that the majority of plots only had one recorded disturbance (fire), but a good number also had 2
all.dist.plots %>% 
  filter(dist_year >= MEASYEAR - 10) %>% 
  filter(Agent == 'fire') %>% 
  left_join(agent.number) %>% 
  group_by(PLT_CN) %>% 
  ggplot(aes(x=num.agents))+
  geom_histogram()

#this histogram breaks down which disturbances were on plots that had fire in the most recent 10 years, 
#but also had another disturbance (or 2 or 3) recorded on the plot
#we can see that most of the other disturbances were in the "other" or "insect/disease" category which gives me more confidence in 
#calling these fire plots 
all.dist.plots %>% 
  filter(dist_year >= MEASYEAR - 10) %>% 
  filter(Agent == 'fire') %>% 
  left_join(agent.number) %>% 
  group_by(PLT_CN) %>%
  filter(num.agents>1) %>% 
  left_join(master.agent.table.new, by="PLT_CN") %>% 
  group_by(PLT_CN) %>% 
  ggplot(aes(x=Agent.y))+
  geom_histogram(stat='count')







