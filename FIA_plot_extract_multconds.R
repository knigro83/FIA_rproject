## Subsets plots for FIA analysis including plots with multiple conditions
## as long as they don't have multiple owner groups or multiple disturbance conditions

## adapted from FIA_data_extraction_master.R
## created: 3.11.22


#load packages
library(readr)
library(dplyr)
library(ggplot2)

## Subset data into public plots with one condition
#FIA data for AZ, CO, NM, NV, ID, UT, WY, MT

plot<- read_csv("compiled_data_annual2020/annual_plots2020.csv")
cond<- read_csv("compiled_data_annual2020/COND.csv")

tree <- read_csv("compiled_data_annual2020/TREE.csv")
seedling<- read_csv("compiled_data_annual2020/SEEDLING.csv")

####################
#goal = obtain all plots that are completely within public land & accessible (COND_STATUS_CD < 3)
public.plots<- plot %>% 
  dplyr::filter(!CN %in% c(cond %>% 
  dplyr::filter(OWNCD > 40 | COND_STATUS_CD > 2) %>% 
  pull(PLT_CN))
  )

#check that there are no private owners in here
View(cond %>% 
  dplyr::filter(PLT_CN %in% public.plots$CN))
#there aren't
####################

####################
#goal = dplyr::filter public plots down to those that only had one disturbance/treatment condition across all subplots
disturbance.summary<- cond %>% 
  dplyr::filter(PLT_CN %in% public.plots$CN) %>% 
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
  dplyr::filter(! CN %in% c(
disturbance.summary %>% 
  dplyr::filter(if_any(.cols=d1:p2a_t3,.fns = ~.>1)) %>% 
  pull(PLT_CN) #these are plots that have different disturbances in different subplots
)
)

nrow(public.onedist.plots)
######################

#######################
#now do the same thing for the previously sampled plot

plot %>% 
  dplyr::filter(CN %in% plot$PREV_PLT_CN) %>% 
  dplyr::filter(!is.na(PREV_PLT_CN)) #there are no plots survyed more than two times



public.plots2<- public.onedist.plots %>% 
  dplyr::filter(!PREV_PLT_CN %in% c(cond %>% 
                      dplyr::filter(OWNCD > 40 | COND_STATUS_CD > 2) %>% 
                      pull(PLT_CN))
  )
public.onedist.plots2<- public.plots2 %>% 
  dplyr::filter(! PREV_PLT_CN %in% c(
    disturbance.summary %>% 
      dplyr::filter(if_any(.cols=d1:p2a_t3,.fns = ~.>1)) %>% 
      pull(PLT_CN) #these are plots that have different disturbances in different subplots
  )
  )

nrow(public.onedist.plots2)

#######################
#eliminate plots with documented artificial regen or stand origin from artificial regen
no.artregen.plots<- public.onedist.plots2 %>% 
  dplyr::filter(!CN %in% c(cond %>% 
  dplyr::filter(TRTCD1 == 30 | TRTCD2 == 30 | TRTCD3 == 30 |
          TRTCD1_P2A == 30 | TRTCD2_P2A == 30 | TRTCD3_P2A == 30 | 
            STDORGCD ==1 ) %>% 
  pull(PLT_CN) %>% 
    unique())
  )
no.artregen.plots2<- no.artregen.plots %>% 
  dplyr::filter(!PREV_PLT_CN %in% c(cond %>% 
                      dplyr::filter(TRTCD1 == 30 | TRTCD2 == 30 | TRTCD3 == 30 |
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
  dplyr::summarise(n=n(), minyear = min(MEASYEAR), maxyear=max(MEASYEAR)) #no periodic inventory plots here

no.artregen.plots2 %>% 
  dplyr::filter(KINDCD == 2) %>% 
  group_by(STATECD) %>% 
  dplyr::summarise(n=n())

cond %>% 
  dplyr::filter(PLT_CN %in% no.artregen.plots2$CN) %>% 
  group_by(PLT_CN) %>% 
  dplyr::summarise(conds = n_distinct(CONDID)) %>% 
  group_by(conds) %>% 
  dplyr::summarise(n=n())

##reduce to just the unique plot locations
unique_plots<- no.artregen.plots2 %>% 
  dplyr::filter(!CN %in% no.artregen.plots2$PREV_PLT_CN) #64,286 plot locations


#####checking out plots more
sumtable<- unique_plots %>% 
  left_join(cond, by=c("CN" = "PLT_CN")) %>% 
  left_join(tree, by=c("CN" = "PLT_CN")) %>% 
  group_by(COND_STATUS_CD,NF_SAMPLING_STATUS_CD,NF_PLOT_STATUS_CD,
           NF_PLOT_NONSAMPLE_REASN_CD,SAMP_METHOD_CD,SUBP_EXAMINE_CD) %>% 
  dplyr::summarise(n_plots=length(unique(CN)),minyear=min(MEASYEAR),maxyear=max(MEASYEAR),
            states=paste(list(unique(STATECD.x))),trees= paste(list(unique(SPCD))))

#write.csv(sumtable, "FIA_plots_sumtable.csv")

unique_plots %>% 
  left_join(cond, by=c("CN" = "PLT_CN")) %>% 
  left_join(tree, by=c("CN" = "PLT_CN")) %>% 
  dplyr::filter((COND_STATUS_CD == 2 & NF_SAMPLING_STATUS_CD==1 & is.na(NF_PLOT_STATUS_CD))|
           (COND_STATUS_CD == 2 & NF_SAMPLING_STATUS_CD==1 & NF_PLOT_STATUS_CD %in% c(1,2))|
           (COND_STATUS_CD == 2 & is.na(NF_SAMPLING_STATUS_CD) & is.na(NF_PLOT_STATUS_CD))) %>% 
  group_by(STATECD.x) %>% 
  dplyr::summarise(n=length(unique(CN)),minyear=min(MEASYEAR),maxyear=max(MEASYEAR))

View(unique_plots %>% 
  left_join(cond, by=c("CN" = "PLT_CN")) %>% 
  left_join(tree, by=c("CN" = "PLT_CN")) %>% 
  dplyr::filter(COND_STATUS_CD==2,NF_SAMPLING_STATUS_CD==0,is.na(NF_PLOT_STATUS_CD),
           is.na(NF_PLOT_NONSAMPLE_REASN_CD),SAMP_METHOD_CD==1,SUBP_EXAMINE_CD==4) %>% 
  group_by(CN) %>% 
  dplyr::summarise(nconds = length(unique(CONDID.x)),trees= paste(list(unique(SPCD)))))

unique_plots %>% 
       left_join(cond, by=c("CN" = "PLT_CN")) %>% 
       left_join(tree, by=c("CN" = "PLT_CN")) %>% 
       dplyr::filter(COND_STATUS_CD==2,NF_SAMPLING_STATUS_CD==0,is.na(NF_PLOT_STATUS_CD),
              is.na(NF_PLOT_NONSAMPLE_REASN_CD),SAMP_METHOD_CD==1,SUBP_EXAMINE_CD==4) %>% 
       group_by(INTENSITY) %>% 
       dplyr::summarise(n=n())

plot.locs <- unique_plots %>% 
  left_join(cond, by=c("CN" = "PLT_CN")) %>% 
  left_join(tree, by=c("CN" = "PLT_CN")) %>% 
  dplyr::filter((COND_STATUS_CD == 2 & NF_SAMPLING_STATUS_CD==1 & is.na(NF_PLOT_STATUS_CD))|
           (COND_STATUS_CD == 2 & NF_SAMPLING_STATUS_CD==1 & NF_PLOT_STATUS_CD %in% c(1,2))|
           (COND_STATUS_CD == 2 & is.na(NF_SAMPLING_STATUS_CD) & is.na(NF_PLOT_STATUS_CD))) %>% 
  distinct(LAT, LON)
  
ggplot(plot.locs, aes(LON, LAT))+
  geom_point(size=.25, show.legend=FALSE)+
  coord_quickmap()

unique_plots %>% 
  left_join(cond, by=c("CN" = "PLT_CN")) %>% 
  group_by(CN) %>% 
  dplyr::summarise(n_conds=n_distinct(COND_STATUS_CD)) %>% 
  group_by(n_conds) %>% 
  dplyr::summarise(n=n())

unique_plots %>% 
  left_join(cond, by=c("CN" = "PLT_CN")) %>% 
  group_by(CN) %>% 
  dplyr::summarise(n_codes=n_distinct(SUBP_EXAMINE_CD)) %>% 
  group_by(n_codes) %>% 
  dplyr::summarise(n=n())

unique_plots %>% 
  left_join(cond, by=c("CN" = "PLT_CN")) %>% 
  dplyr::filter(!(COND_STATUS_CD == 2 & NF_SAMPLING_STATUS_CD ==0)) %>% 
  dplyr::filter(!(COND_STATUS_CD == 2 & NF_PLOT_STATUS_CD == 3)) %>% 
  pull(CN) %>% 
  unique() %>% 
  length()

unique_plots %>% 
  left_join(cond, by=c("CN" = "PLT_CN")) %>% 
  dplyr::filter(!(COND_STATUS_CD == 2 & NF_SAMPLING_STATUS_CD ==0)) %>% 
  dplyr::filter(!(COND_STATUS_CD == 2 & NF_PLOT_STATUS_CD == 3)) %>% 
  group_by(CN) %>% 
  dplyr::summarise(n_conds = n_distinct(COND_STATUS_CD)) %>% 
  dplyr::filter(n_conds > 1)

##STATECDs#
# 4  AZ
# 8  CO
# 16 ID
# 30 MT
# 32 NV
# 35 NM
# 49 UT
# 56 WY

#####

################

#"no.artregen.plots2" is the final plot list for all time points.
#"unique_plots" is the final list for all unique plot locations
#need to grab real coordinates of these plots, 
#check that they are the same between previous plot and recent plot,
#and then extract climate and disturbance data from them. 

#write shapefile for "unique_plots".  
#This is the shapefile for all plots without duplicates

#load packages
library(sp)
library(rgdal)
library(maptools)
library(raster)

coordata<- unique_plots
coordinates(coordata)=~LON+LAT
proj4string(coordata)<- CRS("++proj=longlat +datum=WGS84")
writeOGR(obj=coordata, dsn="C:/Users/Katie/Google Drive/FIA project/spatial data", 
         layer="FIA_plots_2020",driver= "ESRI Shapefile", overwrite_layer = TRUE)


### now extract climate data ###

# set location of climate data
setwd("C:/Users/Katie/Google Drive/FIA project/spatial data")
climdir<-'C:/Users/Katie/Google Drive/ArcGIS/PRISM'

# create list of months of interest

months<-c('01','02','03','04','05','06','07','08','09','10','11','12')

# read in dbf file
dbffile <- "dry_domain_plots_jan.dbf"
poly<-shapefile("dry_domain_plots_jan.shp")
poly.dbf <- read.dbf(dbffile)

# PRCP DATA extract raster data from overlying polygon
ppt_data<-vector()
for(j in 1:length(months)){
  raster<-paste(climdir,'/ppt/PRISM_ppt_30yr_normal_800mM2_',months[j],'_bil.bil',sep="")
  rast <- raster(raster)
  ext.poly <- extract(rast, poly, fun = mean, na.rm=TRUE, df=TRUE)
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
  ext.poly <- extract(rast, poly, fun = mean, na.rm=TRUE, df=TRUE)
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
  ext.poly <- extract(rast, poly, fun = mean, na.rm=TRUE, df=TRUE)
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
  ext.poly <- extract(rast, poly, fun = mean, na.rm=TRUE, df=TRUE)
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
  ext.poly <- extract(rast, poly, fun = mean, na.rm=TRUE, df=TRUE)
  vpdmax<-ext.poly[,2]
  CN<-poly.dbf[,2]
  month <- rep(months[j],length(ext.poly[,2]))
  vpdmax_CN<-cbind(CN,vpdmax,month)
  vpdmax_data<-rbind(vpdmax_data,vpdmax_CN)}

vpdmin_data<-vector()
for(j in 1:length(months)){
  raster<-paste(climdir,'/vpdmin/PRISM_vpdmin_30yr_normal_800mM2_',months[j],'_bil.bil',sep="")
  rast <- raster(raster)
  ext.poly <- extract(rast, poly, fun = mean, na.rm=TRUE, df=TRUE)
  vpdmin<-ext.poly[,2]
  CN<-poly.dbf[,2]
  month <- rep(months[j],length(ext.poly[,2]))
  vpdmin_CN<-cbind(CN,vpdmin,month)
  vpdmin_data<-rbind(vpdmin_data,vpdmin_CN)}

## COMBINE ALL OF THE CLIMATE DATA TOGETHER #####
climate_data<-cbind(tmax_data,tmin_data, tmean_data, ppt_data, vpdmax_data, vpdmin_data)

####MAKE SURE THAT THE CLIMATE VARIABLES ARE ALL LINED UP WELL #####
#all(climate_data[,1]==climate_data[,4])
#all(climate_data[,1]==climate_data[,7])
#all(climate_data[,10]==climate_data[,13])
#all(climate_data[,10]==climate_data[,16])
#all(climate_data[,3]==climate_data[,6])
#all(climate_data[,6]==climate_data[,9])
#all(climate_data[,9]==climate_data[,12])
#all(climate_data[,12]==climate_data[,15])
#all(climate_data[,15]==climate_data[,18])

####make climate data into a dataframe and then remove duplicate columns ####

climate_df<-as.data.frame(climate_data)
climate_df<-cbind(climate_df[,1:3],climate_df[5],climate_df[8],climate_df[11],
                  climate_df[14],climate_df[17])
#View(climate_df)

#### Write the climate data as a csv ####
setwd("C:/Users/Katie/Google Drive/FIA project")
write.csv(climate_df,"FIA_30yrnormal_climate.csv", append=FALSE)

## Now extract MTBS fire perimeters that intersect 
## plots. Need MTBS perimeter shapefile downloaded

##spatial join - dry domain plot data with MTBS fire perims
#load packages
library(units)
library(spatialEco)
library(sp)
library(spatial.tools)

points = readOGR(dsn="C:/Users/Katie/Google Drive/FIA project/spatial data", layer = "dry_domain_plots_2020")
polys = readOGR(dsn="C:/Users/Katie/Google Drive/ArcGIS/mtbs_perimeter_data", layer ="mtbs_perims_DD")
polys = spTransform(polys,CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

polys@data$poly.ids <- 1:nrow(polys)
join <- point.in.poly(points, polys, sp=FALSE, duplicate=FALSE)
jointable <- as.data.table(join@data)
setwd("C:/Users/Katie/Google Drive/FIA project")
write.csv(jointable, "mtbs_join2020.csv")
write.csv(polys@data, "mtbs_data2020.csv")

##Now extract disturbance events from LANDFIRE !!
## first read in rasters and crop to western US
dist.dir <- 'C:/Users/Katie/Google Drive/ArcGIS/US_VegDIST2014_10312018/annual_rasters'

years <- c('1999','2000','2001','2002','2003','2004','2005','2006','2007','2008','2009','2010',
           '2011','2012','2013','2014')

e = extent(-2199337.478017,99686.997269,250725,3180555)

rast.list = c()
for(j in 1:length(years)){
  cat(j, " ")
  raster<-paste(dist.dir,'/us_dist',years[j],sep="")
  rast <- raster(raster)
  rast.crop <- crop(rast, e)
  rast.list <- append(rast.list, rast.crop)
}

##then extract raster info for each FIA plot 

#poly (site points) was loaded in above 

# extract disturbances from each year's raster 
metadatapath <- 'C:/Users/Katie/Google Drive/ArcGIS/US_VegDIST2014_10312018/annual'
dist_data<-vector()
for(j in 1:length(years)){
  ext.points <- extract(rast.list[[j]], poly, df=TRUE)
  dist<-ext.points[,2]
  CN<-poly.dbf[,1]
  year <- rep(years[j],length(ext.points[,2]))
  dist_CN<-cbind(CN,dist,year)
  metadata<- read.csv(paste(metadatapath,'/US_DIST',years[j],'/CSV_Data/US_disturb',years[j],'.csv',sep=""))
  dist_CN_meta <- merge(dist_CN, metadata[,c(1,3:10)], by.x="dist", by.y="Value")
  dist_data<-rbind(dist_data,dist_CN_meta)
  cat(j, " ")}

dist_data_clean = dist_data[complete.cases(dist_data[,4]),]
landfireHC = dist_data_clean[dist_data_clean$Type_Confidence=="High" | dist_data_clean$Type_Confidence=="Medium to High",]
landfire.fin = cbind(landfireHC[,c(3,2,5,4)],rep("landfire",nrow(landfireHC)))
colnames(landfire.fin)=c("PLT_CN","variable","Agent","MEASYEAR","source")
write.csv(landfire.fin, "C:/Users/Katie/Google Drive/FIA project/agent_table_stacked_LANDFIRE.csv")

