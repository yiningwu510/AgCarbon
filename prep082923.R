rm(list = ls())
{ want <- c("tidyverse","magrittr","car","stargazer","readr","dplyr","clubSandwich",
            "multiwayvcov","tidyverse","lmtest","sandwich","rstatix",
            "fastDummies","MASS","readxl","writexl","broom","purrr","miceadds",
            "modelsummary","flextable","plm","data.table","here","ggpubr","psych",
            "ggplot2","writexl","soilDB","odbc","devtools","prism","raster","rasterVis",
            "viridis","profmem","readr","tiff","reproj","proj4","sf","sp","terra",
            "rgdal","pander","gt","car","gridExtra","psych","corrplot","ellipse","dummies",
            "nnet","class","caret","rpart","rpart.plot","ehaGoF","forecast","usmap","maps","mapdata",
            "tigris", "tmaptools", "tmap" ,"sf","aqp", "RODBC","RColorBrewer","miceadds")  
  need <- want[!(want %in% installed.packages()[,"Package"])]
  if (length(need)) install.packages(need)               
  lapply(want, function(i) require(i, character.only=TRUE))
  rm(want, need)
}
setwd("~/Library/CloudStorage/OneDrive-TheOhioStateUniversity/carbon/github/AgCarbon")

###########RaCA data
RaCA_SOC_pedons <- read_csv("data/Raw/RaCA/RaCA_data/RaCA_SOC_pedons.csv")
RaCA_xy <- read_excel("data/Raw/RaCA/RaCA_data//RaCA_data_summary.xlsx", sheet = "RaCA_xy")
RaCA_time <- read_excel("data/Raw/RaCA/RaCA_data/RaCA_data_summary.xlsx", sheet = "RaCA_sample date")


RaCA_xy_time <- merge(RaCA_xy, RaCA_time, by.x = "RaCA_site", by.y = "rcasiteid", all=FALSE)
RaCA_1 <- merge(RaCA_SOC_pedons, RaCA_xy_time, by.x = "rcasiteid", by.y= "RaCA_site", all=FALSE)
RaCA <- RaCA_1[ , ! names(RaCA_1) %in% c("userpedonid")]
rm(RaCA_SOC_pedons,RaCA_time,RaCA_xy,RaCA_xy_time,RaCA_1)

RaCA$year <- format(RaCA$Observation.Date,format="%Y")
RaCA$month <- format(RaCA$Observation.Date,format="%m")

### texture variable
RaCA_samples <- read_excel("data/Raw/RaCA/RaCA_data//RaCA_data_summary.xlsx", 
                           sheet = "RaCA_samples_wmodelBD")
RaCA_samples <- RaCA_samples[RaCA_samples$pedon_no == 1,]

RaCA_texture_1 <-
  RaCA_samples %>% 
  group_by(rcasiteid) %>% 
  filter(row_number()==1)
texture_1 <- RaCA_texture_1[,c("rcasiteid", "texture")]

RaCA_texture_2 <-
  RaCA_samples %>% 
  group_by(rcasiteid) %>% 
  filter(row_number()==2)
texture_2 <- RaCA_texture_2[,c("rcasiteid", "texture")]

setnames(texture_1,old="texture",
         new="texture1")
setnames(texture_2,old="texture",
         new="texture2")

RaCA=RaCA %>% left_join(texture_1)
RaCA=RaCA %>% left_join(texture_2)

RaCA$texture <- ifelse(!is.na(RaCA$texture1), RaCA$texture1,RaCA$texture2)


RaCA$texture_gr <- ifelse(RaCA$texture %in% c("cl","cosl","fsl","l","scl","sicl","sl","vfsl","sil"),"loam",
                          ifelse(RaCA$texture %in% c("c","sc","sic"), "clay", 
                                 ifelse(RaCA$texture %in% c("cos","fs","lcos","lfs","ls","lvfs","s","vfs"), "sand", NA)))

###3 depth seprate SOC
RaCA$SOC0_5 <- RaCA$SOCstock5
RaCA$SOC5_30 <- RaCA$SOCstock30-RaCA$SOCstock5
RaCA$SOC30_100 <- RaCA$SOCstock100-RaCA$SOCstock30

## bulk density
BD <- RaCA_texture_1[,c("rcasiteid", "Bulkdensity")]
RaCA=RaCA %>% left_join(BD)

## map USA
download.file("http://www2.census.gov/geo/tiger/GENZ2022/shp/cb_2022_us_state_20m.zip", destfile = "states.zip")
unzip("states.zip")
us_state<-shapefile("data/Raw/USshapefile/cb_2022_us_state_20m.shp")

#pts <-data.frame(data.frame(x=RaCA$Lon, y=RaCA$Lat))
#coordinates(pts) <- ~x+y
#proj4string(pts) <- CRS("+proj=longlat +datum=NAD83 +no_defs")
#RaCA_state = data.frame(over(pts,us_state))
#RaCA <-cbind(RaCA_state, RaCA)
#RaCA <-RaCA[which(RaCA$NAME %in% c("Illinois", "Indiana", "Iowa", "Michigan", "Minnesota", "Missouri", 
#                                   "Ohio", "Wisconsin","Nebraska", "Kansas", "South Dakota", "North Dakota")),]
#ssurgo <-stack("~/Library/CloudStorage/OneDrive-TheOhioStateUniversity/carbon/Data/gSSURGO/FY2023_gSSURGO_mukey_grid/FY2023_gSSURGO-mukey.tif")
#RaCA.sf <- st_as_sf(RaCA, coords = c("Lon", "Lat"), crs ="+proj=longlat +datum=NAD83 +no_defs")
#ssurgo_value=extract(ssurgo, RaCA.sf)
#RaCA=cbind(RaCA,ssurgo_value)

### divide into 2 years
RaCA_2010 <- RaCA[which(RaCA$year==2010),]
RaCA_2011 <- RaCA[which(RaCA$year==2011),]

############CDL data
##2010
CDL <-stack("data/Raw/CDL/CDLdata/CDL_2008.tif","data/Raw/CDL/CDLdata/CDL_2009.tif","data/Raw/CDL/CDLdata/CDL_2010.tif","data/Raw/CDL/CDLdata/CDL_2011.tif")
RaCA_2010.sf <- st_as_sf(RaCA_2010, coords = c("Lon", "Lat"), crs ="+proj=longlat +datum=NAD83 +no_defs")
CDLvalue_2010=raster::extract(CDL, RaCA_2010.sf)
RaCA_2010_CDL=cbind(RaCA_2010,CDLvalue_2010)

# freq<-apply(RaCA_2010_CDL, 2, table)
# print (freq)
##2011
RaCA_2011.sf <- st_as_sf(RaCA_2011, coords = c("Lon", "Lat"), crs ="+proj=longlat +datum=NAD83 +no_defs")
CDLvalue_2011=raster::extract(CDL, RaCA_2011.sf)
RaCA_2011_CDL=cbind(RaCA_2011,CDLvalue_2011)

rm(CDL,CDLvalue_2010,RaCA_2010,RaCA_2010.sf,CDLvalue_2011,RaCA_2011,RaCA_2011.sf)

## sampling bias (month effect)
# RaCA_2010_CDL<-subset(RaCA_2010_CDL,Lat!=0)
# P2010 <-st_as_sf(RaCA_2010_CDL, coords = c('Lon','Lat'),crs="+proj=longlat +datum=NAD83 +no_defs")
# ggplot(P2010) + 
#   geom_sf(aes(col = month), size=3)
# table(RaCA_2010_CDL$month)
# 
# Aug<-subset(P2010,month=="08")
# Sep<-subset(P2010,month=="09")
# Oct<-subset(P2010,month=="10")
# Nov<-subset(P2010,month=="11")
# Dec<-subset(P2010,month=="12")
# 
# ggplot(P2010) + 
#   geom_sf(aes(col = month), size=3)
# 
# Aug<-ggplot(Aug) + geom_sf(size=1)
# Sep<-ggplot(Sep) + geom_sf(size=1)
# Oct<-ggplot(Oct) + geom_sf(size=1)
# Nov<-ggplot(Nov) + geom_sf(size=1)
# Dec<-ggplot(Dec) + geom_sf(size=1)
# 
# ggarrange(Aug, Sep, Oct, Nov, Dec,
#           labels=c("08","09","10","11","12"),
#           ncol = 3, nrow = 2)


# RaCA_2011_CDL<-subset(RaCA_2011_CDL,Lat!=0)
# P2011 <-st_as_sf(RaCA_2011_CDL, coords = c('Lon','Lat'),crs="+proj=longlat +datum=NAD83 +no_defs")
# ggplot(P2011) + 
#   geom_sf(aes(col = month), size=3)
# table(RaCA_2011_CDL$month)

# Jan<-subset(P2011,month=="01")
# Feb<-subset(P2011,month=="02")
# Mar<-subset(P2011,month=="03")
# Apr<-subset(P2011,month=="04")
# May<-subset(P2011,month=="05")
# Jun<-subset(P2011,month=="06")
# July<-subset(P2011,month=="07")
# Aug<-subset(P2011,month=="08")
# Sep<-subset(P2011,month=="09")
# 
# Jan<-ggplot(Jan) + geom_sf(size=1)
# Feb<-ggplot(Feb) + geom_sf(size=1)
# Mar<-ggplot(Mar) + geom_sf(size=1)
# Apr<-ggplot(Apr) + geom_sf(size=1)
# May<-ggplot(May) + geom_sf(size=1)
# Jun<-ggplot(Jun) + geom_sf(size=1)
# July<-ggplot(July) + geom_sf(size=1)
# Aug<-ggplot(Aug) + geom_sf(size=1)
# Sep<-ggplot(Sep) + geom_sf(size=1)
# 
# 
# ggarrange(Jan, Feb, Mar, Apr, May,Jun,July, Aug, Sep,
#           labels=c("01","02","03","04","05","06","07","08","09"),
#           ncol = 3, nrow = 3)
# 
# rm(Jan, Feb, Mar, Apr, May,Jun,July, Aug, Sep,Oct, Nov, Dec, P2010,P2011)
#############################prism climate data
prism_set_dl_dir("data/Raw/prism/prismtmp")

# get_prism_annual(type = "tmean", year = 2010, keepZip = FALSE)
# get_prism_annual(type = "tmax", year = 2010, keepZip = FALSE)
# get_prism_annual(type = "tmin", year = 2010, keepZip = FALSE)
# get_prism_annual(type = "ppt", year = 2010, keepZip = FALSE)
# 
# ##2010
# tmean2010 <- prism_archive_subset("tmean", "annual", year = 2010)
# tmean2010 <- pd_to_file(tmean2010)
# tmean2010_raster <-raster(tmean2010)
# writeRaster(tmean2010_raster, "data/Raw/prism/tmean2010_raster.tif", overwrite = TRUE)
# # tmax2010 <- prism_archive_subset("tmax", "annual", year = 2010)
# # tmax2010 <- pd_to_file(tmax2010)
# # tmax2010_raster <-raster(tmax2010)
# # writeRaster(tmax2010_raster, "data/Raw/prism/tmax2010_raster.tif", overwrite = TRUE)
# # tmin2010 <- prism_archive_subset("tmin", "annual", year = 2010)
# # tmin2010 <- pd_to_file(tmin2010)
# # tmin2010_raster <-raster(tmin2010)
# # writeRaster(tmin2010_raster, "data/Raw/prism/tmin2010_raster.tif", overwrite = TRUE)
# ppt2010 <- prism_archive_subset("ppt", "annual", year = 2010)
# ppt2010 <- pd_to_file(ppt2010)
# ppt2010_raster <-raster(ppt2010)
# writeRaster(ppt2010_raster, "data/Raw/prism/ppt2010_raster.tif", overwrite = TRUE)
# 
# ##2011
# tmean2011 <- prism_archive_subset("tmean", "annual", year = 2011)
# tmean2011 <- pd_to_file(tmean2011)
# tmean2011_raster <-raster(tmean2011)
# writeRaster(tmean2011_raster, "data/Raw/prism/tmean2011_raster.tif", overwrite = TRUE)
# # tmax2011 <- prism_archive_subset("tmax", "annual", year = 2011)
# # tmax2011 <- pd_to_file(tmax2011)
# # tmax2011_raster <-raster(tmax2011)
# # writeRaster(tmax2011_raster, "data/Raw/prism/tmax2011_raster.tif", overwrite = TRUE)
# # tmin2011 <- prism_archive_subset("tmin", "annual", year = 2011)
# # tmin2011 <- pd_to_file(tmin2011)
# # tmin2011_raster <-raster(tmin2011)
# # writeRaster(tmin2010_raster, "data/Raw/prism/tmin2011_raster.tif", overwrite = TRUE)
# ppt2011 <- prism_archive_subset("ppt", "annual", year = 2011)
# ppt2011 <- pd_to_file(ppt2011)
# ppt2011_raster <-raster(ppt2011)
# writeRaster(ppt2011_raster, "data/Raw/prism/ppt2011_raster.tif", overwrite = TRUE)
# 
# rm(tmean2010,tmean2011,tmax2010,tmax2011,tmin2010,tmin2011,ppt2010,ppt2011)


##2010
# get_prism_monthlys(type = "tmean", year = 2010, mon=1:12, keepZip = FALSE)
# get_prism_monthlys(type = "ppt", year = 2010, mon=1:12, keepZip = FALSE)

tmean2010 <- prism_archive_subset("tmean", "monthly", year = 2010)
tmean2010 <- pd_to_file(tmean2010)
t2010 <- lapply(tmean2010,raster)
st2010 <-stack(t2010)
writeRaster(st2010 , "data/Raw/prism/tmean2010_raster.tif", overwrite = TRUE)

ppt2010 <- prism_archive_subset("ppt", "monthly", year = 2010)
ppt2010 <- pd_to_file(ppt2010)
p2010 <- lapply(ppt2010,raster)
sp2010 <-stack(p2010)
writeRaster(sp2010 , "data/Raw/prism/ppt2010_raster.tif", overwrite = TRUE)


##2011
#get_prism_monthlys(type = "tmean", year = 2011, mon=1:12, keepZip = FALSE)
#get_prism_monthlys(type = "ppt", year = 2011, mon=1:12, keepZip = FALSE)

tmean2011 <- prism_archive_subset("tmean", "monthly", year = 2011)
tmean2011 <- pd_to_file(tmean2011)
t2011 <- lapply(tmean2011,raster)
st2011 <-stack(t2011)
# writeRaster(st2011 , "prism/tmean2011_raster.tif", overwrite = TRUE)

ppt2011 <- prism_archive_subset("ppt", "monthly", year = 2011)
ppt2011 <- pd_to_file(ppt2011)
p2011 <- lapply(ppt2011,raster)
sp2011 <-stack(p2011)
# writeRaster(sp2011 , "prism/ppt2011_raster.tif", overwrite = TRUE)

##2009
# get_prism_monthlys(type = "tmean", year = 2009, mon=1:12, keepZip = FALSE)
# get_prism_monthlys(type = "ppt", year = 2009, mon=1:12, keepZip = FALSE)

tmean2009 <- prism_archive_subset("tmean", "monthly", year = 2009)
tmean2009 <- pd_to_file(tmean2009)
t2009 <- lapply(tmean2009,raster)
st2009 <-stack(t2009)
writeRaster(st2009 , "data/Raw/prism/tmean2009_raster.tif", overwrite = TRUE)

ppt2009 <- prism_archive_subset("ppt", "monthly", year = 2009)
ppt2009 <- pd_to_file(ppt2009)
p2009 <- lapply(ppt2009,raster)
sp2009 <-stack(p2009)
writeRaster(sp2009 , "data/Raw/prism/ppt2009_raster.tif", overwrite = TRUE)



###extract climate
climate2009 <-stack(st2009,sp2009)
climate2010 <-stack(st2010,sp2010)
climate2011 <-stack(st2011,sp2011)
RaCA_2010_CDL.sf <- st_as_sf(RaCA_2010_CDL, coords = c("Lon", "Lat"), crs ="+proj=longlat +datum=NAD83 +no_defs")
RaCA_2011_CDL.sf <- st_as_sf(RaCA_2011_CDL, coords = c("Lon", "Lat"), crs ="+proj=longlat +datum=NAD83 +no_defs")
climatevalue2010=raster::extract(climate2010, RaCA_2010_CDL.sf)
climatevalue2011=raster::extract(climate2011, RaCA_2011_CDL.sf)
RaCA_2010=cbind(RaCA_2010_CDL,climatevalue2010)
RaCA_2011=cbind(RaCA_2011_CDL,climatevalue2011)

climatevalue2009_10=raster::extract(climate2009, RaCA_2010_CDL.sf)
climatevalue2010_11=raster::extract(climate2010, RaCA_2011_CDL.sf)
RaCA_2010=cbind(RaCA_2010,climatevalue2009_10)
RaCA_2011=cbind(RaCA_2011,climatevalue2010_11)

rm(list = ls()[grepl("t", ls())])
rm(list = ls()[grepl("CDL", ls())])
rm(list = ls()[grepl("s", ls())])
rm(list = ls()[grepl("p", ls())])

setnames(RaCA_2010, old = c('PRISM_tmean_stable_4kmM3_201001_bil','PRISM_tmean_stable_4kmM3_201002_bil','PRISM_tmean_stable_4kmM3_201003_bil','PRISM_tmean_stable_4kmM3_201004_bil',
                            'PRISM_tmean_stable_4kmM3_201005_bil','PRISM_tmean_stable_4kmM3_201006_bil','PRISM_tmean_stable_4kmM3_201007_bil','PRISM_tmean_stable_4kmM3_201008_bil',
                            'PRISM_tmean_stable_4kmM3_201009_bil','PRISM_tmean_stable_4kmM3_201010_bil','PRISM_tmean_stable_4kmM3_201011_bil','PRISM_tmean_stable_4kmM3_201012_bil'), 
         new = c('tmean_1','tmean_2','tmean_3','tmean_4','tmean_5','tmean_6','tmean_7','tmean_8','tmean_9','tmean_10','tmean_11','tmean_12'))
setnames(RaCA_2011, old = c('PRISM_tmean_stable_4kmM3_201101_bil','PRISM_tmean_stable_4kmM3_201102_bil','PRISM_tmean_stable_4kmM3_201103_bil','PRISM_tmean_stable_4kmM3_201104_bil',
                            'PRISM_tmean_stable_4kmM3_201105_bil','PRISM_tmean_stable_4kmM3_201106_bil','PRISM_tmean_stable_4kmM3_201107_bil','PRISM_tmean_stable_4kmM3_201108_bil',
                            'PRISM_tmean_stable_4kmM3_201109_bil','PRISM_tmean_stable_4kmM3_201110_bil','PRISM_tmean_stable_4kmM3_201111_bil','PRISM_tmean_stable_4kmM3_201112_bil'), 
         new = c('tmean_1','tmean_2','tmean_3','tmean_4','tmean_5','tmean_6','tmean_7','tmean_8','tmean_9','tmean_10','tmean_11','tmean_12'))

setnames(RaCA_2010, old = c('PRISM_tmean_stable_4kmM3_200901_bil','PRISM_tmean_stable_4kmM3_200902_bil','PRISM_tmean_stable_4kmM3_200903_bil','PRISM_tmean_stable_4kmM3_200904_bil',
                            'PRISM_tmean_stable_4kmM3_200905_bil','PRISM_tmean_stable_4kmM3_200906_bil','PRISM_tmean_stable_4kmM3_200907_bil','PRISM_tmean_stable_4kmM3_200908_bil',
                            'PRISM_tmean_stable_4kmM3_200909_bil','PRISM_tmean_stable_4kmM3_200910_bil','PRISM_tmean_stable_4kmM3_200911_bil','PRISM_tmean_stable_4kmM3_200912_bil'), 
         new = c('tmean_01','tmean_02','tmean_03','tmean_04','tmean_05','tmean_06','tmean_07','tmean_08','tmean_09','tmean_010','tmean_011','tmean_012'))

setnames(RaCA_2011, old = c('PRISM_tmean_stable_4kmM3_201001_bil','PRISM_tmean_stable_4kmM3_201002_bil','PRISM_tmean_stable_4kmM3_201003_bil','PRISM_tmean_stable_4kmM3_201004_bil',
                            'PRISM_tmean_stable_4kmM3_201005_bil','PRISM_tmean_stable_4kmM3_201006_bil','PRISM_tmean_stable_4kmM3_201007_bil','PRISM_tmean_stable_4kmM3_201008_bil',
                            'PRISM_tmean_stable_4kmM3_201009_bil','PRISM_tmean_stable_4kmM3_201010_bil','PRISM_tmean_stable_4kmM3_201011_bil','PRISM_tmean_stable_4kmM3_201012_bil'), 
         new = c('tmean_01','tmean_02','tmean_03','tmean_04','tmean_05','tmean_06','tmean_07','tmean_08','tmean_09','tmean_010','tmean_011','tmean_012'))



setnames(RaCA_2010, old = c('PRISM_ppt_stable_4kmM3_201001_bil','PRISM_ppt_stable_4kmM3_201002_bil','PRISM_ppt_stable_4kmM3_201003_bil','PRISM_ppt_stable_4kmM3_201004_bil',
                            'PRISM_ppt_stable_4kmM3_201005_bil','PRISM_ppt_stable_4kmM3_201006_bil','PRISM_ppt_stable_4kmM3_201007_bil','PRISM_ppt_stable_4kmM3_201008_bil',
                            'PRISM_ppt_stable_4kmM3_201009_bil','PRISM_ppt_stable_4kmM3_201010_bil','PRISM_ppt_stable_4kmM3_201011_bil','PRISM_ppt_stable_4kmM3_201012_bil'), 
         new = c('ppt_1','ppt_2','ppt_3','ppt_4','ppt_5','ppt_6','ppt_7','ppt_8','ppt_9','ppt_10','ppt_11','ppt_12'))

setnames(RaCA_2011, old = c('PRISM_ppt_stable_4kmM3_201101_bil','PRISM_ppt_stable_4kmM3_201102_bil','PRISM_ppt_stable_4kmM3_201103_bil','PRISM_ppt_stable_4kmM3_201104_bil',
                            'PRISM_ppt_stable_4kmM3_201105_bil','PRISM_ppt_stable_4kmM3_201106_bil','PRISM_ppt_stable_4kmM3_201107_bil','PRISM_ppt_stable_4kmM3_201108_bil',
                            'PRISM_ppt_stable_4kmM3_201109_bil','PRISM_ppt_stable_4kmM3_201110_bil','PRISM_ppt_stable_4kmM3_201111_bil','PRISM_ppt_stable_4kmM3_201112_bil'), 
         new = c('ppt_1','ppt_2','ppt_3','ppt_4','ppt_5','ppt_6','ppt_7','ppt_8','ppt_9','ppt_10','ppt_11','ppt_12'))

setnames(RaCA_2010, old = c('PRISM_ppt_stable_4kmM3_200901_bil','PRISM_ppt_stable_4kmM3_200902_bil','PRISM_ppt_stable_4kmM3_200903_bil','PRISM_ppt_stable_4kmM3_200904_bil',
                            'PRISM_ppt_stable_4kmM3_200905_bil','PRISM_ppt_stable_4kmM3_200906_bil','PRISM_ppt_stable_4kmM3_200907_bil','PRISM_ppt_stable_4kmM3_200908_bil',
                            'PRISM_ppt_stable_4kmM3_200909_bil','PRISM_ppt_stable_4kmM3_200910_bil','PRISM_ppt_stable_4kmM3_200911_bil','PRISM_ppt_stable_4kmM3_200912_bil'), 
         new = c('ppt_01','ppt_02','ppt_03','ppt_04','ppt_05','ppt_06','ppt_07','ppt_08','ppt_09','ppt_010','ppt_011','ppt_012'))

setnames(RaCA_2011, old = c('PRISM_ppt_stable_4kmM3_201001_bil','PRISM_ppt_stable_4kmM3_201002_bil','PRISM_ppt_stable_4kmM3_201003_bil','PRISM_ppt_stable_4kmM3_201004_bil',
                            'PRISM_ppt_stable_4kmM3_201005_bil','PRISM_ppt_stable_4kmM3_201006_bil','PRISM_ppt_stable_4kmM3_201007_bil','PRISM_ppt_stable_4kmM3_201008_bil',
                            'PRISM_ppt_stable_4kmM3_201009_bil','PRISM_ppt_stable_4kmM3_201010_bil','PRISM_ppt_stable_4kmM3_201011_bil','PRISM_ppt_stable_4kmM3_201012_bil'), 
         new = c('ppt_01','ppt_02','ppt_03','ppt_04','ppt_05','ppt_06','ppt_07','ppt_08','ppt_09','ppt_010','ppt_011','ppt_012'))

###process before regression
options(max.print=1000000)
library(data.table)
setnames(RaCA_2010,old=c("Class_Names.4", "Class_Names.3","Class_Names.2","Class_Names.1"),
         new=c("CDL_n1","CDL_0","CDL_1","CDL_2"))

setnames(RaCA_2011,old=c("Class_Names.4","Class_Names.3","Class_Names.2","Class_Names.1"),
         new=c("CDL_0","CDL_1","CDL_2","CDL_3"))

RaCA_2010=subset(RaCA_2010,select = -c(CDL_n1))
RaCA_2011=subset(RaCA_2011,select = -c(CDL_3))


raca <-rbind(RaCA_2010,RaCA_2011)

rm(RaCA)
raca <- raca[!duplicated(raca$rcasiteid), ]

setnames(raca,old=c("SOC0_5","SOC5_30","SOC30_100"),
         new=c("SOC_5","SOC_30","SOC_100"))

## export df 
write.csv(raca, "data/Created/climate.csv", row.names=TRUE)
raca$tmean_3m<- ifelse(raca$month=="01", (raca$tmean_1+raca$tmean_011+raca$tmean_012)/3,
                       ifelse(raca$month=="02", (raca$tmean_1+raca$tmean_2+raca$tmean_012)/3,
                              ifelse(raca$month=="03", (raca$tmean_1+raca$tmean_2+raca$tmean_3)/3,
                                     ifelse(raca$month=="04", (raca$tmean_2+raca$tmean_3+raca$tmean_4)/3,
                                            ifelse(raca$month=="05", (raca$tmean_3+raca$tmean_4+raca$tmean_5)/3,
                                                   ifelse(raca$month=="06", (raca$tmean_4+raca$tmean_4+raca$tmean_5)/3,
                                                          ifelse(raca$month=="07", (raca$tmean_5+raca$tmean_6+raca$tmean_7)/3,
                                                                 ifelse(raca$month=="08", (raca$tmean_6+raca$tmean_7+raca$tmean_8)/3,
                                                                        ifelse(raca$month=="09", (raca$tmean_7+raca$tmean_8+raca$tmean_9)/3,
                                                                               ifelse(raca$month=="10", (raca$tmean_8+raca$tmean_9+raca$tmean_10)/3,
                                                                                      ifelse(raca$month=="11", (raca$tmean_9+raca$tmean_10+raca$tmean_11)/3,
                                                                                             ifelse(raca$month=="12", (raca$tmean_10+raca$tmean_11+raca$tmean_12)/3,
                                                                                                    NA))))))))))))

raca$ppt_3m<- ifelse(raca$month=="01", (raca$ppt_1+raca$ppt_011+raca$ppt_012)/3,
                     ifelse(raca$month=="02", (raca$tppt_1+raca$ppt_2+raca$ppt_012)/3,
                            ifelse(raca$month=="03", (raca$ppt_1+raca$ppt_2+raca$ppt_3)/3,
                                   ifelse(raca$month=="04", (raca$ppt_2+raca$ppt_3+raca$ppt_4)/3,
                                          ifelse(raca$month=="05", (raca$ppt_3+raca$ppt_4+raca$ppt_5)/3,
                                                 ifelse(raca$month=="06", (raca$ppt_4+raca$ppt_4+raca$ppt_5)/3,
                                                        ifelse(raca$month=="07", (raca$ppt_5+raca$ppt_6+raca$ppt_7)/3,
                                                               ifelse(raca$month=="08", (raca$ppt_6+raca$ppt_7+raca$ppt_8)/3,
                                                                      ifelse(raca$month=="09", (raca$ppt_7+raca$ppt_8+raca$ppt_9)/3,
                                                                             ifelse(raca$month=="10", (raca$ppt_8+raca$ppt_9+raca$ppt_10)/3,
                                                                                    ifelse(raca$month=="11", (raca$ppt_9+raca$ppt_10+raca$ppt_11)/3,
                                                                                           ifelse(raca$month=="12", (raca$ppt_10+raca$ppt_11+raca$ppt_12)/3,
                                                                                                  NA ))))))))))))

raca$tmean_4_12m<- ifelse(raca$month=="01", (raca$tmean_010+raca$tmean_09+raca$tmean_08+raca$tmean_07+raca$tmean_06+raca$tmean_05+raca$tmean_04+raca$tmean_03+raca$tmean_02)/9,
                          ifelse(raca$month=="02", (raca$tmean_011+raca$tmean_010+raca$tmean_09+raca$tmean_08+raca$tmean_07+raca$tmean_06+raca$tmean_05+raca$tmean_04+raca$tmean_03)/9,
                                 ifelse(raca$month=="03", (raca$tmean_012+raca$tmean_011+raca$tmean_010+raca$tmean_09+raca$tmean_08+raca$tmean_07+raca$tmean_06+raca$tmean_05+raca$tmean_04)/9,
                                        ifelse(raca$month=="04", (raca$tmean_1+raca$tmean_012+raca$tmean_011+raca$tmean_010+raca$tmean_09+raca$tmean_08+raca$tmean_07+raca$tmean_06+raca$tmean_05)/9,
                                               ifelse(raca$month=="05", (raca$tmean_2+raca$tmean_1+raca$tmean_012+raca$tmean_011+raca$tmean_010+raca$tmean_09+raca$tmean_08+raca$tmean_07+raca$tmean_06)/9,
                                                      ifelse(raca$month=="06", (raca$tmean_3+raca$tmean_2+raca$tmean_1+raca$tmean_012+raca$tmean_011+raca$tmean_010+raca$tmean_09+raca$tmean_08+raca$tmean_07)/9,
                                                             ifelse(raca$month=="07", (raca$tmean_4+raca$tmean_3+raca$tmean_2+raca$tmean_1+raca$tmean_012+raca$tmean_011+raca$tmean_010+raca$tmean_09+raca$tmean_08)/9,
                                                                    ifelse(raca$month=="08", (raca$tmean_5+raca$tmean_4+raca$tmean_3+raca$tmean_2+raca$tmean_1+raca$tmean_012+raca$tmean_011+raca$tmean_010+raca$tmean_09)/9,
                                                                           ifelse(raca$month=="09", (raca$tmean_6+raca$tmean_5+raca$tmean_4+raca$tmean_3+raca$tmean_2+raca$tmean_1+raca$tmean_012+raca$tmean_011+raca$tmean_010)/9,
                                                                                  ifelse(raca$month=="10", (raca$tmean_7+raca$tmean_6+raca$tmean_5+raca$tmean_4+raca$tmean_3+raca$tmean_2+raca$tmean_1+raca$tmean_012+raca$tmean_011)/9,
                                                                                         ifelse(raca$month=="11", (raca$tmean_8+raca$tmean_7+raca$tmean_6+raca$tmean_5+raca$tmean_4+raca$tmean_3+raca$tmean_2+raca$tmean_1+raca$tmean_012)/9,
                                                                                                ifelse(raca$month=="12", (raca$tmean_9+raca$tmean_8+raca$tmean_7+raca$tmean_6+raca$tmean_5+raca$tmean_4+raca$tmean_3+raca$tmean_2+raca$tmean_1)/9,
                                                                                                       NA ))))))))))))



raca$ppt_4_12m<- ifelse(raca$month=="01", (raca$ppt_010+raca$ppt_09+raca$ppt_08+raca$ppt_07+raca$ppt_06+raca$ppt_05+raca$ppt_04+raca$ppt_03+raca$ppt_02)/9,
                          ifelse(raca$month=="02", (raca$ppt_011+raca$ppt_010+raca$ppt_09+raca$ppt_08+raca$ppt_07+raca$ppt_06+raca$ppt_05+raca$ppt_04+raca$ppt_03)/9,
                                 ifelse(raca$month=="03", (raca$ppt_012+raca$ppt_011+raca$ppt_010+raca$ppt_09+raca$ppt_08+raca$ppt_07+raca$ppt_06+raca$ppt_05+raca$ppt_04)/9,
                                        ifelse(raca$month=="04", (raca$ppt_1+raca$ppt_012+raca$ppt_011+raca$ppt_010+raca$ppt_09+raca$ppt_08+raca$ppt_07+raca$ppt_06+raca$ppt_05)/9,
                                               ifelse(raca$month=="05", (raca$ppt_2+raca$ppt_1+raca$ppt_012+raca$ppt_011+raca$ppt_010+raca$ppt_09+raca$ppt_08+raca$ppt_07+raca$ppt_06)/9,
                                                      ifelse(raca$month=="06", (raca$ppt_3+raca$ppt_2+raca$ppt_1+raca$ppt_012+raca$ppt_011+raca$ppt_010+raca$ppt_09+raca$ppt_08+raca$ppt_07)/9,
                                                             ifelse(raca$month=="07", (raca$ppt_4+raca$ppt_3+raca$ppt_2+raca$ppt_1+raca$ppt_012+raca$ppt_011+raca$ppt_010+raca$ppt_09+raca$ppt_08)/9,
                                                                    ifelse(raca$month=="08", (raca$ppt_5+raca$ppt_4+raca$ppt_3+raca$ppt_2+raca$ppt_1+raca$ppt_012+raca$ppt_011+raca$ppt_010+raca$ppt_09)/9,
                                                                           ifelse(raca$month=="09", (raca$ppt_6+raca$ppt_5+raca$ppt_4+raca$ppt_3+raca$ppt_2+raca$ppt_1+raca$ppt_012+raca$ppt_011+raca$ppt_010)/9,
                                                                                  ifelse(raca$month=="10", (raca$ppt_7+raca$ppt_6+raca$ppt_5+raca$ppt_4+raca$ppt_3+raca$ppt_2+raca$ppt_1+raca$ppt_012+raca$ppt_011)/9,
                                                                                         ifelse(raca$month=="11", (raca$ppt_8+raca$ppt_7+raca$ppt_6+raca$ppt_5+raca$ppt_4+raca$ppt_3+raca$ppt_2+raca$ppt_1+raca$ppt_012)/9,
                                                                                                ifelse(raca$month=="12", (raca$ppt_9+raca$ppt_8+raca$ppt_7+raca$ppt_6+raca$ppt_5+raca$ppt_4+raca$ppt_3+raca$ppt_2+raca$ppt_1)/9,
                                                                                                       NA ))))))))))))

#racalong <- reshape(raca, direction = "long", idvar="rcasiteid", 
#                    varying = 40:42, sep = "_")

racalong <- reshape(raca, direction = "long", idvar="rcasiteid", 
                    varying = 31:33, sep = "_")

setnames(racalong,old="time",
         new="depth")

setnames(raca,old=c("SOC_5","SOC_30","SOC_100"),
         new=c("SOC0_5","SOC5_30","SOC30_100"))

racalong_deepenough <-subset(racalong, racalong$SOC_thickness>racalong$depth | racalong$SOC_thickness==racalong$depth,)
racalong_deepenough <-subset(racalong_deepenough,racalong_deepenough$SOC>0,)
racalong <- racalong_deepenough                             

racalong$tmean_sp <-(racalong$tmean_3+racalong$tmean_4+racalong$tmean_5)/3
racalong$tmean_su <-(racalong$tmean_6+racalong$tmean_7+racalong$tmean_8)/3
racalong$tmean_au <-(racalong$tmean_9+racalong$tmean_10+racalong$tmean_11)/3
racalong$tmean_wi <-(racalong$tmean_12+racalong$tmean_1+racalong$tmean_2)/3

racalong$ppt_sp <-(racalong$ppt_3+racalong$ppt_4+racalong$ppt_5)/3
racalong$ppt_su <-(racalong$ppt_6+racalong$ppt_7+racalong$ppt_8)/3
racalong$ppt_au <-(racalong$ppt_9+racalong$ppt_10+racalong$ppt_11)/3
racalong$ppt_wi <-(racalong$ppt_12+racalong$ppt_1+racalong$ppt_2)/3


################################################################
#################################################################
#### subsetting 7 major types
racalong$CDL_0[racalong$CDL_0=="1"] <-'corn'
racalong$CDL_0[racalong$CDL_0=="2"] <-'cotton'
racalong$CDL_0[racalong$CDL_0=="3"] <-'rice'
racalong$CDL_0[racalong$CDL_0=="5"] <-'soybean'
racalong$CDL_0[racalong$CDL_0=="23"] <-'spring_wheat'
racalong$CDL_0[racalong$CDL_0=="24"] <-'winter_wheat'
racalong$CDL_0[racalong$CDL_0=="36"] <-'alfalfa'
racalong$CDL_0[racalong$CDL_0=="37"] <-'other_hay/non-alfalfa'
racalong$CDL_0[racalong$CDL_0=="61"] <-'fallow'

racalong$CDL_1[racalong$CDL_1=="1"] <-'corn'
racalong$CDL_1[racalong$CDL_1=="2"] <-'cotton'
racalong$CDL_1[racalong$CDL_1=="3"] <-'rice'
racalong$CDL_1[racalong$CDL_1=="5"] <-'soybean'
racalong$CDL_1[racalong$CDL_1=="23"] <-'spring_wheat'
racalong$CDL_1[racalong$CDL_1=="24"] <-'winter_wheat'
racalong$CDL_1[racalong$CDL_1=="36"] <-'alfalfa'
racalong$CDL_1[racalong$CDL_1=="37"] <-'other_hay/non-alfalfa'
racalong$CDL_1[racalong$CDL_1=="61"] <-'fallow'

racalong$CDL_2[racalong$CDL_2=="1"] <-'corn'
racalong$CDL_2[racalong$CDL_2=="2"] <-'cotton'
racalong$CDL_2[racalong$CDL_2=="3"] <-'rice'
racalong$CDL_2[racalong$CDL_2=="5"] <-'soybean'
racalong$CDL_2[racalong$CDL_2=="23"] <-'spring_wheat'
racalong$CDL_2[racalong$CDL_2=="24"] <-'winter_wheat'
racalong$CDL_2[racalong$CDL_2=="36"] <-'alfalfa'
racalong$CDL_2[racalong$CDL_2=="37"] <-'other_hay/non-alfalfa'
racalong$CDL_2[racalong$CDL_2=="61"] <-'fallow'


raca_major0 <- racalong[racalong$CDL_0=="corn" | racalong$CDL_0=="cotton"| racalong$CDL_0=="rice"| 
                          racalong$CDL_0=="soybean"| racalong$CDL_0=="spring_wheat"| racalong$CDL_0=="winter_wheat"| 
                          racalong$CDL_0=="alfalfa"| racalong$CDL_0=="other_hay/non-alfalfa"| racalong$CDL_0=="fallow", ]

raca_major1 <- raca_major0[raca_major0$CDL_1=="corn" | raca_major0$CDL_1=="cotton"| raca_major0$CDL_1=="rice"| 
                             raca_major0$CDL_1=="soybean"| raca_major0$CDL_1=="spring_wheat"| raca_major0$CDL_1=="winter_wheat"| 
                             raca_major0$CDL_1=="alfalfa"| raca_major0$CDL_1=="other_hay/non-alfalfa"| raca_major0$CDL_1=="fallow", ]


raca_major1 <- raca_major1[raca_major1$CDL_2=="corn" | raca_major1$CDL_2=="cotton"| raca_major1$CDL_2=="rice"| 
                             raca_major1$CDL_2=="soybean"| raca_major1$CDL_2=="spring_wheat"| raca_major1$CDL_2=="winter_wheat"| 
                             raca_major1$CDL_2=="alfalfa"| raca_major1$CDL_2=="other_hay/non-alfalfa"| raca_major1$CDL_2=="fallow", ]

rm(BD,RaCA_2010,RaCA_2011)

raca_major1_5 <- raca_major1[raca_major1$depth=="5",]
raca_major1_30 <- raca_major1[raca_major1$depth=="30",]
raca_major1_100 <- raca_major1[raca_major1$depth=="100",]
#### summarize by region

raca_major1$monthnum <- as.numeric(raca_major1$month)
raca_major1$yearnum <- as.numeric(raca_major1$year)
#raca_major1$CDL_0num <- as.numeric(raca_major1$CDL_0)
#raca_major1$CDL_1num <- as.numeric(raca_major1$CDL_1)
#raca_major1$CDL_2num <- as.numeric(raca_major1$CDL_2)


#raca_major1 %>%
#  group_by(Region) %>%
#  dplyr::summarize(MeanSOC = mean(SOC, na.rm = TRUE),
#                   Meanmonthnum = mean(monthnum, na.rm = TRUE),
#                   Meanyearnum = mean(yearnum, na.rm = TRUE),
#                   MeanCDL_0 = mean(CDL_0num, na.rm = TRUE),
#                   MeanCDL_1 = mean(CDL_1num, na.rm = TRUE),
#                   Meanppt_wi = mean(ppt_wi, na.rm = TRUE),)


# # Converting character variables to factor :
#raca_major1$month  = as.factor(raca_major1$month)



## export df 

write.csv(raca_major1, "data/Created/majorcrop.csv", row.names=TRUE)

rm(raca, raca_major0, raca_major1_100, raca_major1_30, raca_major1_5, racalong_deepenough, racalong)

raca_major1 <-raca_major1[!is.na(raca_major1$CDL_0),]
raca_major1 <-raca_major1[!is.na(raca_major1$CDL_1),]
raca_major1 <-raca_major1[!is.na(raca_major1$CDL_2),]


#############export lastyear.png
library("RColorBrewer")
library("reshape2")
library("ggplot2")

box_plot <- ggplot(raca_major1 ,aes(y= fct_rev(factor(CDL_0)), fill = factor(CDL_1)))
box_plot +
  geom_bar(stat="count", position = "stack") + 
  theme_grey(base_size = 18)+ 
  scale_fill_brewer(palette="Paired") +
  xlab("Number of observations") +
  ylab("Crop type this year")+ 
  guides(fill=guide_legend("Crop type last year"))
ggsave("output/figures/lastyear.png", width = 350, height = 200, units = "mm")

################ native Americans
library(rgeos)
library(sf)
library(sp)
library(rgdal)

map <- read_sf("data/Raw/native/tl_2018_us_aiannh/tl_2018_us_aiannh.shp")



raca_major1.sf <- st_as_sf(raca_major1, coords = c("Lon", "Lat"), crs ="+proj=longlat +datum=NAD83 +no_defs")
inter <- raca_major1.sf %>% mutate(
  intersection = as.integer(st_intersects( raca_major1.sf,map)))


inter$native<-'N'
inter$native[inter$intersection>0]<-'Y'
inter = subset(inter, select =c(native))
raca_major1=cbind(raca_major1,inter)



table(raca_major1$native)

rm(inter, map)

################ 9 groups

raca<-raca_major1
### corn & soybean 
raca$CDL_1[raca$CDL_1=="1"] <-'corn'
raca$CDL_1[raca$CDL_1=="5"] <-'soybean'
raca$CDL_1[raca$CDL_1=="61"] <-'fallow'
raca$CDL_0[raca$CDL_0=="1"] <-'corn'
raca$CDL_0[raca$CDL_0=="5"] <-'soybean'
raca$CDL_0[raca$CDL_0=="61"] <-'fallow'
raca$CDL_2[raca$CDL_2=="1"] <-'corn'
raca$CDL_2[raca$CDL_2=="5"] <-'soybean'
raca$CDL_2[raca$CDL_2=="61"] <-'fallow'

raca <- raca[raca$CDL_0=="corn" | raca$CDL_0=="soybean" | raca$CDL_0=="fallow",]
raca <- raca[raca$CDL_1=="corn" | raca$CDL_1=="soybean" | raca$CDL_1=="fallow",]
raca <- raca[raca$CDL_2=="corn" | raca$CDL_2=="soybean" | raca$CDL_2=="fallow",]


racawide_cornsoybean<-raca
racawide_cornsoybean$rotation <-NA
racawide_cornsoybean$rotation<- ifelse(racawide_cornsoybean$CDL_2=="soybean" & racawide_cornsoybean$CDL_1=="soybean" & racawide_cornsoybean$CDL_0=="soybean","sss", 
                                       ifelse(racawide_cornsoybean$CDL_2=="soybean" & racawide_cornsoybean$CDL_1=="soybean" & racawide_cornsoybean$CDL_0=="corn","ssc",
                                              ifelse(racawide_cornsoybean$CDL_2=="soybean" & racawide_cornsoybean$CDL_1=="corn" & racawide_cornsoybean$CDL_0=="soybean","scs",
                                                     ifelse(racawide_cornsoybean$CDL_2=="corn" & racawide_cornsoybean$CDL_1=="soybean" & racawide_cornsoybean$CDL_0=="soybean","css",
                                                            ifelse(racawide_cornsoybean$CDL_2=="corn" & racawide_cornsoybean$CDL_1=="corn" & racawide_cornsoybean$CDL_0=="soybean","ccs",
                                                                   ifelse(racawide_cornsoybean$CDL_2=="corn" & racawide_cornsoybean$CDL_1=="soybean" & racawide_cornsoybean$CDL_0=="corn","csc",
                                                                          ifelse(racawide_cornsoybean$CDL_2=="soybean" & racawide_cornsoybean$CDL_1=="corn" & racawide_cornsoybean$CDL_0=="corn","scc",
                                                                                 ifelse(racawide_cornsoybean$CDL_2=="corn" & racawide_cornsoybean$CDL_1=="corn" & racawide_cornsoybean$CDL_0=="corn","ccc",
                                                                                        ifelse(racawide_cornsoybean$CDL_2=="fallow" & racawide_cornsoybean$CDL_1=="soybean" & racawide_cornsoybean$CDL_0=="soybean","fss", 
                                                                                               ifelse(racawide_cornsoybean$CDL_2=="soybean" & racawide_cornsoybean$CDL_1=="fallow" & racawide_cornsoybean$CDL_0=="soybean","sfs", 
                                                                                                      ifelse(racawide_cornsoybean$CDL_2=="soybean" & racawide_cornsoybean$CDL_1=="soybean" & racawide_cornsoybean$CDL_0=="fallow","ssf", 
                                                                                                             ifelse(racawide_cornsoybean$CDL_2=="fallow" & racawide_cornsoybean$CDL_1=="fallow" & racawide_cornsoybean$CDL_0=="soybean","ffs",      
                                                                                                                    ifelse(racawide_cornsoybean$CDL_2=="fallow" & racawide_cornsoybean$CDL_1=="soybean" & racawide_cornsoybean$CDL_0=="fallow","fsf", 
                                                                                                                           ifelse(racawide_cornsoybean$CDL_2=="soybean" & racawide_cornsoybean$CDL_1=="fallow" & racawide_cornsoybean$CDL_0=="fallow","sff", 
                                                                                                                                  ifelse(racawide_cornsoybean$CDL_2=="fallow" & racawide_cornsoybean$CDL_1=="corn" & racawide_cornsoybean$CDL_0=="corn","fcc",
                                                                                                                                         ifelse(racawide_cornsoybean$CDL_2=="corn" & racawide_cornsoybean$CDL_1=="fallow" & racawide_cornsoybean$CDL_0=="corn","cfc",
                                                                                                                                                ifelse(racawide_cornsoybean$CDL_2=="corn" & racawide_cornsoybean$CDL_1=="corn" & racawide_cornsoybean$CDL_0=="fallow","ccf",
                                                                                                                                                       ifelse(racawide_cornsoybean$CDL_2=="fallow" & racawide_cornsoybean$CDL_1=="corn" & racawide_cornsoybean$CDL_0=="fallow","fcf",
                                                                                                                                                              ifelse(racawide_cornsoybean$CDL_2=="corn" & racawide_cornsoybean$CDL_1=="fallow" & racawide_cornsoybean$CDL_0=="fallow","cff",
                                                                                                                                                                     ifelse(racawide_cornsoybean$CDL_2=="fallow" & racawide_cornsoybean$CDL_1=="fallow" & racawide_cornsoybean$CDL_0=="corn","ffc",
                                                                                                                                                                            ifelse(racawide_cornsoybean$CDL_2=="corn" & racawide_cornsoybean$CDL_1=="fallow" & racawide_cornsoybean$CDL_0=="soybean","cfs",
                                                                                                                                                                                   ifelse(racawide_cornsoybean$CDL_2=="corn" & racawide_cornsoybean$CDL_1=="soybean" & racawide_cornsoybean$CDL_0=="fallow","csf",
                                                                                                                                                                                          ifelse(racawide_cornsoybean$CDL_2=="soybean" & racawide_cornsoybean$CDL_1=="corn" & racawide_cornsoybean$CDL_0=="fallow","scf",
                                                                                                                                                                                                 ifelse(racawide_cornsoybean$CDL_2=="soybean" & racawide_cornsoybean$CDL_1=="fallow" & racawide_cornsoybean$CDL_0=="corn","sfc",
                                                                                                                                                                                                        ifelse(racawide_cornsoybean$CDL_2=="fallow" & racawide_cornsoybean$CDL_1=="corn" & racawide_cornsoybean$CDL_0=="soybean","fcs",
                                                                                                                                                                                                               ifelse(racawide_cornsoybean$CDL_2=="fallow" & racawide_cornsoybean$CDL_1=="soybean" & racawide_cornsoybean$CDL_0=="corn","fsc",
                                                                                                                                                                                                                      "fff"))))))))))))))))))))))))))

## export df 
write.csv(racawide_cornsoybean, "data/Created/cornsoy.csv", row.names=TRUE)


racawide_cornsoybean <-subset(racawide_cornsoybean,racawide_cornsoybean$rotation%in% c("ccc","ccs","csc","css","scc","scs","ssc","sss"),)
box_plot <- ggplot(racawide_cornsoybean, aes(x = factor(rotation), y = log(SOC)))
# Add the geometric object box plot
box_plot +
  geom_boxplot(outlier.colour = "red",
               outlier.shape = 1,
               outlier.size = 1) + 
  aes(fill = factor(depth)) +
  xlab("Crop sequence") +
  ylab("Log(SOC stocks)")+ 
  guides(fill=guide_legend("depth (cm)")) +
  scale_fill_discrete(labels=c('0-5', '5-30', '30-100'))+
  theme_grey(base_size = 16) 
ggsave("output/figures/boxCS.png", width = 350, height = 200, units = "mm")

names(racawide_cornsoybean)[names(racawide_cornsoybean) == "Group"] <- "SampleGroup"
racawide_cornsoybean$group<- ifelse(racawide_cornsoybean$rotation=="ccc","3c", 
                                    ifelse(racawide_cornsoybean$rotation=="ccs" | racawide_cornsoybean$rotation=="csc" | racawide_cornsoybean$rotation=="scc", "2c",
                                           ifelse(racawide_cornsoybean$rotation=="scs" | racawide_cornsoybean$rotation=="ssc" | racawide_cornsoybean$rotation=="css", "1c",
                                                  ifelse(racawide_cornsoybean$rotation=="sss","0c",
                                                         "fallow"))))

box_plot <- ggplot(racawide_cornsoybean, aes(x = factor(group), y = log(SOC)))
# Add the geometric object box plot
box_plot +
  geom_boxplot(outlier.colour = "red",
               outlier.shape = 1,
               outlier.size = 1) + 
  aes(fill = factor(depth)) +
  xlab("Crop sequence") +
  ylab("Log(SOC stocks)")+ 
  guides(fill=guide_legend("depth (cm)")) +
  scale_fill_discrete(labels=c('0-5', '5-30', '30-100'))+
  theme_grey(base_size = 16) 
ggsave("output/figures/box4group.png", width = 200, height = 200, units = "mm")



racawide_cornsoybean$season<- ifelse(racawide_cornsoybean$month=="03" | racawide_cornsoybean$month=="04" |racawide_cornsoybean$month=="05" ,"spring", 
                                     ifelse(racawide_cornsoybean$month=="06" | racawide_cornsoybean$month=="07" |racawide_cornsoybean$month=="08" ,"summer",
                                            ifelse(racawide_cornsoybean$month=="09" | racawide_cornsoybean$month=="10" |racawide_cornsoybean$month=="11" ,"fall",
                                                   ifelse(racawide_cornsoybean$month=="12" | racawide_cornsoybean$month=="01" |racawide_cornsoybean$month=="02" ,"winter",
                                                          ""))))
racawide_cornsoybean$recent_first<- ifelse(racawide_cornsoybean$rotation=="ccc" | racawide_cornsoybean$rotation=="csc" | racawide_cornsoybean$rotation=="scc" | racawide_cornsoybean$rotation=="ssc","C",
                                           "S")

racawide_cornsoybean$recent_second<- ifelse(racawide_cornsoybean$rotation=="ccc" | racawide_cornsoybean$rotation=="ccs" | racawide_cornsoybean$rotation=="scc" | racawide_cornsoybean$rotation=="scs","C",
                                            "S")

racawide_cornsoybean$recent_third<- ifelse(racawide_cornsoybean$rotation=="ccc" | racawide_cornsoybean$rotation=="csc" | racawide_cornsoybean$rotation=="ccs" | racawide_cornsoybean$rotation=="css","C",
                                           "S")
######### map
library(USAboundaries)
library(ggplot2)
library(sp)
library(maps)

crs_use <- "+proj=laea +lat_0=30 +lon_0=-95"
cornsoybean_rotation <-st_as_sf(racawide_cornsoybean, coords = c('Lon','Lat'),crs=4326) %>%
  st_transform(crs = crs_use)

usa_sf <-  us_states() %>% 
  dplyr::filter(!state_abbr %in%  c("PR", "AK", "HI")) %>% 
  st_transform(crs = crs_use)


##### CSmap_5group
ggplot(data = usa_sf) +
  geom_sf() + 
  geom_sf(data = cornsoybean_rotation, aes(shape = group, color= group), 
          size = 2) +
  theme_grey(base_size = 16) 
ggsave("output/figures/CSmap_4group.png", width = 400, height = 250, units = "mm")



library(sf)
pts <-data.frame(data.frame(x=racawide_cornsoybean$Lon, y=racawide_cornsoybean$Lat))
coordinates(pts) <- ~x+y
download.file("http://www2.census.gov/geo/tiger/GENZ2022/shp/cb_2022_us_state_20m.zip", destfile = "states.zip")
unzip("states.zip")
us_state<-shapefile("data/Raw/USshapefile/cb_2022_us_state_20m.shp")
proj4string(pts) <- CRS("+proj=longlat +datum=NAD83 +no_defs")
RACA_state = data.frame(over(pts,us_state))
RACA_STATE <-cbind(RACA_state, racawide_cornsoybean)
RACA_STATE2 <-RACA_STATE[which(RACA_STATE$NAME %in% c("Illinois", "Indiana", "Iowa", "Ohio")),]
RACA_STATE3 <-RACA_STATE[which(RACA_STATE$NAME %in% c("Illinois", "Indiana", "Iowa", "Ohio")),]
racawide_cornsoybean <-RACA_STATE
##summarize by state
RACA_STATE2 %>%
  group_by(STUSPS) %>%
  dplyr::summarize(MeanSOCstock5 = mean(SOCstock5, na.rm = TRUE),
                   MeanSOCstock30 = mean(SOCstock30, na.rm = TRUE),
                   MeanSOCstock100 = mean(SOCstock100, na.rm = TRUE),)
table(RACA_STATE3$STUSPS, RACA_STATE3$group)

agg <- RACA_STATE3 %>% group_by(NAME, group) %>% 
  dplyr::summarise(ave=mean(SOCstock100, na.rm = T),
            .groups = 'drop')

agg <- racawide_cornsoybean %>% group_by(group) %>% 
  dplyr::summarise(ave=mean(SOCstock100, na.rm = T),
                   .groups = 'drop')


#### map in corn belt
# crs_use <- "+proj=laea +lat_0=30 +lon_0=-95"
# cornsoybean_rotation <-st_as_sf(racawide_cornsoybean, coords = c('Lon','Lat'),crs=4326) %>%
#   st_transform(crs = crs_use)
# 
# usa_sf <-  us_states() %>% 
#   dplyr::filter(state_abbr %in%  c("OH", "IA", "IL", "IN")) %>% 
#   st_transform(crs = crs_use)
# 
# ggplot(data = usa_sf) +
#   geom_sf() + 
#   geom_sf(data = cornsoybean_rotation, aes(col = group), 
#           size = 1) 

rm(cornsoybean_rotation, usa_sf)

#descriptive table
library(psych) 
psych::describe(racawide_cornsoybean[ , c('Lat', 'tmean_3m', 'tmean_4_12m', 'ppt_3m', 'ppt_4_12m' )], fast=TRUE)

table(racawide_cornsoybean$texture)
table(racawide_cornsoybean$year)
table(racawide_cornsoybean$season)
table(racawide_cornsoybean$rotation)
table(racawide_cornsoybean$group)
table(racawide_cornsoybean$native)

x[!x %in% boxplot.stats(x)$out]
#test
racawide_cornsoybean$Region =as.character(racawide_cornsoybean$Region)
racawide_cornsoybean$area<- ifelse(racawide_cornsoybean$Region %in% c("12","13","14","18","11","10"),"east", 
                                   ifelse(racawide_cornsoybean$Region %in% c("1","4","7","5"),"north", 
                                                 "south"))

table(racawide_cornsoybean$area)

#racawide_cornsoybean <- racawide_cornsoybean[which(racawide_cornsoybean$Region%in% c("5,7,10,11,12,13,14,16,18")),]

#####regression
raca_major1_5 <- racawide_cornsoybean[racawide_cornsoybean$depth=="5",]
raca_major1_30 <- racawide_cornsoybean[racawide_cornsoybean$depth=="30",]
raca_major1_100 <- racawide_cornsoybean[racawide_cornsoybean$depth=="100",]

psych::describe(raca_major1_5[ , c('SOC')], fast=TRUE)
psych::describe(raca_major1_30[ , c('SOC')], fast=TRUE) 
psych::describe(raca_major1_100[ , c('SOC')], fast=TRUE)
library("texreg")
library("stargazer")
table(raca_major1_100$rotation)


lccc<-lm(formula = log(SOCstock100) ~  year +season+  factor(texture) + Lat+ factor(area)+ native	+
                       relevel(as.factor(rotation), ref="ccc") +
                       tmean_3m+tmean_4_12m+
                       ppt_3m+ppt_4_12m,
                     data = raca_major1_100)

lccs<-lm(formula = log(SOCstock100) ~  year +season+  factor(texture) + Lat+ factor(area)+ native	+
                       relevel(as.factor(rotation), ref="ccs") +
                       tmean_3m+tmean_4_12m+
                       ppt_3m+ppt_4_12m,
                     data = raca_major1_100)

lcsc<-lm(formula = log(SOCstock100) ~  year +season+  factor(texture) + Lat+ factor(area)+ native	+
                       relevel(as.factor(rotation), ref="csc") +
                       tmean_3m+tmean_4_12m+
                       ppt_3m+ppt_4_12m,
                     data = raca_major1_100)

lcss<-lm(formula = log(SOCstock100) ~  year +season+  factor(texture) + Lat+ factor(area)+ native	+
                       relevel(as.factor(rotation), ref="css") +
                       tmean_3m+tmean_4_12m+
                       ppt_3m+ppt_4_12m,
                     data = raca_major1_100)
lscc<-lm(formula = log(SOCstock100) ~  year +season+  factor(texture) + Lat+ factor(area)+ native	+
                       relevel(as.factor(rotation), ref="scc") +
                       tmean_3m+tmean_4_12m+
                       ppt_3m+ppt_4_12m,
                     data = raca_major1_100)

lscs<-lm(formula = log(SOCstock100) ~  year +season+  factor(texture) + Lat+ factor(area)+ native	+
                       relevel(as.factor(rotation), ref="scs") +
                       tmean_3m+tmean_4_12m+
                       ppt_3m+ppt_4_12m,
                     data = raca_major1_100)
lssc<-lm(formula = log(SOCstock100) ~ year +season+  factor(texture) + Lat+ factor(area)+ native	+
                       relevel(as.factor(rotation), ref="ssc") +
                       tmean_3m+tmean_4_12m+
                       ppt_3m+ppt_4_12m,
                     data = raca_major1_100)

lsss<-lm(formula = log(SOCstock100) ~  year +season+  factor(texture) + Lat+ factor(area)+ native	+
                       relevel(as.factor(rotation), ref="sss") +
                       tmean_3m+tmean_4_12m+
                       ppt_3m+ppt_4_12m,
                     data = raca_major1_100)

 
 # export table results

 # screenreg(list(lm_major1_5,lm_major1_30,lm_major1_100,lm_racawide_now ))
 # stargazer(lm_major1_5,lm_major1_30,lm_major1_100,lm_racawide_now, type="latex",summary=FALSE,
 #           title="The effects of different sequences of corn-soybean rotation on log(SOC(Mg/ha)) stocks at different depths",
 #           column.labels = c("0-5 cm", "5-30 cm", "30-100 cm","0-100 cm"),
 #           out="output/tables/table01.tex")
 
 
 #screenreg(list(lm_racawide_ccc, lm_racawide_ccs,lm_racawide_csc, lm_racawide_css, lm_racawide_scc, lm_racawide_scs,lm_racawide_ssc, lm_racawide_sss ))
 stargazer(lccc, lccs,lcsc, lcss, lscc, lscs,lssc, lsss, type="latex",summary=FALSE,
           title="The effects of different sequences of corn-soybean rotation on log(SOC(Mg/ha)) stocks",
           column.labels = c( "ref(ccc)", "ref(ccs)", "ref(csc)","ref(css)", "ref(scc)","ref(scs)", "ref(ssc)", "ref(sss)"),
           omit=c("year", "season","texture","Lat","area","native","tmean","ppt","Constant"),
           out="output/tables/table8g.tex")
 
 # stargazer(lm_racawide_ccc, lm_racawide_ccs,lm_racawide_csc, lm_racawide_css, lm_racawide_scc, lm_racawide_scs,lm_racawide_ssc, lm_racawide_sss , type="text",summary=FALSE,
 #           title="The effects of different sequences of corn-soybean rotation on log(SOC(Mg/ha)) stocks",
 #           column.labels = c( "ref(ccc)", "ref(ccs)", "ref(csc)","ref(css)", "ref(scc)","ref(scs)", "ref(ssc)", "ref(sss)"),
 #           out="output/tables/table8g.doc")
 
 rm(lccc, lccs,lcsc, lcss, lscc, lscs,lssc, lsss)
 
library("sandwich")
library("plm")
library("stargazer")
library("lmtest") 
lm_major1_5<-lm(log(SOC) ~  year +season+  factor(texture) + Lat+ factor(area)+ native	+		
                  relevel(as.factor(group), ref="0c") +
                  tmean_3m+tmean_4_12m+
                  ppt_3m+ppt_4_12m,
                data = raca_major1_5)				
lm_major1_5_cl <- coeftest(lm_major1_5, vcov = vcovCL, type = "HC1", cluster = ~ 
                        area)
se_robust_5_cl <- lm_major1_5_cl[, 2]

lm_major1_30<-lm(formula = log(SOC) ~  year +season+  factor(texture) + Lat+ factor(area)+ 		native	+	
                   relevel(as.factor(group), ref="0c") +
                   tmean_3m+tmean_4_12m+
                   ppt_3m+ppt_4_12m,
                 data = raca_major1_30)				


lm_major1_30_cl <- coeftest(lm_major1_30, vcov = vcovCL, type = "HC1", cluster = ~ 
                          area)
se_robust_30_cl <- lm_major1_30_cl[, 2]



lm_major1_100<-lm(formula = log(SOC) ~  year +season+  factor(texture) + Lat+ factor(area)+ 	native	+
                    relevel(as.factor(group), ref="0c") +
                    tmean_3m+tmean_4_12m+
                    ppt_3m+ppt_4_12m,
                  data = raca_major1_100)				

lm_major1_100_cl <- coeftest(lm_major1_100, vcov = vcovCL, type = "HC1", cluster = ~ 
                              area)
se_robust_100_cl <- lm_major1_100_cl[, 2]


lm_racawide_0c<-lm(formula = log(SOCstock100) ~  year + season +  factor(texture) + Lat+ factor(area)+ 	native	+	
                     relevel(as.factor(group), ref="0c") +
                     tmean_3m+tmean_4_12m+
                     ppt_3m+ppt_4_12m,
                   data = raca_major1_100)				
lm_racawide_0c_cl <- coeftest(lm_racawide_0c, vcov = vcovCL, type = "HC1", cluster = ~ 
                               area)
se_robust_0c_cl <-  lm_racawide_0c_cl[, 2]


screenreg(list(lm_major1_5_cl,lm_major1_30_cl,lm_major1_100_cl,lm_racawide_0c_cl ))
stargazer(lm_major1_5,lm_major1_30,lm_major1_100,lm_racawide_0c, type="latex",summary=FALSE,
          title="The effects of different sequences of corn-soybean rotation on log(SOC(Mg/ha)) stocks at different depths",
          column.labels = c("0-5 cm", "5-30 cm", "30-100 cm","0-100 cm"),
          omit=c("year", "season","texture","Lat","area","native","tmean","ppt","Constant"),
          se = list(se_robust_5_cl, se_robust_30_cl, se_robust_100_cl, se_robust_0c_cl),
          out="output/tables/tabledepth.tex")

lm_racawide_1c<-lm(formula = log(SOCstock100) ~  year + season +  factor(texture) + Lat+ factor(area)+ 	native	+	
                     relevel(as.factor(group), ref="1c") +
                     tmean_3m+tmean_4_12m+
                     ppt_3m+ppt_4_12m,
                   data = raca_major1_100)				
lm_racawide_1c_cl <- coeftest(lm_racawide_1c, vcov = vcovCL, type = "HC1", cluster = ~ 
                                area)
se_robust_1c_cl <-  lm_racawide_1c_cl[, 2]


lm_racawide_2c<-lm(formula = log(SOCstock100) ~  year + season +  factor(texture) + Lat+ factor(area)+ 	native	+	
                     relevel(as.factor(group), ref="2c") +
                     tmean_3m+tmean_4_12m+
                     ppt_3m+ppt_4_12m,
                   data = raca_major1_100)				
lm_racawide_2c_cl <- coeftest(lm_racawide_2c, vcov = vcovCL, type = "HC1", cluster = ~ 
                                area)
se_robust_2c_cl <-  lm_racawide_2c_cl[, 2]


lm_racawide_3c<-lm(formula = log(SOCstock100) ~  year + season +  factor(texture) + Lat+ factor(area)+ 	native	+	
                     relevel(as.factor(group), ref="3c") +
                     tmean_3m+tmean_4_12m+
                     ppt_3m+ppt_4_12m,
                   data = raca_major1_100)				
lm_racawide_3c_cl <- coeftest(lm_racawide_3c, vcov = vcovCL, type = "HC1", cluster = ~ 
                                area)
se_robust_3c_cl <-  lm_racawide_3c_cl[, 2]			

screenreg(list(lm_racawide_0c,lm_racawide_1c,lm_racawide_2c,lm_racawide_3c))

stargazer(lm_racawide_0c,lm_racawide_1c,lm_racawide_2c,lm_racawide_3c, type="latex",summary=FALSE,
          title="The effects of different sequences of corn-soybean rotation on log(SOC(Mg/ha)) stocks",
          column.labels = c("ref(0c)", "ref(1c)", "ref(2c)","ref(3c)"),
          omit=c("year", "season","texture","Lat","area","native","tmean","ppt","Constant"),
          se = list(se_robust_0c_cl, se_robust_1c_cl, se_robust_2c_cl, se_robust_3c_cl),
          out="output/tables/table4g.tex")

######## Robustness check
#non-log
l_0c_nolog<-lm(formula = SOCstock100 ~  year + season +  factor(texture) + Lat+ factor(area)+ 	native	+	
                     relevel(as.factor(group), ref="0c") +
                     tmean_3m+tmean_4_12m+
                     ppt_3m+ppt_4_12m,
                   data = raca_major1_100)				
l_0c_nolog_cl <- coeftest(l_0c_nolog, vcov = vcovCL, type = "HC1", cluster = ~ 
                                area)
se_robust_0c_nolog_cl <-  l_0c_nolog_cl[, 2]

## no area fixed effect
l_0c_noarea<-lm(formula = log(SOCstock100) ~  year + season +  factor(texture) + Lat+  	native	+	
                 relevel(as.factor(group), ref="0c") +
                 tmean_3m+tmean_4_12m+
                 ppt_3m+ppt_4_12m,
               data = raca_major1_100)				

## no season fixed effect
l_0c_noseason<-lm(formula = log(SOCstock100) ~  year +  +  factor(texture) + Lat+ factor(area)+  	native	+	
                  relevel(as.factor(group), ref="0c") +
                  tmean_3m+tmean_4_12m+
                  ppt_3m+ppt_4_12m,
                data = raca_major1_100)
l_0c_noseason_cl <- coeftest(l_0c_noseason, vcov = vcovCL, type = "HC1", cluster = ~ 
                            area)
se_robust_0c_noseason_cl <-  l_0c_noseason_cl[, 2]


screenreg(list(l_0c_nolog, lm_racawide_0c,l_0c_noarea, l_0c_noseason ))


stargazer(l_0c_nolog, lm_racawide_0c,l_0c_noarea, l_0c_noseason , type="latex",summary=FALSE,
          title="The effects of different sequences of corn-soybean rotation on log(SOC(Mg/ha)) stocks across model specifications",
          column.labels = c("non-log", "log", "log","log"),
          omit=c("year", "season","texture","Lat","area","native","tmean","ppt","Constant"),
          se = list(se_robust_0c_nolog_cl, se_robust_0c_cl,NULL, se_robust_0c_noseason_cl),
          add.lines=list(c('Area fixed effects', 'Yes','Yes','No','Yes'),c('Season fixed effects', 'Yes','Yes','Yes','No') ),
          out="output/tables/tableRobust.tex")




######## Anova test
res.aov <- raca_major1_100 %>% anova_test(SOCstock100 ~ group)
res.aov

######## t test
## rename 


raca_major1_100$group.recoded<- ifelse(raca_major1_100$group=="0c" ,"group3",
                                       ifelse(raca_major1_100$group=="1c" ,"group2",      
                                              ifelse(raca_major1_100$group=="2c" ,"group1",
                                                     ifelse(raca_major1_100$group=="3c" ,"group0",
                                                            ""))))


#1c
raca_major1_100_1c_0c <- raca_major1_100[raca_major1_100$group=="1c" | raca_major1_100$group=="0c", ]
t1 <- t.test(log(SOCstock100) ~ group.recoded, data = raca_major1_100_1c_0c  , alternative = 'greater')



#2c
raca_major1_100_2c_1c <- raca_major1_100[raca_major1_100$group=="2c" | raca_major1_100$group=="1c", ]
t2 <- t.test(log(SOCstock100) ~ group.recoded, data = raca_major1_100_2c_1c  , alternative = 'greater')

raca_major1_100_2c_0c <- raca_major1_100[raca_major1_100$group=="2c" | raca_major1_100$group=="0c", ]
t3 <- t.test(log(SOCstock100) ~ group.recoded, data = raca_major1_100_2c_0c  , alternative = 'greater')



#3c
raca_major1_100_3c_2c <- raca_major1_100[raca_major1_100$group=="3c" | raca_major1_100$group=="2c", ]
t4 <- t.test(log(SOCstock100) ~ group.recoded, data = raca_major1_100_3c_2c  , alternative = 'greater')

raca_major1_100_3c_1c <- raca_major1_100[raca_major1_100$group=="3c" | raca_major1_100$group=="1c", ]
t5 <- t.test(log(SOCstock100) ~ group.recoded, data = raca_major1_100_3c_1c  , alternative = 'greater')

raca_major1_100_3c_0c <- raca_major1_100[raca_major1_100$group=="3c" | raca_major1_100$group=="0c", ]
t6 <- t.test(log(SOCstock100) ~ group.recoded, data = raca_major1_100_3c_0c  , alternative = 'greater')



library(broom)
library(purrr)

tab <- map_df(list(t1, t2, t3, t4, t5, t6), tidy)
tab[c("estimate", "statistic", "p.value", "conf.low", "conf.high")]




###########propensity score estimation
library(twang)
library(tidyverse)
set.seed(1)


raca_major1_100 <- raca_major1_100 %>% mutate(SOCstock100 = log(SOCstock100))
raca_major1_100$rotation <- factor(raca_major1_100$rotation)
raca_major1_100$group <- factor(raca_major1_100$group)
raca_major1_100$texture_gr <- factor(raca_major1_100$texture_gr)
raca_major1_100$year <- factor(raca_major1_100$year)
raca_major1_100$month <- factor(raca_major1_100$month)
raca_major1_100$texture <- factor(raca_major1_100$texture)
raca_major1_100$season <- factor(raca_major1_100$season)
raca_major1_100$native <- factor(raca_major1_100$native)
raca_major1_100$area <- factor(raca_major1_100$area)

racawide_cornsoybean_nomiss <- raca_major1_100 %>%  # MatchIt does not allow missing values
  dplyr::select(SOCstock100 ,area,group,texture,Lat,tmean_3m,tmean_4_12m,ppt_3m,ppt_4_12m,year,season,native) %>%
  na.omit()

# lm_racawide_0c<-lm(formula = log(SOCstock100) ~  year + season +  factor(texture) + Lat+ factor(area)+ 	native	+	
#                      relevel(as.factor(group), ref="0c") +
#                      tmean_3m+tmean_4_12m+
#                      ppt_3m+ppt_4_12m,
#                    data = raca_major1_100)				
# lm_racawide_0c_cl <- coeftest(lm_racawide_0c, vcov = vcovCL, type = "HC1", cluster = ~ 
#                                 area)
# se_robust_0c_cl <-  lm_racawide_0c_cl[, 2]
###################matching
library(MatchIt)

data <-racawide_cornsoybean_nomiss
colnames(data)[colnames(data) == "group"] ="rotation"
data$treat <-data$rotation
treatments <- levels(data$treat) #Levels of treatment variable
control <- "3c" #Name of control level
data$match.weights <- 1 #Initialize matching weights

for (i in treatments[treatments != control]) {
  d <- data[data$treat %in% c(i, control),] #Subset just the control and 1 treatment
  d$treat_i <- as.numeric(d$treat != i) #Create new binary treatment variable
  m <- matchit(treat_i ~ year + season + 	  texture + Lat+ area + 	tmean_3m+tmean_4_12m+ppt_3m+ppt_4_12m, data = d) ### all no-native
  data[names(m$weights), "match.weights"] <- m$weights[names(m$weights)] #Assign matching weights
}

#Check balance using cobalt
library(cobalt)
bal.tab(treat ~ year + season + 	  texture + Lat+ area + 	tmean_3m+tmean_4_12m+ppt_3m+ppt_4_12m, data = data, 
        weights = "match.weights", method = "matching", 
        focal = control, which.treat = .all)

#Estimate treatment effects

matching_3c <- glm(SOCstock100 ~ relevel(treat, control), 
                   data = data[data$match.weights > 0,], 
                   weights = match.weights)
summary(matching_3c) 
stargazer(matching_0c,matching_1c,matching_2c,matching_3c,type="latex",summary=FALSE,
          title="The effects of different sequences of corn-soybean rotation on log(SOC(Mg/ha)) stocks - Propensity Score Matching",
          column.labels = c("ref(0c)", "ref(1c)", "ref(2c)","ref(3c)"),
          out="output/tables/psm.tex")

###################################################
library(WeightIt)
#w.out <- weightit(treat ~ texture_gr + Lat+	Bulkdensity+tmean + pptmean+year+month+Region, data = data, focal = "ccc", estimand = "ATT")
w.out <- weightit(treat ~ texture_gr + Lat+tmean_3m+tmean_3_6m+tmean_6_9m+tmean_9_12m+ppt_3m+ppt_3_6m+ppt_6_9m+ppt_9_12m+year+month, data = data, estimand = "ATE")
summary(w.out)
trim(w.out,at=5,lower=TRUE)
#Check balance
bal.tab(w.out, which.treat = .all)

#Estimate treatment effects (using jtools to get robust SEs)
#(Can also use survey package)
library(jtools)
summ(glm(SOCstock100 ~ relevel(treat, "fallow"), data = data,
         weights = w.out$weights), robust = "HC1")
