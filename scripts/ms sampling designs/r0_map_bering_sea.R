####################################################################
####################################################################
##
##    Script #0
##    Plot sampling stations for each region
##    Figure 1
##    Baseline strata - existing sampling design
##    Daniel Vilas (danielvilasgonzalez@gmail.com/dvilasg@uw.edu/daniel.vilas@noaa.gov)
##    Lewis Barnett, Stan Kotwicki, Zack Oyafuso, Megsie Siple, Leah Zacher, Lukas Defilippo, Andre Punt
##    
####################################################################
####################################################################

#####################################
# Settings
#####################################

#clear all objects
rm(list = ls(all.names = TRUE)) 
#free up memrory and report the memory usage
gc() 

#libraries from cran to call or install/load
pack_cran<-c('cowplot','ggspatial','raster','rasterVis','rgeos','scales','rnaturalearth','grid','ggplot2','lubridate','ragg','rgdal')

#install pacman to use p_load function - call library and if not installed, then install
if (!('pacman' %in% installed.packages())) {
  install.packages("pacman")}

#install akgfmaps to extract shapefile of Alaska
if (!('akgfmaps' %in% installed.packages())) {
  devtools::install_github('afsc-gap-products/akgfmaps')};library(akgfmaps)

#load/install packages
pacman::p_load(pack_cran,character.only = TRUE)

#set working directory
#out_dir<-'C:/Users/Daniel.Vilas/Work//Adapting Monitoring to a Changing Seascape/'
out_dir<-'/Users/daniel/Work/Adapting Monitoring to a Changing Seascape/'
setwd(out_dir)

# Define plot extent (through trial end error) units km
panel_extent <- data.frame(x = c(-1716559.21, -77636.05), #x = c(-1326559.21, -87636.05),
                           y = c(483099.5, 2194909.7)) #y = c(533099.5, 1894909.7))

#get files from google drive and set up
files<-googledrive::drive_find()
3 #for dvilasg@uw.edu

################################################
# Alaska land shapefile from afgfmaps package
################################################

#Alaska land shapefile layer
ebs_layers <- akgfmaps::get_base_layers(select.region = "ebs", set.crs = "EPSG:3338")
ak_sppoly<-as(ebs_layers$akland, 'Spatial')

#####################################
# Polygon regions shapefiles (EBS, NBS and slope)
#####################################

#create directory
dir.create('./shapefiles/',showWarnings = FALSE)

#name shapefiles 
shfiles<-c('EBSshelfThorson','NBSThorson','EBSslopeThorson')

#get id shared folder from google drive
id.bering.folder<-files[which(files$name=='Shapefiles'),'id']

#list of files and folder
files.1<-googledrive::drive_ls(id.bering.folder$id)

#loop over shapefiles
for (i in shfiles) {
  
  #i=shfiles[1]
  
  id.data<-files.1[which(grepl(i,files.1$name)),]
  
  for (j in 1:nrow(id.data)) {
    
    #download data
    googledrive::drive_download(file=id.data$id[j],
                                path = paste0('./shapefiles/',id.data$name[j]),
                                overwrite = TRUE)
    
  }
  
  #shapefile EBS
  sh<-rgdal::readOGR(dsn='./shapefiles/',layer = i)
  
  if (i=='EBSslopeThorson') {
    
    #reproject shapefile
    proj4string(sh) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") 
    sh<-spTransform(sh,CRSobj = CRS('+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs'))
    
  }
  
  #shapefile name
  shname<-paste0(gsub('Thorson','',i),'_sh')
  
  #assign shapefiles
  assign(shname,sh)
  
}

#merge shapefiles
bs_sh1<-raster::union(EBSshelf_sh,NBS_sh)
bs_sh<-raster::union(bs_sh1,EBSslope_sh)

#####################################
# Depth raster (from gebco) - downloaded in december 2022
#####################################

#create directory
dir.create('./bathymetry/',showWarnings = FALSE)

#get id shared folder from google drive
id.bering.folder<-files[which(files$name=='Bathymetry'),'id']

#list of files and folder
files.1<-googledrive::drive_ls(id.bering.folder$id)
id.data<-files.1[which(files.1$name=='ak_bathy_NAD83.tiff'),]

#download file
googledrive::drive_download(file=id.data$id,
                            path = paste0('./bathymetry/',id.data$name),
                            overwrite = TRUE)

#read raster
ak_bathy_2<-raster('./bathymetry/ak_bathy_NAD83.tiff')

## crop and mask
ak_bathy_3 <- crop(ak_bathy_2, extent(bs_sh1))
ak_bathy_4 <- mask(ak_bathy_3, bs_sh1)

#positive values equal to zero and convert negative to positive values
ak_bathy_4[ak_bathy_4>0]<-0 
ak_bathy_4<--ak_bathy_4 

#to dataframe for plotting purposes
ak_bathy_5<-as.data.frame(ak_bathy_4,xy=TRUE)
ak_bathy_5<-ak_bathy_5[complete.cases(ak_bathy_5$ak_bathy_NAD83),] 

#########################################################
# Exclusive Economic Zone (EEZ) shapefile, if necessary
#########################################################

#create directory
dir.create('./shapefiles/',showWarnings = FALSE)

#get id shared folder from google drive
id.bering.folder<-files[which(files$name=='EEZ'),'id']

#list of files and folder
id.data<-googledrive::drive_ls(id.bering.folder$id)

for (j in 1:nrow(id.data)) {
  
  googledrive::drive_download(file=id.data$id[j],
                              path = paste0('./shapefiles/',id.data$name[j]),
                              overwrite = TRUE)
  
}

#shapefile EEZ
eez_sh<-rgdal::readOGR(dsn='./shapefiles',layer = 'EEZ_Land_v3_202030')

#clip EEZ
bbox = c(latN = 70, latS = 50, lonW = -200, lonE = -150)
pol <- extent(bbox[3],bbox[4], bbox[2],bbox[1])
eez_sh1<-crop(eez_sh, pol)
bbox = c(latN = 70, latS = 50, lonW = 160, lonE = 180)
pol <- extent(bbox[3],bbox[4], bbox[2],bbox[1])
eez_sh2<-crop(eez_sh, pol)

#change CRS projection of EEZ files
proj4string(eez_sh1) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") 
eez_sh1<-spTransform(eez_sh1,CRSobj = CRS('+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs'))
proj4string(eez_sh2) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") 
eez_sh2<-spTransform(eez_sh2,CRSobj = CRS('+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs'))

#####################################
# Sampling stations locations
#####################################

#create directory
dir.create('./data raw/',showWarnings = FALSE)

#get id shared folder from google drive
id.bering.folder<-files[which(files$name=='Bering redesign RWP project'),'id']

#list of files and folder
files.1<-googledrive::drive_ls(id.bering.folder$id)
id.data<-files.1[which(files.1$name=='data raw'),'id']
files.2<-googledrive::drive_ls(id.data$id)

#get haul (stations) data
file<-files.2[grep('haul',files.2$name),]
#file.id<-files.2[which(files.2$name %in% file),]

#download file
googledrive::drive_download(file=file$id,
                            path = paste0('./data raw/',file$name),
                            overwrite = TRUE)

#read csv file
haul<-readRDS(paste0('./data raw/',file$name))
dim(haul);length(unique(haul$hauljoin))

haul$year<-year(as.POSIXlt(haul$date, format="%d/%m/%Y"))

#select year where slope sheld and nbs were carried out
haul1<-subset(haul,year=='2010')

#convert to spatial object
coordinates(haul1)<-c('lon_start','lat_start')
proj4string(haul1) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") 

#change CRS
haul1<-spTransform(haul1,CRSobj = CRS('+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs'))

#convert to dataframe
haul1<-as.data.frame(haul1)

#########################################################
# Plot sampling stations in EBS, NBS and slope in 2010
#########################################################

#plot
ggplot()+
  geom_sf(data=ebs_layers$survey.strata,fill = NA)+
  geom_point(data=haul1,aes(x=lon_start,y=lat_start,fill=survey_name),shape=21)+
  geom_polygon(data=ak_sppoly,aes(x=long,y=lat,group=group),fill = 'grey60')+
  scale_x_continuous(expand = c(0,0),position = 'bottom',
                     breaks = c(-175,-170,-165,-160,-155),sec.axis = dup_axis())+
  geom_polygon(data=NBS_sh,aes(x=long,y=lat,group=group),fill=NA,col='black')+
  geom_polygon(data=EBSshelf_sh,aes(x=long,y=lat,group=group),fill=NA,col='black')+
  geom_polygon(data=EBSslope_sh,aes(x=long,y=lat,group=group),fill=NA,col='black')+
  coord_sf(crs = '+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs',
           #xlim = c(panel_extent$x[1]+200000,panel_extent$x[2]),
           xlim = c(panel_extent$x[1]+200000,panel_extent$x[2]+100000),
           ylim = c(panel_extent$y[1]-100000,panel_extent$y[2]-200000),
           label_axes = "-NE-")+
  theme(aspect.ratio = 1,panel.grid.major = element_line(color = rgb(0, 0, 0,20, maxColorValue = 285), linetype = 'dashed', linewidth =  0.5),
        panel.background = element_rect(fill = NA),panel.ontop = TRUE,text = element_text(size=10),
        legend.background =  element_rect(fill = "transparent", colour = "transparent"),legend.key.height= unit(25, 'points'),
        legend.key.width= unit(25, 'points'),axis.title = element_blank(),legend.position = 'none', #c(0.12,0.47)
        panel.border = element_rect(fill = NA, colour = 'black'),legend.key = element_rect(color="black"),
        axis.text = element_text(color='black'),legend.spacing.y = unit(10, 'points'),
        axis.text.y.right = element_text(hjust= 0.1 ,margin = margin(0,7,0,-25, unit = 'points'),color='black'),
        axis.text.x = element_text(vjust = 6, margin = margin(-7,0,7,0, unit = 'points'),color='black'),
        axis.ticks.length = unit(-5,"points"))+
  scale_fill_manual(values=c("Northern Bering Sea Crab/Groundfish Survey - Eastern Bering Sea Shelf Survey Extension"="#4682B4",
                              "Eastern Bering Sea Crab/Groundfish Bottom Trawl Survey"="#B4464B",
                              "Eastern Bering Sea Slope Bottom Trawl Survey"="#B4AF46"))+
  scale_y_continuous(expand = c(0,0),position = 'right',sec.axis = dup_axis())#+

#####################################
# Baseline strata
#####################################

#load baseline strata
load('./output/baseline_strata.RData')

#corner stations
baseline_strata$locations[grep('GF|HG|JI|IH|ON|QP|PO',baseline_strata$locations$stationid),]
baseline_strata$locations$corner<-ifelse(grepl('GF|HG|JI|IH|ON|QP|PO',baseline_strata$locations$stationid),'TRUE','FALSE')
#tapply(baseline_strata$locations$stratum, baseline_strata$locations$corner, function(x) {length(x[!is.na(x)])})
#aggregate(baseline_strata$locations, by=list(baseline_strata$locations$stratum, baseline_strata$locations$corner), FUN=length)

#sort
baseline_strata$locations<-baseline_strata$locations[order(baseline_strata$locations$cell),]
baseline_strata$locations$difference <- c( NA, diff( baseline_strata$locations$cell ) )
mean(baseline_strata$locations$difference,na.rm=TRUE)/2 #so 50

#baseline_strata
pts<-baseline_strata$locations

#####################################
# EBS+NBS grid 
#####################################

#load grid of NBS and EBS (available from https://github.com/James-Thorson-NOAA/FishStatsUtils/tree/main/data)
load('./extrapolation grids/northern_bering_sea_grid.rda')
load('./extrapolation grids/eastern_bering_sea_grid.rda')
grid<-as.data.frame(rbind(data.frame(northern_bering_sea_grid,region='NBS'),data.frame(eastern_bering_sea_grid,region='EBS')))
grid$cell<-1:nrow(grid)
#add col and row number
x1<-grid[,c('Lon','Lat','cell',"Stratum")]
names(x1)<-c('x','y','z','strata')
coordinates(x1)=~x + y
crs(x1)<-c(crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
x2<-spTransform(x1,'+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')
x3<-data.frame(x2)
x3$x<-as.integer(x3$x)
x3$y<-as.integer(x3$y)
lon<-sort(unique(x3$x),decreasing = FALSE) #1556
lat<-sort(unique(x3$y),decreasing = TRUE) #1507
lons<-data.frame(x=lon,col=1:length(lon))
lats<-data.frame(y=lat,row=1:length(lat))
x4<-merge(x3,lons,by='x',all.x=TRUE)
x5<-merge(x4,lats,by='y',all.x=TRUE)
colnames(x5)<-c('Lat','Lon','cell','strata','optional','col','row')
grid<-x5[,c('Lat','Lon','cell','strata','col','row')]
grid1<-as.data.frame(grid)

######################################################
# Plot study location map - Figure 1; 1st manuscript
######################################################

#segment for pointing islands in the zoomin plot
seg<-data.frame('x'=c(-170,-169,-172.9,-176.7,-179,-168.9,-170.9,-169,-169.5),'y'=c(56.8,55,60.5,62.3, 62.1,54.4,63.5,67,67.5))
coordinates(seg)<- ~x + y
proj4string(seg) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") 
seg1<-spTransform(seg,CRSobj = CRS('+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs'))
seg2<-as.data.frame(seg1)

#zoomin plot
zoomin<-
    ggplot()+
    geom_sf(data=ebs_layers$survey.strata,fill=NA,color='grey30')+
    geom_polygon(data=ak_sppoly,aes(x=long,y=lat,group=group),color='black',linewidth=0.2,fill = 'grey80')+
    geom_segment(aes(x = seg2[1,'x'], y = seg2[1,'y'], xend = seg2[2,'x'], yend = seg2[2,'y']), colour = "black")+
    geom_segment(aes(x = seg2[3,'x'], y = seg2[3,'y'], xend = seg2[4,'x'], yend = seg2[4,'y']), colour = "black")+
    geom_segment(aes(x = seg2[7,'x'], y = seg2[7,'y'], xend = seg2[8,'x'], yend = seg2[8,'y']), colour = "black")+
    geom_point(data=pts,aes(x=Lon,y=Lat),shape=4,size=1)+
    geom_point(data=subset(pts,corner==TRUE),aes(x=Lon,y=Lat),shape=4,size=1,color='red')+
    #geom_polygon(data=eez_sh33,aes(x=long,y=lat,group=group),fill=NA,color='grey40')+
    scale_x_continuous(expand = c(0,0),position = 'bottom',
                       breaks = c(-180,-175,-170,-165,-160,-155),sec.axis = dup_axis())+
    geom_polygon(data=NBS_sh,aes(x=long,y=lat,group=group),fill=NA,col='black')+
    geom_polygon(data=EBSshelf_sh,aes(x=long,y=lat,group=group),fill=NA,col='black')+
    #geom_polygon(data=EBSslope_sh,aes(x=long,y=lat,group=group),fill=NA,col='black')+
    coord_sf(crs = '+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs',
             xlim = panel_extent$x,
             ylim = panel_extent$y,
             label_axes = "-NE-")+
    scale_fill_gradient2(low = '#B2EBF2','grey90',#'#c1f2fe',
                        high = '#006064',#'#007c9b',
                       limits=c(0,200),oob = scales::squish,breaks=c(0,50,100,200),
                       labels=c('0','50','100',paste0('200')), #round(maxValue(ak_bathy_4))
                        na.value = 'white',
                        name='depth (m)',
                        guide = guide_colorbar(frame.colour = 'black',ticks.colour = 'black'))+
    theme(aspect.ratio = 1,panel.grid.major = element_line(color = rgb(0, 0, 0,20, maxColorValue = 285), linetype = 'dashed', linewidth =  0.5),
          panel.background = element_rect(fill = NA),panel.ontop = TRUE,text = element_text(size=12),
          legend.background =  element_rect(fill = "transparent", colour = "transparent"),legend.key.height= unit(25, 'points'),
          legend.key.width= unit(25, 'points'),axis.title = element_blank(),legend.position = c(0.12,0.47), #c(0.12,0.47)
          panel.border = element_rect(fill = NA, colour = 'black'),legend.key = element_rect(color="black"),
          axis.text = element_text(color='black'),legend.spacing.y = unit(10, 'points'),
          axis.text.y.right = element_text(hjust= 0.1 ,margin = margin(0,7,0,-25, unit = 'points'),color='black'),
          axis.text.x = element_text(vjust = 6, margin = margin(-7,0,7,0, unit = 'points'),color='black'),
          axis.ticks.length = unit(-5,"points"))+
    annotate("text", x = -256559, y = 1354909, label = "Alaska",parse=TRUE,size=7)+
    annotate("text", x = -1376559, y = 2049090, label = "Russia",parse=TRUE,size=7)+
    annotate("text", x = -816559, y = 1454909, label = "NBS",parse=TRUE,size=7)+
    annotate("text", x = -816559, y = 1024909, label = "EBS",size=7)+
    annotate("text", x = seg2[5,'x'], y = seg2[5,'y'], label = "St. Matthew\nIsland",size=5,lineheight = 0.9)+
    annotate("text", x = seg2[6,'x'], y = seg2[6,'y'], label = "Pribilof\nIslands",size=5,lineheight = 0.9)+
    annotate("text", x = seg2[9,'x'], y = seg2[9,'y'], label = "St. Lawrence\nIsland",size=5,lineheight = 0.9)+
    annotation_scale(location='tr')+
    scale_y_continuous(expand = c(0,0),position = 'right',sec.axis = dup_axis())+
    annotate("text", x = -1376559, y = 744900, label = "italic('Bering Sea')",parse=TRUE,size=9)

#extract countries and lake for the zoomout plot
count<-ne_countries(scale = 'large',
                    country = c('united states of america',"russia","canada",'mexico'),
                    returnclass = c( "sf"))
lakes <- ne_download(category = "physical", type = "lakes", returnclass = "sf", 
                     scale = 'small')
count<-as(count, 'Spatial')
lakes<-as(lakes,'Spatial')

#reproject shapefiles
count83<-spTransform(count,CRSobj = CRS('+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs'))
lakes83<-spTransform(lakes,CRSobj = CRS('+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs'))

#zoomout plot
zoomout<-
  ggplot() + 
    geom_polygon(data=count83,aes(x=long,y=lat,group=group),color='black',linewidth=0.2,fill='grey80') + 
    geom_polygon(data=lakes83,aes(x=long,y=lat,group=group),color='black',linewidth=0.2,fill='white') + 
    scale_y_continuous(expand = c(0,0))+
    scale_x_continuous(expand = c(0,0),
                       breaks = c(160,170,-180,-170,-160,-150,-140,-130))+
    coord_sf(crs ='+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +no_defs',
             xlim=c(-3500000, 3500000),ylim = c(-2000000, 3000000))+
    theme(panel.grid.major = element_line(color = rgb(0, 0, 0,20, maxColorValue = 285), linetype = 'dashed', linewidth =  0.5),
          panel.ontop = TRUE,text = element_text(size=10),
          legend.background =  element_rect(fill = "transparent", colour = "transparent"),legend.key.height= unit(20, 'points'),
          legend.key.width= unit(20, 'points'),axis.title = element_blank(),legend.position = c(0.12,0.155),
          panel.border = element_rect(fill = NA, colour = 'black'),
          axis.text = element_blank(),axis.ticks.length = unit(-3, "points"),
          panel.background = element_rect(fill = NA),plot.margin=grid::unit(c(0,0,0,0), "mm"))+
    geom_rect(aes(xmin = panel_extent$x[1], xmax = panel_extent$x[2], ymin = panel_extent$y[1], ymax = panel_extent$y[2]),
          colour = '#800000', linetype='solid', fill = NA,linewidth=0.7) +
    annotate("text", x = -2300000, y = 2600000, label = "Russia",parse=TRUE,size=4)+
    annotate("text", x = 0, y = -1000000, label = "italic('Pacific Ocean')",parse=TRUE,size=4)+
    annotate("text", x = +2000000, y = 1400000, label = "Canada",parse=TRUE,size=4)+
    annotate("text", x = +3000000, y = 0, label = "USA",parse=TRUE,size=4)

#save plot
dir.create('./figures/')
agg_png(paste0('./figures/map_bering6.png'), width = 7, height = 7, units = "in", res = 300)
grid.newpage()
vp_b <- viewport(width = 1, height = 1, x = 0.5, y = 0.5)  # the larger map
vp_a <- viewport(width = 0.4, height = 0.3, x = 0.217, y = 0.846)  # the inset in upper left
print(zoomin , vp = vp_b)
print(zoomout , vp = vp_a)
dev.off()

#depth miniplot
pd<-
ggplot()+
  geom_raster(data=ak_bathy_5,aes(x=x,y=y,fill=ak_bathy_NAD83))+
  geom_polygon(data=ak_sppoly,aes(x=long,y=lat,group=group),color='black',linewidth=0.2,fill = 'grey80')+
  scale_x_continuous(expand = c(0,0),position = 'bottom',
                     breaks = c(-180,-175,-170,-165,-160,-155),sec.axis = dup_axis())+
  geom_polygon(data=NBS_sh,aes(x=long,y=lat,group=group),linewidth=0.3,fill=NA,col='black')+
  geom_polygon(data=EBSshelf_sh,aes(x=long,y=lat,group=group),linewidth=0.3,fill=NA,col='black')+
  coord_sf(crs = '+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs',
           xlim = panel_extent$x,
           ylim = c(panel_extent$y[1],panel_extent$y[2]-300000),
           label_axes = "-NE-")+
  scale_fill_gradient2(high = '#10715e',mid='#1ABC9C',low = 'white',midpoint = 100,#'#007c9b',
                       limits=c(5,200),oob = scales::squish,breaks=c(5,50,100,200),
                       labels=c('0','50','100',paste0('200')), #round(maxValue(ak_bathy_4))
                       na.value = 'white',
                       name='depth (m)',
                       guide = guide_colorbar(frame.colour = 'black',ticks.colour = 'black'))+
  theme(aspect.ratio = 1,panel.grid.major = element_line(color = rgb(0, 0, 0,20, maxColorValue = 285), linetype = 'dashed', linewidth =  0.5),
        panel.background = element_rect(fill = NA),panel.ontop = TRUE,text = element_text(size=12),
        legend.background =  element_rect(fill = "transparent", colour = "transparent"),legend.key.height= unit(20, 'points'),
        legend.key.width= unit(15, 'points'),axis.title = element_blank(),legend.position = c(0.13,0.55), #c(0.12,0.47)
        panel.border = element_rect(fill = NA, colour = 'black'),legend.key = element_rect(color="black"),
        axis.text = element_text(color='black'),legend.spacing.y = unit(10, 'points'),
        axis.text.y.right = element_text(hjust= 0.1 ,margin = margin(0,0,0,-28, unit = 'points'),color='black'),
        axis.text.x = element_text(vjust = 6, margin = margin(-4,0,0,0, unit = 'points'),color='black'),
        axis.ticks.length = unit(-5,"points"))+
  scale_y_continuous(expand = c(0,0),position = 'right',sec.axis = dup_axis(),breaks = c(65,60,55))#+


#load optim file (in script #6 to get meanTemp and varSBT)
load('./output/multisp_optimization_static_data_ebsnbs.RData')
head(df)
coordinates(df) <- ~ Lon + Lat
proj4string(df)<-CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#proj4string(strata_pol) <- CRS('+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
df1<-spTransform(df,CRSobj = CRS('+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs'))
df1<-as.data.frame(df1,xy=TRUE)

#varSBT miniplot
pv<-
ggplot()+
  geom_point(data=df1,aes(x=Lon,y=Lat,color=varTemp),size=0.3)+
  geom_polygon(data=ak_sppoly,aes(x=long,y=lat,group=group),color='black',linewidth=0.2,fill = 'grey80')+
  scale_x_continuous(expand = c(0,0),position = 'bottom',
                     breaks = c(-180,-175,-170,-165,-160,-155),sec.axis = dup_axis())+
  geom_polygon(data=NBS_sh,aes(x=long,y=lat,group=group),linewidth=0.3,fill=NA,col='black')+
  geom_polygon(data=EBSshelf_sh,aes(x=long,y=lat,group=group),linewidth=0.3,fill=NA,col='black')+
  coord_sf(crs = '+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs',
           xlim = panel_extent$x,
           ylim = c(panel_extent$y[1],panel_extent$y[2]-300000),
           label_axes = "-NE-")+
  scale_color_gradient2(high = '#3498DB',#'#007c9b',
                       name='varSBT (°C)',
                       guide = guide_colorbar(frame.colour = 'black',ticks.colour = 'black'))+
  theme(aspect.ratio = 1,panel.grid.major = element_line(color = rgb(0, 0, 0,20, maxColorValue = 285), linetype = 'dashed', linewidth =  0.5),
        panel.background = element_rect(fill = NA),panel.ontop = TRUE,text = element_text(size=12),
        legend.background =  element_rect(fill = "transparent", colour = "transparent"),legend.key.height= unit(20, 'points'),
        legend.key.width= unit(15, 'points'),axis.title = element_blank(),legend.position = c(0.16,0.55), #c(0.12,0.47)
        panel.border = element_rect(fill = NA, colour = 'black'),legend.key = element_rect(color="black"),
        axis.text = element_text(color='black'),legend.spacing.y = unit(10, 'points'),
        axis.text.y.right = element_text(hjust= 0.1 ,margin = margin(0,0,0,-28, unit = 'points'),color='black'),
        axis.text.x = element_text(vjust = 6, margin = margin(-4,0,0,0, unit = 'points'),color='black'),
        axis.ticks.length = unit(-5,"points"))+
  scale_y_continuous(expand = c(0,0),position = 'right',sec.axis = dup_axis(),breaks = c(65,60,55))#+

#meanTemp miniplot
pt<-
  ggplot()+
  geom_point(data=df1,aes(x=Lon,y=Lat,color=meanTemp),size=0.3)+
  geom_polygon(data=ak_sppoly,aes(x=long,y=lat,group=group),color='black',linewidth=0.2,fill = 'grey80')+
  scale_x_continuous(expand = c(0,0),position = 'bottom',
                     breaks = c(-180,-175,-170,-165,-160,-155),sec.axis = dup_axis())+
  geom_polygon(data=NBS_sh,aes(x=long,y=lat,group=group),linewidth=0.3,fill=NA,col='black')+
  geom_polygon(data=EBSshelf_sh,aes(x=long,y=lat,group=group),linewidth=0.3,fill=NA,col='black')+
  coord_sf(crs = '+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs',
           xlim = panel_extent$x,
           ylim = c(panel_extent$y[1],panel_extent$y[2]-300000),
           label_axes = "-NE-")+
  scale_color_gradient2(low = 'grey90',#'#c1f2fe',
    high = '#556111',#'#007c9b',
    name='SBT (°C)',
    guide = guide_colorbar(frame.colour = 'black',ticks.colour = 'black'))+
  theme(aspect.ratio = 1,panel.grid.major = element_line(color = rgb(0, 0, 0,20, maxColorValue = 285), linetype = 'dashed', linewidth =  0.5),
        panel.background = element_rect(fill = NA),panel.ontop = TRUE,text = element_text(size=12),
        legend.background =  element_rect(fill = "transparent", colour = "transparent"),legend.key.height= unit(20, 'points'),
        legend.key.width= unit(15, 'points'),axis.title = element_blank(),legend.position = c(0.16,0.55), #c(0.12,0.47)
        panel.border = element_rect(fill = NA, colour = 'black'),legend.key = element_rect(color="black"),
        axis.text = element_text(color='black'),legend.spacing.y = unit(10, 'points'),
        axis.text.y.right = element_text(hjust= 0.1 ,margin = margin(0,0,0,-28, unit = 'points'),color='black'),
        axis.text.x = element_text(vjust = 6, margin = margin(-4,0,0,0, unit = 'points'),color='black'),
        axis.ticks.length = unit(-5,"points"))+
  scale_y_continuous(expand = c(0,0),position = 'right',sec.axis = dup_axis(),breaks = c(65,60,55))#+

#combine two plots
pp<-plot_grid(pd,pv,ncol=1)

#save env plot
agg_png(paste0('./figures/env_map.png'), width = 4, height = 7, units = "in", res = 300)
print(pp)
dev.off()

#####################################
# Slope strata
#####################################

#get slope data
slope<-akgfmaps::get_base_layers(select.region = 'ebs.slope', set.crs = "EPSG:3338")
slop_sppoly<-as(slope$survey.strata, 'Spatial')

#strata for depth and subarea, since we are only using 200-400 and 6 subareas
plot(slop_sppoly['STRATUM'])
length(unique(slop_sppoly$STRATUM))

#get stratums wfor 200-400m slope
slope200_400<-sort(unique(slop_sppoly$STRATUM))[c(1,6,11,16,21,27)]
#slop_sppoly[,"STRATUM"][slop_sppoly[,"STRATUM"] %in% slope200_400,]

#get stratum and sf object
xx<-slop_sppoly['STRATUM']
xx1<-xx[xx$STRATUM %in% slope200_400,]
xx2<-as(xx1, "sf")

#plot
ggplot()+
  geom_sf(data=xx2,aes(fill=as.factor(STRATUM)))+
  geom_polygon(data=ak_sppoly,aes(x=long,y=lat,group=group),fill = 'grey60')+
  coord_sf(crs = '+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs',
           #xlim = c(panel_extent$x[1]+200000,panel_extent$x[2]),
           xlim = c(panel_extent$x[1]+200000,panel_extent$x[2]+100000),
           ylim = c(panel_extent$y[1]-100000,panel_extent$y[2]-200000),
           label_axes = "-NE-")

#################################################
# CREATE DATA SAMPLING SCENARIO BASELINE (existing)
#################################################

#load grid of NBS and EBS
load('./extrapolation grids/northern_bering_sea_grid.rda')
load('./extrapolation grids/eastern_bering_sea_grid.rda')
grid<-as.data.frame(rbind(data.frame(northern_bering_sea_grid,region='NBS'),data.frame(eastern_bering_sea_grid,region='EBS')))
grid$cell<-1:nrow(grid)
#df to spatialpoint df
coordinates(grid) <- ~ Lon + Lat
crs(grid)<-c(crs='+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs')
#reproject coordinates for plotting purposes
D2_1<-grid
D2_2<-data.frame(D2_1)
#x and y cells
xycells<-as.integer(sqrt(dim(D2_1)[1]))
# create a template raster
r1 <- raster(ext=extent(D2_1),ncol=xycells, nrow=xycells) #c(15800,15800) 7000
#create raster
r2<-rasterize(D2_1, r1 ,field='cell')
#plot(r2)

#EBS and NBS layer
ebs_layers <- akgfmaps::get_base_layers(select.region = "ebs", set.crs = "EPSG:3338")

#baseline strata areas
strata_areas<-as.data.frame(ebs_layers$survey.strata)
sum(strata_areas$F_AREA)
sum(strata_areas$Precise_Ar/1000000)

#dataframe stratum and area
strata_areas <- data.frame('Stratum'=strata_areas$Stratum,'Area_in_survey_km2'=strata_areas$Precise_Ar/1000000)
sum(strata_areas$Area_in_survey_km2)

#strata polygon
strata_pol<-as(ebs_layers$survey.strata, 'Spatial')
proj4string(strata_pol) <- CRS('+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
strata_pol<-spTransform(strata_pol,CRSobj = CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs "))

#locations
x<-ebs_layers$survey.grid
st<-x$STATIONID
xx<-st_geometry(x)
xxx<-st_centroid(xx)
#plot(xxx);class(xxx)
coords <- st_coordinates(xxx)
lat <- coords[, 2]
lon <- coords[, 1]
# plot(lon,lat)
# text(lon, lat, st, pos = 3)
baseline<-data.frame('Lat'=lat,'Lon'=lon,'stationid'=st)
#corner stations
corner<- c('GF','HG','IH','QP','JI','ON','PO')
st.corner<-paste(corner,collapse = '|')
baseline$corner<-ifelse(grepl(st.corner,baseline$stationid),TRUE,FALSE)
#locations of stations
locations <- as.data.frame(baseline)
st<-baseline
coordinates(st)<- ~ Lon + Lat
proj4string(st) <- CRS('+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
st<-spTransform(st,CRSobj = CRS('+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs'))
#cell
locations$cell<-extract(r2,st)
st1<-as.data.frame(st)[,c("coords.x1","coords.x2")]#[,c("Lon","Lat")]
names(st1)<-c('x','y')
xy<-st1
sampled = apply(X = xy, MARGIN = 1, FUN = function(xy) r2@data@values[which.min(replace(distanceFromPoints(r2,xy), is.na(r2), NA))])
locations$cell<-sampled
locations$Stratum<-over(st,strata_pol)[,'Stratum']

#number of samples per strata for random sampling
y<-aggregate(locations$cell,by=list(locations$Stratum),length)
yc<-aggregate(subset(locations,corner!=TRUE)[,'cell'],by=list(subset(locations,corner!=TRUE)[,'Stratum']),length)
n_samples<-data.frame('stratum'=yc$Group.1,'scnbase'=y$x,'scnbase_bis'=yc$x)

#list baseline strata
baseline_strata<-list(strata_areas=strata_areas,locations=locations,n_samples=n_samples,cell_strata=as.data.frame(grid))

#create directory
dir.create('./output/',showWarnings = FALSE)

#save data
save(baseline_strata,file='./output/baseline_strata.RData')
