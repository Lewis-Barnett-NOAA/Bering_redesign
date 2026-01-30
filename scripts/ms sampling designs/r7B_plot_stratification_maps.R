####################################################################
####################################################################
##
##    Run sampling optimization based on predicted densities from VAST model
##    and get samples from each strata for each sampling design
##
##    by best stratification, we mean the stratification that ensures the minimum sample cost,
##    sufficient to satisfy precision constraints set on the accuracy of the estimates of the survey target variables Y’s
##    constraints expressed as maximum allowable coefficients of variation in the different domains of interest
##
##    https://cran.r-project.org/web/packages/SamplingStrata/vignettes/SamplingStrata.html
##    (using devtools::install_github(repo = "zoyafuso-NOAA/SamplingStrata"))
##    Daniel Vilas (daniel.vilas@noaa.gov/dvilasg@uw.edu)
##
####################################################################
####################################################################

#clear all objects
rm(list = ls(all.names = TRUE))
#free up memrory and report the memory usage
gc()

#libraries from cran to call or install/load
pack_cran<-c("splines",'SamplingStrata','wesanderson','dplyr','sp',
             'sf','maptools','parallel','rasterVis','rgeos','scales',
             'rnaturalearth','grid','ggplot2','spatstat','raster')

#install pacman to use p_load function - call library and if not installed, then install
if (!('pacman' %in% installed.packages())) {
  install.packages("pacman")}

#install akgfmaps to extract shapefile of Alaska
if (!('akgfmaps' %in% installed.packages())) {
  devtools::install_github('afsc-gap-products/akgfmaps')};library(akgfmaps)

#install coldpool to extract SBT for the EBS
if (!('VAST' %in% installed.packages())) {
  devtools::install_github("james-thorson/VAST@main", INSTALL_opts="--no-staged-install")};library(VAST)

#load/install packages
pacman::p_load(pack_cran,character.only = TRUE)

#setwd
out_dir<-'C:/Users/Daniel.Vilas/Work/Adapting Monitoring to a Changing Seascape/'
out_dir<-'/Users/daniel/Work/Adapting Monitoring to a Changing Seascape/'
setwd(out_dir)

#version VAST (cpp)
version<-'VAST_v14_0_1'

#selected species
spp<-c('Limanda aspera',
       'Gadus chalcogrammus',
       'Gadus macrocephalus',
       'Atheresthes stomias',
       'Reinhardtius hippoglossoides',
       'Lepidopsetta polyxystra',
       'Hippoglossoides elassodon',
       'Pleuronectes quadrituberculatus',
       'Hippoglossoides robustus',
       'Boreogadus saida',
       'Eleginus gracilis',
       'Anoplopoma fimbria',
       'Chionoecetes opilio',
       'Paralithodes platypus',
       'Paralithodes camtschaticus',
       'Chionoecetes bairdi')

#remove Anoploma and Reinhardtius because habitat preference reasons
spp<-setdiff(spp, c('Anoplopoma fimbria','Reinhardtius hippoglossoides'))

spp1<-c('Yellowfin sole',
        'Alaska pollock',
        'Pacific cod',
        'Arrowtooth flounder',
        #'Greenland turbot',
        'Northern rock sole',
        'Flathead sole',
        'Alaska plaice',
        'Bering flounder',
        'Arctic cod',
        'Saffron cod',
        #'Sablefish',
        'Snow crab',
        'Blue king crab',
        'Red king crab',
        'Tanner crab')

spp_name<-data.frame('spp'=spp,
                     'common'=spp1) 


#df spp, number and target variables
df_spp<-data.frame('spp'=spp,
                   'n'=c(1:length(spp)),
                   'Y'=paste0('Y',c(1:length(spp))))

#number sp
n_spp<-length(spp)


##############################
#OBJECTS FOR PLOTTING
#############################

# Define plot extent (through trial end error) units km
panel_extent <- data.frame(x = c(-1716559.21, -77636.05), #x = c(-1326559.21, -87636.05),
                           y = c(483099.5, 2194909.7)) #y = c(533099.5, 1894909.7))

#Alaska layer
ebs_layers <- akgfmaps::get_base_layers(select.region = "ebs", set.crs = "EPSG:3338")
ak_sppoly<-as(ebs_layers$akland, 'Spatial')

#create directory
dir.create('./shapefiles/',showWarnings = FALSE)

#name shapefiles
shfiles<-c('EBSshelfThorson','NBSThorson','EBSslopeThorson')

#get files from google drive and set up
files<-googledrive::drive_find()
3 #for dvilasg@uw.edu

#get id shared folder from google drive
id.bering.folder<-files[which(files$name=='Shapefiles'),'id']

#list of files and folder
files.1<-googledrive::drive_ls(id.bering.folder$id)

#loop over shapefiles
for (i in shfiles) {
  
  #i=shfiles[1]
  
  #identify files
  id.data<-files.1[which(grepl(i,files.1$name)),]
  
  #loop over files
  for (j in 1:nrow(id.data)) {
    
    #if not file, download
    if (!(id.data$name[j] %in% list.files('./shapefiles/'))) {
      #download data
      googledrive::drive_download(file=id.data$id[j],
                                  path = paste0('./shapefiles/',id.data$name[j]),
                                  overwrite = TRUE)}
    
  }
  
  #shapefile EBS
  sh<-rgdal::readOGR(dsn='./shapefiles/',layer = i)
  
  #if slope reproject
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
bs_sh1<-terra::union(EBSshelf_sh,NBS_sh)
bs_sh<-terra::union(bs_sh1,EBSslope_sh)
nbs_sh<-NBS_sh
ebs_sh<-EBSshelf_sh

#color palette
pal<-wesanderson::wes_palette('Zissou1',15,type='continuous')

#####################################
# Get current ebs and NBS stations
#####################################

#create directory
dir.create('./additional/',showWarnings = FALSE)

#get id shared folder from google drive
id.bering.folder<-files[which(files$name=='Additional'),'id']

#list of files and folder
files.1<-googledrive::drive_ls(id.bering.folder$id)
id.data<-files.1[which(files.1$name=='ebs_nbs_temperature_full_area.csv'),]

#download file
googledrive::drive_download(file=id.data$id,
                            path = paste0('./additional/',id.data$name),
                            overwrite = TRUE)

#extract station EBS bottom trawl
st_EBS<-read.csv('./additional//ebs_nbs_temperature_full_area.csv')

#filter 2019 stations, an example year where EBS and NBS surveys were carried out
st_EBS<-subset(st_EBS,year==2019 ) #& survey_definition_id ==98

#convert to spatial object
coordinates(st_EBS)<-c('longitude','latitude')
proj4string(st_EBS) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

#change CRS
st_EBS1<-spTransform(st_EBS,CRSobj = CRS('+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs'))

#convert to dataframe
st_EBS<-as.data.frame(st_EBS)
st_EBS$survey_definition_id<-as.factor(st_EBS$survey_definition_id)

#convert to dataframe
st_EBS1<-as.data.frame(st_EBS1)
st_EBS1$survey_definition_id<-as.factor(st_EBS1$survey_definition_id)
st_EBS1<-st_EBS1[which(st_EBS1$survey_definition_id=='98'),]

#remove some polygons of EEZ object
eez_sh11 <- eez_sh1[eez_sh1$AREA_KM2 == 5193061,]
eez_sh22 <- eez_sh2[eez_sh2$AREA_KM2 == 5193061,]  #"5193061"  "24614858" "8521"    

#join both polygons without inner line
eez_sh3<-aggregate(rbind(eez_sh11,eez_sh22),dissolve=T)
eez_sh33<-rgeos::gUnaryUnion(eez_sh3)

#corner stations
st_corners1<-st_EBS1[which(nchar(st_EBS1$stationid)>=6 & st_EBS1$stationid!='AZ0504'),]
st_EBS2<-st_EBS1[which(nchar(st_EBS1$stationid)<=5 | st_EBS1$stationid=='AZ0504'),]

#change projection of spatial object
coordinates(st_EBS)<- ~ longitude + latitude

#reproject shapefile
proj4string(st_EBS) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
st_EBS<-spTransform(st_EBS,CRSobj = CRS('+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs'))

#to dataframe
st_EBS<-as.data.frame(st_EBS)

###################################
# LOAD GRID EBS (remember to keep the same order as in fit_model if multiple grids)
###################################

#load grid of NBS and EBS
load('./extrapolation grids/northern_bering_sea_grid.rda')
load('./extrapolation grids/eastern_bering_sea_grid.rda')
grid<-as.data.frame(rbind(data.frame(northern_bering_sea_grid,region='NBS'),data.frame(eastern_bering_sea_grid,region='EBS')))
grid$cell<-1:nrow(grid)
#add col and row number
x1<-grid[,c('Lon','Lat','cell','Area_in_survey_km2')]
names(x1)<-c('x','y','z','area')
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
colnames(x5)<-c('Lat','Lon','cell','area','optional','col','row')
grid<-x5[,c('Lat','Lon','cell','area','col','row')]

###################################
# BASELINE/CURRENT SAMPLING DESIGN
###################################

load('./output/baseline_strata.RData') #baseline_strata

###################################
# Sampling designs (from script #11) 
###################################

#sampling scenarios
samp_df<-expand.grid(strat_var=c('Depth_varTemp','varTemp','Depth'),
                     target_var=c('sumDensity'), #,'sqsumDensity'
                     n_samples=c(520), #c(300,500) 520 (EBS+NBS+CRAB);26 (CRAB); 350 (EBS-CRAB); 494 (NBS-CRAB)
                     n_strata=c(15),
                     stringsAsFactors = FALSE) #c(5,10,15)

#add scenario number
samp_df$samp_scn<-paste0(paste0('scn',1:nrow(samp_df)))
samp_df<-rbind(samp_df,c('baseline','current',520,15,'scnbase'),
               c('baseline w/o corner','current',494,15,'scnbase_bis'))

samp_df$n<-1:nrow(samp_df)

###################################
# Crab shapefiles
###################################

# Set the path to the geodatabase file
gdb_path <- "./shapefiles/CrabStrataShapefiles_GAPgrid.gdb/"

# List the layers/tables in the geodatabase file
gdb_layers <- st_layers(gdb_path)

# Select the specific table you want to read
selected_layer <- gdb_layers$name[1]

# Read the selected table from the geodatabase
#"BBRKC_strata"        "Pribilof_BKC_strata" "Pribilof_RKC_strata" "StMatt_BKC_strata"   "Norton_RKC_Strata"  
#[6] "EBS_CO_CB_strata"   
gdb_table1 <- st_read(dsn = gdb_path, layer = gdb_layers$name[1])
gdb_table2 <- st_read(dsn = gdb_path, layer = gdb_layers$name[2])
gdb_table3 <- st_read(dsn = gdb_path, layer = gdb_layers$name[3])
gdb_table4 <- st_read(dsn = gdb_path, layer = gdb_layers$name[4])
gdb_table5 <- st_read(dsn = gdb_path, layer = gdb_layers$name[5])
gdb_table6 <- st_read(dsn = gdb_path, layer = gdb_layers$name[6])

###########################
# demo strat and alloc samples 
###########################

#5rows - str x 3columns - apr 

#load optimization data
load(paste0('./output/multisp_optimization_static_data.RData')) #df
#load(paste0('./output/species/',sp,'/projection_data.RData')) #temp_dens_vals
D6<-df
#removed cells because of depth
rem_cells<-D6[which(D6$include==FALSE),'cell']
ok_cells<-D6[which(D6$include==1),'cell']

plot_list<- list()
plot_list_str<-list()
raster_list<-list()

samp_df<-samp_df[order(match(samp_df$samp_scn,c('scnbase','scnbase_bis','scn3','scn2','scn1'))),]

for (samp in samp_df$samp_scn[3:5]) {
  
  #samp<-samp_df$samp_scn[3]
  
  load(file = paste0("./output/survey_allocations_",samp,".RData")) #scn_allocations
  #load(file = paste0("./output/survey_allocations_scn1.RData")) #scn_allocations
  
  #rename due to error
  dimnames(scn_allocations)[[2]][1:2]<-c("Lon","Lat")
  
  #allocation for survey 1
  scn_allocations1<-scn_allocations[scn_allocations[,'sur','sys']==1,,]
  
  #SYSTEMATIC POINTS EXAMPLE
  #some systematic allocation have wrong lat and lot in terms of CRS
  sys<-data.frame(scn_allocations1[,,'sys'])
  #dim(sys[which(sys$Lat > 66 & sys$Lon < -179),]) #to transform
  #dim(sys[which(sys$Lat < 66 & sys$Lon > -179),])
  syst<-sys[which(sys$Lat > 66 & sys$Lon < -179),]
  coordinates(syst)<-~Lon+Lat
  crs(syst)<-c(crs='+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')
  syst<-spTransform(syst,"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  sys1<-sys[which(sys$Lat < 66 & sys$Lon > -179),]
  sys<-rbind(sys1,as.data.frame(syst))[,c('Lon',"Lat",'strata')]
  #plot(sys$Lon,sys$Lat)
  coordinates(sys) <- ~ Lon + Lat
  crs(sys)<-c(crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  #reproject coordinates for plotting purposes
  sys1<-spTransform(sys,'+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')
  sys1<-data.frame(sys1)
  #####SPATIALLY-BALANCED SURVEY

  #RANDOM POINTS EXAMPLE
  rand<-data.frame(scn_allocations1[,,'rand'])
  coordinates(rand) <- ~ Lon + Lat
  crs(rand)<-c(crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  #reproject coordinates for plotting purposes
  rand1<-spTransform(rand,CRS('+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs'))
  rand1<-data.frame(rand1)

  #SPB POINTS EXAMPLE
  sb<-data.frame(scn_allocations1[,,'sb'])
  #df to spatialpoint df
  coordinates(sb) <- ~ Lon + Lat
  crs(sb)<-c(crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  #reproject coordinates for plotting purposes
  sb<-spTransform(sb,'+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')
  sb<-data.frame(sb)
  
  if (samp %in% paste0('scn',1:3)) {
    #STRATIFICATION
    #load files for stratification (all) and example allocations
    load(file = paste0("./output/ms_optim_allocations_",samp,".RData")) #all
    
    strata<-rbind(all$result_list$solution$indices,
                  data.frame(ID=rem_cells,X1=NA))
    colnames(strata)<-c('cell','Strata')
    
    strata<-merge(strata,grid,by='cell')
    
    #n cells by strata
    strata_sum<-aggregate(cell ~ Strata, data = strata, FUN = length)
    strata_area<-aggregate(area ~ Strata, data = strata, FUN = sum)
    names(strata_sum)[2]<-'total_cell'
    strata_agr<-cbind(strata_sum,'total_area'=strata_area[,'area'])
    
    #merge
    strata<-merge(strata,all$samples_strata,by.x='Strata',by.y='strata')
    strata<-merge(strata,strata_agr,by='Strata')
    strata$samp_cell<-strata$n_samples/strata$total_cell
    dim(strata)
    
    D8<-merge(D6,strata,by=c('cell'))
    D8$Strata[is.na(D8$Strata)]<-99
    D8$samp_area<-D8$n_samples/D8$total_area
    #D8<-D8[,c(1:40,43:50)]
    names(D8)[c(2,3)]<-c('Lat','Lon')
    
  } else {

      strata<-as.data.frame(baseline_strata$cell_strata)[,c('cell','Lat','Lon','Stratum','Area_in_survey_km2')]
      names(strata)[c(4,5)]<-c('Strata','area')
      strata_area<-aggregate(area ~ Strata, data = strata, FUN = sum)
      
      #aggregate(strata$cell,by=list(strata$strata),FUN=length)
      D8<-strata
      
      #allocations<-
      if(samp=='scnbase'){
        allocations<-data.frame('Strata'=baseline_strata$n_samples$stratum,
                                'n_samples'=baseline_strata$n_samples$scnbase)
      }else{
        allocations<-data.frame('Strata'=baseline_strata$n_samples$stratum,
                                'n_samples'=baseline_strata$n_samples$scnbase_bis)
      }
      
      D8<-merge(D8,allocations,by='Strata',all.x=TRUE)
      
      #n cells by strata
      strata_sum<-aggregate(cell ~ Strata, data = D8, FUN = length)
      names(strata_sum)[2]<-'total_cell'
      strata_agr<-cbind(strata_sum,'total_area'=strata_area[,'area'])
      
      #merge
      D8<-merge(D8,strata_agr,by='Strata')
      D8$samp_cell<-D8$n_samples/D8$total_cell
      D8$samp_area<-D8$n_samples/D8$total_area
      
      D8$Strata[is.na(D8$Strata)]<-99
      
  }

  
  
  #df to spatialpoint df
  coordinates(D8) <- ~ Lon + Lat
  crs(D8)<-c(crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  
  #reproject coordinates for plotting purposes
  D8_1<-spTransform(D8,'+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')
  D8_2<-data.frame(D8_1)
  
  #x and y cells
  xycells<-as.integer(sqrt(dim(D8_1)[1]))
  
  # create a template raster
  r1 <- raster(ext=extent(D8_1),ncol=xycells, nrow=xycells) #c(15800,15800) 7000
  crs(r1)<-CRS('+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')
  #create raster
  r2<-rasterize(D8_1, r1 ,field=c('Strata','n_samples','samp_area','samp_cell'))
  crs(r2) <- CRS('+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')
  #plot(r2)
  
  r2[r2==99]<-NA
  
  r3<-as.data.frame(r2,xy=TRUE)
  r4<-rasterToPolygons(r2$Strata,dissolve=TRUE,digits = 1)
  #plot(r4)
  
  for (istr in c(1:15,99)) {
    
  
  #strata selected
  #istr<-99
  r44<-r2$Strata
  if (istr!=99) {
    r44[r44!=istr]<-NA 
    sba<-subset(sb,strata==istr)
    sysa<-subset(sys1,strata==istr)
    randa<-subset(rand1,strata==istr)
  } else{
    sba<-sb
    sysa<-sys1
    randa<-rand1
    
  }
 
  r4a<-rasterToPolygons(r44,dissolve=TRUE,digits = 1)
  
  
  #par(mfrow=c(2,2))
  #ras
  p1<-
  ggplot()+
    #geom_raster(data=r3,aes(x=x,y=y,fill=log(samp_cell)))+
    geom_polygon(data=r4,aes(x=long,y=lat,group=group), colour=rgb(0, 0, 0, 0.1),alpha=0.2, fill=NA)+
    geom_polygon(data=r4a,aes(x=long,y=lat,group=group), colour='black',alpha=0.2, fill=NA)+
    #geom_point(data=D8_2, aes(Lon, Lat, fill=Strata, group=NULL),size=2, stroke=0,shape=21)+
    #scale_fill_gradientn(colours=c("#ea5545", "#f46a9b", "#ef9b20", "#edbf33", "#ede15b", "#bdcf32", "#87bc45", "#27aeef", "#b33dc6"))+
    #scale_fill_manual(values = color_scale)+
    scale_fill_gradientn(colors=pal1)+ #,limits=c(imin,imax)
    scale_x_continuous(expand = c(0,0),position = 'bottom',
                       breaks = c(-175,-170,-165,-160,-155),sec.axis = dup_axis())+
    coord_sf(crs = '+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs',
             xlim = c(-1372205, -224348.5),
             ylim = c(547256.1, 1813031),
             label_axes = "-NE-")+
    ggtitle(paste('strata ',istr))+
    #ggtitle(paste(''))+
    theme(aspect.ratio = 1,panel.grid.major = element_blank(),plot.background = element_rect(color='black'),
          panel.background = element_rect(fill = NA),panel.ontop = TRUE,
          legend.background =  element_rect(fill = "transparent", colour = "transparent"),legend.key.height= unit(20, 'points'),
          legend.key.width= unit(20, 'points'),axis.title = element_blank(),
          legend.position = 'none',
          #panel.border = element_rect(fill = NA, colour = 'grey'),
          legend.key = element_rect(color="black"),
          #legend.spacing.y = unit(8, 'points'),
          axis.text=element_blank(),axis.ticks = element_blank(),
          plot.margin = margin(0.01,0.01,0.01,0.01),
          #plot.title=element_text(margin=margin(t=40,b=-30)),
          axis.ticks.length = unit(-5,"points"),plot.title = element_text(size=18,hjust = 0.1,vjust=-4, face="bold"))+
    #ggtitle(paste(ititle))+
    scale_y_continuous(expand = c(0,0),position = 'right',sec.axis = dup_axis())#+
  
  p2<-
    ggplot()+
    #geom_raster(data=r3,aes(x=x,y=y,fill=log(samp_cell)))+
    geom_polygon(data=r4,aes(x=long,y=lat,group=group), colour=rgb(0, 0, 0, 0.1),alpha=0.2, fill=NA)+
    geom_polygon(data=r4a,aes(x=long,y=lat,group=group), colour='black',alpha=0.2, fill=NA)+
    #scale_fill_gradientn(colours=c("#ea5545", "#f46a9b", "#ef9b20", "#edbf33", "#ede15b", "#bdcf32", "#87bc45", "#27aeef", "#b33dc6"))+
    #scale_fill_manual(values = color_scale)+
    scale_fill_gradientn(colors=pal1)+ #,limits=c(imin,imax)
    scale_x_continuous(expand = c(0,0),position = 'bottom',
                       breaks = c(-175,-170,-165,-160,-155),sec.axis = dup_axis())+
    coord_sf(crs = '+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs',
             xlim = c(-1372205, -224348.5),
             ylim = c(547256.1, 1813031),
             label_axes = "-NE-")+
    theme(aspect.ratio = 1,panel.grid.major = element_blank(),plot.background = element_rect(color='black'),
          panel.background = element_rect(fill = NA),panel.ontop = TRUE,
          legend.background =  element_rect(fill = "transparent", colour = "transparent"),legend.key.height= unit(20, 'points'),
          legend.key.width= unit(20, 'points'),axis.title = element_blank(),
          legend.position = 'none',
          #panel.border = element_rect(fill = NA, colour = 'grey'),
          legend.key = element_rect(color="black"),
          #legend.spacing.y = unit(8, 'points'),
          axis.text=element_blank(),axis.ticks = element_blank(),
          plot.margin = margin(0.01,0.01,0.01,0.01),
          #plot.title=element_text(margin=margin(t=40,b=-30)),
          axis.ticks.length = unit(-5,"points"),plot.title = element_text(size=18,hjust = 0.1,vjust=-4, face="bold"))+
    #ggtitle(paste(ititle))+
    scale_y_continuous(expand = c(0,0),position = 'right',sec.axis = dup_axis())#+
  
    p3<-p2 + geom_point(data=sysa, aes(Lon, Lat, group=NULL),fill='red',size=3,shape=21,alpha=0.7) + ggtitle(paste('systematic'))
    p4<-p2  + geom_point(data=randa, aes(Lon, Lat, group=NULL),fill='green',size=3,shape=22,alpha=0.7)+ ggtitle(paste('random'))
    p2<-p2 +  geom_point(data=sba, aes(Lon, Lat, group=NULL),fill='yellow',size=3,shape=24,alpha=0.7) + ggtitle(paste('balanced random'))
      
    
    print(
    gridExtra::grid.arrange(p1,p3,p2,p4,nrow=1)
    )
  }
    
  plot(r44)
  points(sba$Lon,sba$Lat)
  plot(r44)
  points(sysa$Lon,sysa$Lat,pch=3)
  plot(r44)
  points(randa$Lon,randa$Lat,pch=4)
  
  r3<-r3[complete.cases(r3$Strata),] 
  #r3$prop<-as.factor(r3$prop)
  #r3$n_samples<-as.factor(r3$n_samples)
  # Create a named vector for scale_colour_manual
  pal <- wesanderson::wes_palette("Zissou1", 100, type = "continuous")
  color_scale <- setNames(as.character(pal), sort(unique(r3$samp_area)))
  
  r5<-crop(r2,gdb_table3)
  plot(r5)
  
  r5<-as.data.frame(r5,xy=TRUE)
  r5<-r5[complete.cases(r5$Strata),] 
  #r5$prop<-as.factor(r5$prop)
  #r5$n_samples<-as.factor(r5$n_samples)
  
  raster_list[[paste0(samp,'raster')]]<-r3
  raster_list[[paste0(samp,'polygon')]]<-r4
  
# }
# 
# max<-c()
# min<-c()
# 
# for (samp in samp_df$samp_scn) {
#   
#   r<-raster_list[[paste0(samp,'raster')]]
#   print(paste0(max(r$prop),min(r$prop)))
#   min<-c(min,min(r$prop))
#   max<-c(max,max(r$prop))
#   
# }
# 
imax<-max(log(r3$samp_area))
imin<-min(log(r3$samp_area))
# 
# samp_df$n<-1:5
# 
# for (samp in samp_df$samp_scn) {

  r3<-raster_list[[paste0(samp,'raster')]]
  r4<-raster_list[[paste0(samp,'polygon')]]
  # pal <- wes_palette("Zissou1", 15, type = "continuous")
  # color_scale <- setNames(as.character(pal), sort(unique(r3$samp_cell)))
  pal1 <- wes_palette("Zissou1", length(unique(r3$samp_cell)), type = "continuous")
  
  color_scale <- setNames(as.character(pal1), sort(unique(log(r3$samp_cell))))
  
  if (samp=='scn1') {
    ititle<-'depth + varSBT'
  } else if (samp=='scn2') {
    ititle<-'varSBT'
  } else if (samp=='scn3') {
    ititle<-'depth'
  } else if (samp=='scnbase') {
   ititle<-'existing' 
  }
  
  p<-
    ggplot()+
    geom_raster(data=r3,aes(x=x,y=y,fill=log(samp_cell)))+
    geom_polygon(data=r4,aes(x=long,y=lat,group=group), colour=rgb(0, 0, 0, 0.4),alpha=0.2, fill=NA)+
    #geom_point(data=D8_2, aes(Lon, Lat, fill=Strata, group=NULL),size=2, stroke=0,shape=21)+
    #scale_fill_gradientn(colours=c("#ea5545", "#f46a9b", "#ef9b20", "#edbf33", "#ede15b", "#bdcf32", "#87bc45", "#27aeef", "#b33dc6"))+
    #scale_fill_manual(values = color_scale)+
    scale_fill_gradientn(colors=pal1,na.value = 'transparent')+ #,limits=c(imin,imax)
    scale_x_continuous(expand = c(0,0),position = 'bottom',
                       breaks = c(-175,-170,-165,-160,-155),sec.axis = dup_axis())+
    coord_sf(crs = '+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs',
             xlim = c(-1372205, -224348.5),
             ylim = c(547256.1, 1813031),
             label_axes = "-NE-")+
    theme(aspect.ratio = 1,panel.grid.major = element_blank(),plot.background = element_rect(color='black'),
          panel.background = element_rect(fill = NA),panel.ontop = TRUE,
          legend.background =  element_rect(fill = "transparent", colour = "transparent"),legend.key.height= unit(20, 'points'),
          legend.key.width= unit(20, 'points'),axis.title = element_blank(),
          legend.position = 'none',
          #panel.border = element_rect(fill = NA, colour = 'grey'),
          legend.key = element_rect(color="black"),
          #legend.spacing.y = unit(8, 'points'),
          axis.text=element_blank(),axis.ticks = element_blank(),
          plot.margin = margin(0.01,0.01,0.01,0.01),
          #plot.title=element_text(margin=margin(t=40,b=-30)),
          axis.ticks.length = unit(-5,"points"),plot.title = element_text(size=18,hjust = 0.1,vjust=-4, face="bold"))+
    ggtitle(paste(ititle))+
    scale_y_continuous(expand = c(0,0),position = 'right',sec.axis = dup_axis())#+
    # guides(fill = guide_legend(order=2,override.aes=list(shape = 22,size=8)),
    #        color = guide_legend(order=1,override.aes=list(size=8)),
    #        shape = guide_legend(order=1),override.aes=list(size=8))#+
  
  print(p)
  plot_list_str[[samp]]<-p
  #plot_list[[paste0(samp_df[which(samp_df$samp_scn==samp),'n'])]] <-p #+ geom_point(data=sys1,aes(x=Lon,y=Lat),size=1,shape=4,stroke=1,alpha=0.7) #+ theme(axis.title.x.top = element_text(size=18,vjust = 2),axis.title.y.left =  element_text(size=18)) +labs(x='optimized_depth+SBTvar',y=' ')


  # if (samp == 'scnbase') {
  #   #add points
  #   plot_list[[paste0('sys_',samp)]] <-p + geom_point(data=sys1,aes(x=Lon,y=Lat),size=1,shape=4,stroke=1,alpha=0.7) #+ theme(axis.title.x.top = element_text(size=18,vjust = 2),axis.title.y.left =  element_text(size=18,vjust=3)) +labs(x='current',y='systematic')
  #   plot_list[[paste0('spb_',samp)]] <- p + geom_point(data=sb,aes(x=Lon,y=Lat),size=1,shape=4,stroke=1,alpha=0.7)#+ theme(axis.title.x.top = element_text(size=18),axis.title.y.left =  element_text(size=18,vjust=3)) +labs(x=' ',y='spatially-balanced')
  #   plot_list[[paste0('rand_',samp)]] <-p + geom_point(data=rand1,aes(x=Lon,y=Lat),size=1,shape=4,stroke=1,alpha=0.7)#+ theme(axis.title.x.top = element_text(size=18),axis.title.y.left =  element_text(size=18,vjust=3)) +labs(x=' ',y='random')
  # 
  # } else if (samp == 'scnbase_bis') {
  #   #add points
  #   # plot_list[[paste0('sys_',samp)]] <-p + geom_point(data=sys1,aes(x=Lon,y=Lat),size=1,shape=4,stroke=1,alpha=0.7)# + theme(axis.title.x.top = element_text(size = 18,vjust = 2),axis.title.y.left =  element_text(size=18)) +labs(x='current_w/o corner',y=' ')
  #   # plot_list[[paste0('spb_',samp)]] <- p + geom_point(data=sb,aes(x=Lon,y=Lat),size=1,shape=4,stroke=1,alpha=0.7)#+ theme(axis.title.x.top = element_text(size = 18),axis.title.y.left =  element_text(size=18)) +labs(x=' ',y=' ')
  #   # plot_list[[paste0('rand_',samp)]] <-p + geom_point(data=rand1,aes(x=Lon,y=Lat),size=1,shape=4,stroke=1,alpha=0.7)#+ theme(axis.title.x.top = element_text(size = 18),axis.title.y.left =  element_text(size=18)) +labs(x=' ',y=' ')
  # } else if (samp == 'scn3') {
  #   #add points
  #   plot_list[[paste0('sys_',samp)]] <-p + geom_point(data=sys1,aes(x=Lon,y=Lat),size=1,shape=4,stroke=1,alpha=0.7)# + theme(axis.title.x.top = element_text(size=18,vjust = 2),axis.title.y.left =  element_text(size=18)) +labs(x='optimized_depth',y=' ')
  #   plot_list[[paste0('spb_',samp)]] <- p + geom_point(data=sb,aes(x=Lon,y=Lat),size=1,shape=4,stroke=1,alpha=0.7)#+ theme(axis.title.x.top = element_text(size=18),axis.title.y.left =  element_text(size=18)) +labs(x=' ',y=' ')
  #   plot_list[[paste0('rand_',samp)]] <-p + geom_point(data=rand1,aes(x=Lon,y=Lat),size=1,shape=4,stroke=1,alpha=0.7)#+ theme(axis.title.x.top = element_text(size=18),axis.title.y.left =  element_text(size=18)) +labs(x=' ',y=' ')
  # } else if (samp == 'scn2') {
  #   #add points
  #   plot_list[[paste0('sys_',samp)]] <-p + geom_point(data=sys1,aes(x=Lon,y=Lat),size=1,shape=4,stroke=1,alpha=0.7)# + theme(axis.title.x.top = element_text(size=18,vjust = 2),axis.title.y.left =  element_text(size=18)) +labs(x='optimized_SBTvar',y=' ')
  #   plot_list[[paste0('spb_',samp)]] <- p + geom_point(data=sb,aes(x=Lon,y=Lat),size=1,shape=4,stroke=1,alpha=0.7)#+ theme(axis.title.x.top = element_text(size=18),axis.title.y.left =  element_text(size=18)) +labs(x=' ',y=' ')
  #   plot_list[[paste0('rand_',samp)]] <-p + geom_point(data=rand1,aes(x=Lon,y=Lat),size=1,shape=4,stroke=1,alpha=0.7)#+ theme(axis.title.x.top = element_text(size=18),axis.title.y.left =  element_text(size=18)) +labs(x=' ',y=' ')
  # } else if (samp == 'scn1') {
  #   #add points
  #   plot_list[[paste0('sys_',samp)]] <-p + geom_point(data=sys1,aes(x=Lon,y=Lat),size=1,shape=4,stroke=1,alpha=0.7)# + theme(axis.title.x.top = element_text(size=18,vjust = 2),axis.title.y.left =  element_text(size=18)) +labs(x='optimized_depth+SBTvar',y=' ')
  #   plot_list[[paste0('spb_',samp)]] <- p + geom_point(data=sb,aes(x=Lon,y=Lat),size=1,shape=4,stroke=1,alpha=0.7)#+ theme(axis.title.x.top = element_text(size=18),axis.title.y.left =  element_text(size=18)) +labs(x=' ',y=' ')
  #   plot_list[[paste0('rand_',samp)]] <-p + geom_point(data=rand1,aes(x=Lon,y=Lat),size=1,shape=4,stroke=1,alpha=0.7)#+ theme(axis.title.x.top = element_text(size=18),axis.title.y.left =  element_text(size=18)) +labs(x=' ',y=' ')
  # }

  
}

  # #base map
  # p<-
  # ggplot()+
  #   geom_raster(data=r3,aes(x=x,y=y,fill=prop),alpha=0.8)+
  #   geom_polygon(data=r4,aes(x=long,y=lat,group=group), colour=rgb(0, 0, 0, 0.4),alpha=0.2, fill=NA)+
  #   #geom_point(data=D8_2, aes(Lon, Lat, fill=Strata, group=NULL),size=2, stroke=0,shape=21)+
  #   #scale_fill_gradientn(colours=c("#ea5545", "#f46a9b", "#ef9b20", "#edbf33", "#ede15b", "#bdcf32", "#87bc45", "#27aeef", "#b33dc6"))+
  #   scale_fill_manual(values = color_scale)+ 
  #   #scale_fill_gradientn(colors=pal,values = sort(unique(r3$prop)))+ 
  #   scale_x_continuous(expand = c(0,0),position = 'bottom',
  #                      breaks = c(-175,-170,-165,-160,-155),sec.axis = dup_axis())+
  #   coord_sf(crs = '+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs',
  #            xlim = c(-1372205, -224348.5),
  #            ylim = c(547256.1, 1813031),
  #            label_axes = "-NE-")+
  #   theme(aspect.ratio = 1,panel.grid.major = element_blank(),plot.background = element_rect(color='black'),
  #         panel.background = element_rect(fill = NA),panel.ontop = TRUE,
  #         legend.background =  element_rect(fill = "transparent", colour = "transparent"),legend.key.height= unit(20, 'points'),
  #         legend.key.width= unit(20, 'points'),axis.title = element_blank(),
  #         legend.position = 'none',
  #         #panel.border = element_rect(fill = NA, colour = 'grey'),
  #         legend.key = element_rect(color="black"),
  #         legend.spacing.y = unit(8, 'points'),
  #         axis.text=element_blank(),axis.ticks = element_blank(),
  #         plot.margin = margin(10,10,10,10), 
  #         axis.ticks.length = unit(-5,"points"),plot.title = element_text(size=18,hjust = 0.5,vjust=-5, face="bold"))+
  #   scale_y_continuous(expand = c(0,0),position = 'right',sec.axis = dup_axis())+
  #   guides(fill = guide_legend(order=2,override.aes=list(shape = 22,size=8)),
  #          color = guide_legend(order=1,override.aes=list(size=8)),
  #          shape = guide_legend(order=1),override.aes=list(size=8))#+
  # #labs(title=paste0(gsub('_',' + ',samp_df[s,'strat_var'])),fill='')
  # 
  # #legend_d<-
  #   # ggplot()+
  #   # geom_raster(data=r3,aes(x=x,y=y,fill=prop),alpha=0.8)+
  #   # geom_polygon(data=r4,aes(x=long,y=lat,group=group), colour=rgb(0, 0, 0, 0.4),alpha=0.2, fill=NA)+
  #   # #geom_point(data=D8_2, aes(Lon, Lat, fill=Strata, group=NULL),size=2, stroke=0,shape=21)+
  #   # #scale_fill_gradientn(colours=c("#ea5545", "#f46a9b", "#ef9b20", "#edbf33", "#ede15b", "#bdcf32", "#87bc45", "#27aeef", "#b33dc6"))+
  #   # scale_fill_manual(values = color_scale,breaks=unique(r3$prop)[c(1,15)])+ #,labels=c("Lower","Higher"),name='log(ss/ms samples)' 
  #   # guides(fill=guide_colorsteps(title.position = 'top', title.hjust = 0.5,ticks.colour = NA,frame.colour = 'black'))+
  #   # theme(legend.position = 'bottom')+
  #   # labs(fill='')
  # 
  # pcrab<-
  # ggplot()+
  #   geom_raster(data=r5,aes(x=x,y=y,fill=n_samples),alpha=0.8)+
  #   geom_polygon(data=r4,aes(x=long,y=lat,group=group), colour=rgb(0, 0, 0, 0.4),alpha=0.2, fill=NA)+
  #   #geom_point(data=D8_2, aes(Lon, Lat, fill=Strata, group=NULL),size=2, stroke=0,shape=21)+
  #   #scale_fill_gradientn(colours=c("#ea5545", "#f46a9b", "#ef9b20", "#edbf33", "#ede15b", "#bdcf32", "#87bc45", "#27aeef", "#b33dc6"))+
  #   scale_fill_manual(values = color_scale)+ 
  #   #scale_fill_gradientn(colors=pal,values = sort(unique(r3$prop)))+ 
  #   scale_x_continuous(expand = c(0,0),position = 'bottom',
  #                      breaks = c(-175,-170,-165,-160,-155),sec.axis = dup_axis())+
  #   coord_sf(crs = '+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs',
  #            xlim = c(-1372205, -224348.5),
  #            ylim = c(547256.1, 1813031),
  #            label_axes = "-NE-")+
  #   theme(aspect.ratio = 1,panel.grid.major = element_blank(),plot.background = element_rect(color='black'),
  #         panel.background = element_rect(fill = NA),panel.ontop = TRUE,
  #         legend.background =  element_rect(fill = "transparent", colour = "transparent"),legend.key.height= unit(20, 'points'),
  #         legend.key.width= unit(20, 'points'),axis.title = element_blank(),legend.position = 'none',
  #         #panel.border = element_rect(fill = NA, colour = 'grey'),
  #         legend.key = element_rect(color="black"),
  #         legend.spacing.y = unit(8, 'points'),
  #         axis.text=element_blank(),axis.ticks = element_blank(),
  #         plot.margin = margin(10,10,10,10), 
  #         axis.ticks.length = unit(-5,"points"),plot.title = element_text(size=18,hjust = 0.5,vjust=-5, face="bold"))+
  #   scale_y_continuous(expand = c(0,0),position = 'right',sec.axis = dup_axis())+
  #   guides(fill = guide_legend(order=2,override.aes=list(shape = 22,size=8)),
  #          color = guide_legend(order=1,override.aes=list(size=8)),
  #          shape = guide_legend(order=1),override.aes=list(size=8))#+
  # 
  # 
  # plot_list[[paste0('sys_',samp)]] <-p + geom_point(data=sys1,aes(x=Lon,y=Lat),size=1,shape=4,stroke=1,alpha=0.7) #+ theme(axis.title.x.top = element_text(size=18,vjust = 2),axis.title.y.left =  element_text(size=18)) +labs(x='optimized_depth+SBTvar',y=' ')
  # plot_list[[paste0('spb_',samp)]] <- p + geom_point(data=sb,aes(x=Lon,y=Lat),size=1,shape=4,stroke=1,alpha=0.7)#+ theme(axis.title.x.top = element_text(size=18),axis.title.y.left =  element_text(size=18)) +labs(x=' ',y=' ')
  # plot_list[[paste0('rand_',samp)]] <-p + geom_point(data=rand1,aes(x=Lon,y=Lat),size=1,shape=4,stroke=1,alpha=0.7)#+ theme(axis.title.x.top = element_text(size=18),axis.title.y.left =  element_text(size=18)) +labs(x=' ',y=' ')
  # 

  #samp<-samp_df$samp_scn[1]
  
  # if (samp == 'scnbase') {
  #   #add points
  #   plot_list[[paste0('sys_',samp)]] <-p + geom_point(data=sys1,aes(x=Lon,y=Lat),size=1,shape=4,stroke=1,alpha=0.7) #+ theme(axis.title.x.top = element_text(size=18,vjust = 2),axis.title.y.left =  element_text(size=18,vjust=3)) +labs(x='current',y='systematic')
  #   plot_list[[paste0('spb_',samp)]] <- p + geom_point(data=sb,aes(x=Lon,y=Lat),size=1,shape=4,stroke=1,alpha=0.7)#+ theme(axis.title.x.top = element_text(size=18),axis.title.y.left =  element_text(size=18,vjust=3)) +labs(x=' ',y='spatially-balanced')
  #   plot_list[[paste0('rand_',samp)]] <-p + geom_point(data=rand1,aes(x=Lon,y=Lat),size=1,shape=4,stroke=1,alpha=0.7)#+ theme(axis.title.x.top = element_text(size=18),axis.title.y.left =  element_text(size=18,vjust=3)) +labs(x=' ',y='random')
  #   
  #   } else if (samp == 'scnbase_bis') {
  #   #add points
  #   plot_list[[paste0('sys_',samp)]] <-p + geom_point(data=sys1,aes(x=Lon,y=Lat),size=1,shape=4,stroke=1,alpha=0.7)# + theme(axis.title.x.top = element_text(size = 18,vjust = 2),axis.title.y.left =  element_text(size=18)) +labs(x='current_w/o corner',y=' ')
  #   plot_list[[paste0('spb_',samp)]] <- p + geom_point(data=sb,aes(x=Lon,y=Lat),size=1,shape=4,stroke=1,alpha=0.7)#+ theme(axis.title.x.top = element_text(size = 18),axis.title.y.left =  element_text(size=18)) +labs(x=' ',y=' ')
  #   plot_list[[paste0('rand_',samp)]] <-p + geom_point(data=rand1,aes(x=Lon,y=Lat),size=1,shape=4,stroke=1,alpha=0.7)#+ theme(axis.title.x.top = element_text(size = 18),axis.title.y.left =  element_text(size=18)) +labs(x=' ',y=' ')
  # } else if (samp == 'scn3') {
  #   #add points
  #   plot_list[[paste0('sys_',samp)]] <-p + geom_point(data=sys1,aes(x=Lon,y=Lat),size=1,shape=4,stroke=1,alpha=0.7)# + theme(axis.title.x.top = element_text(size=18,vjust = 2),axis.title.y.left =  element_text(size=18)) +labs(x='optimized_depth',y=' ')
  #   plot_list[[paste0('spb_',samp)]] <- p + geom_point(data=sb,aes(x=Lon,y=Lat),size=1,shape=4,stroke=1,alpha=0.7)#+ theme(axis.title.x.top = element_text(size=18),axis.title.y.left =  element_text(size=18)) +labs(x=' ',y=' ')
  #   plot_list[[paste0('rand_',samp)]] <-p + geom_point(data=rand1,aes(x=Lon,y=Lat),size=1,shape=4,stroke=1,alpha=0.7)#+ theme(axis.title.x.top = element_text(size=18),axis.title.y.left =  element_text(size=18)) +labs(x=' ',y=' ')
  # } else if (samp == 'scn2') {
  #   #add points
  #   plot_list[[paste0('sys_',samp)]] <-p + geom_point(data=sys1,aes(x=Lon,y=Lat),size=1,shape=4,stroke=1,alpha=0.7)# + theme(axis.title.x.top = element_text(size=18,vjust = 2),axis.title.y.left =  element_text(size=18)) +labs(x='optimized_SBTvar',y=' ')
  #   plot_list[[paste0('spb_',samp)]] <- p + geom_point(data=sb,aes(x=Lon,y=Lat),size=1,shape=4,stroke=1,alpha=0.7)#+ theme(axis.title.x.top = element_text(size=18),axis.title.y.left =  element_text(size=18)) +labs(x=' ',y=' ')
  #   plot_list[[paste0('rand_',samp)]] <-p + geom_point(data=rand1,aes(x=Lon,y=Lat),size=1,shape=4,stroke=1,alpha=0.7)#+ theme(axis.title.x.top = element_text(size=18),axis.title.y.left =  element_text(size=18)) +labs(x=' ',y=' ')
  # } else if (samp == 'scn1') {
  #   #add points
  #   plot_list[[paste0('sys_',samp)]] <-p + geom_point(data=sys1,aes(x=Lon,y=Lat),size=1,shape=4,stroke=1,alpha=0.7)# + theme(axis.title.x.top = element_text(size=18,vjust = 2),axis.title.y.left =  element_text(size=18)) +labs(x='optimized_depth+SBTvar',y=' ')
  #   plot_list[[paste0('spb_',samp)]] <- p + geom_point(data=sb,aes(x=Lon,y=Lat),size=1,shape=4,stroke=1,alpha=0.7)#+ theme(axis.title.x.top = element_text(size=18),axis.title.y.left =  element_text(size=18)) +labs(x=' ',y=' ')
  #   plot_list[[paste0('rand_',samp)]] <-p + geom_point(data=rand1,aes(x=Lon,y=Lat),size=1,shape=4,stroke=1,alpha=0.7)#+ theme(axis.title.x.top = element_text(size=18),axis.title.y.left =  element_text(size=18)) +labs(x=' ',y=' ')
  # }

 
#}

legend1 <- cowplot::get_legend( 
  plot_list_str$scn3 + 
    theme(legend.position = "bottom",plot.background = element_blank(), panel.background = element_blank())+
    scale_fill_gradientn(colors=pal,limits=c(imin,imax),
                         breaks=range(imin,imax),labels=c("Low","High"),name='sampling effort (stations/area)')+
    guides(fill=guide_colorbar(title.position = 'top', title.hjust = 0.5,ticks.colour = NA,frame.colour = 'black',))+
    theme(legend.position = 'bottom',legend.key.size = unit(60, 'points'),legend.text = element_text(size=14), #legend.position=c(.85,.19)
          legend.title = element_text(size=16),legend.key.height= unit(1, 'cm'),
          legend.key.width= unit(2, 'cm'))+
    labs(fill='')
) 

ppa<-cowplot::plot_grid(plotlist = plot_list[3:5],nrow = 3,ncol=1,byrow = FALSE)#+
pp<-cowplot::plot_grid(plotlist = plot_list_str,nrow = 3,ncol=1,byrow = FALSE)#+


  #theme(panel.border = element_rect(fill = NA, colour = 'grey'))
#theme(panel.border = element_rect(fill = NA, colour = 'grey')

pp1<-cowplot::plot_grid(pp,legend1,nrow = 2,ncol=1,rel_heights = c(1,0.12))
pp1a<-cowplot::plot_grid(ppa,legend1,nrow = 2,ncol=1,rel_heights = c(1,0.08))


#if length 3
ragg::agg_png(paste0('./figures/stratification_maps_legendscnlog.png'), width = 5, height = 15, units = "in", res = 300)
print(pp1a)
dev.off()


ragg::agg_png(paste0('./figures/stratification_maps_legends_all.png'),  width = 5, height = 15, units = "in", res = 300)
print(pp1)
dev.off()



#########################
# RUN LOOP SPECIES
#########################



#loop over species
for (sp in spp) {
  
  sp<-'Gadus macrocephalus'
  
  #load optimization data
  load(paste0('./output/species/',sp,'/optimization data/optimization_static_data.RData')) #D6
  #load(paste0('./output/species/',sp,'/projection_data.RData')) #temp_dens_vals
  
  #load fit OM
  load(paste0('./shelf EBS NBS VAST/',sp,'/fit-001.RData'))
  
  #removed cells because of depth
  rem_cells<-D6[which(D6$include==FALSE),'cell']
  ok_cells<-D6[which(D6$include==1),'cell']
  
  #load data_geostat file
  data_geostat<-readRDS(paste0('./data processed/species/',sp,'/','data_geostat_temp.rds')) #fit
  
  ###################################
  # CREATE SPATIAL OBJECT BASED ON CELLS STRATA
  ###################################
  
  plot_list_nsamples<-list()
  plot_list_sampleprop<-list()
  
  for (s in 1:nrow(samp_df)) {
  
      
  #s<-3
  
  #load solutions
  #load(file=paste0('./output/species/',sp,'/optimization data/optimization_results_',samp_df[s,'samp_scn'],'.RData')) #result_list
  
  load(file = paste0("./output/ms_optim_allocations_",samp_df[s,'samp_scn'],".RData")) #all
  
  strata<-rbind(all$result_list$solution$indices,
                data.frame(ID=rem_cells,X1=NA))
  colnames(strata)<-c('cell','Strata')
  
  #n cells by strata
  strata_sum<-aggregate(cell ~ Strata, data = strata, FUN = length)
  names(strata_sum)[2]<-'total_cell'
  #merge
  strata<-merge(strata,all$samples_strata,by.x='Strata',by.y='strata')
  strata<-merge(strata,strata_sum,by='Strata')
  strata$prop<-strata$n_samples/strata$total_cell
  dim(strata)
  
  D8<-merge(D6,strata,by='cell')
  D8$Strata[is.na(D8$Strata)]<-99

  #df to spatialpoint df
  coordinates(D8) <- ~ Lon + Lat
  crs(D8)<-c(crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  
  #reproject coordinates for plotting purposes
  D8_1<-spTransform(D8,'+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')
  D8_2<-data.frame(D8_1)
  
  #x and y cells
  xycells<-as.integer(sqrt(dim(D8_1)[1]))
  
  # create a template raster
  r1 <- raster(ext=extent(D8_1),ncol=xycells, nrow=xycells) #c(15800,15800) 7000
  
  #create raster
  r2<-rasterize(D8_1, r1 ,field=c('Strata','n_samples','prop'))
  crs(r2) <- CRS('+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')
  #plot(r2)
  
  r2[r2==99]<-NA
  
  r3<-as.data.frame(r2,xy=TRUE)
  r4<-rasterToPolygons(r2$Strata,dissolve=TRUE,digits = 1)
  
  pal <- wes_palette("Zissou1", 15, type = "continuous")
  
  r3<-r3[complete.cases(r3$Strata),] 
  
  r3$prop<-as.factor(r3$prop)
  r3$n_samples<-as.factor(r3$n_samples)
  # Create a named vector for scale_colour_manual
  color_scale <- setNames(as.character(pal), sort(unique(r3$n_samples)))
  
  p1<-
    ggplot()+
    geom_raster(data=r3,aes(x=x,y=y,fill=n_samples))+
    geom_polygon(data=r4,aes(x=long,y=lat,group=group), colour="black", fill=NA)+
    #geom_point(data=D8_2, aes(Lon, Lat, fill=Strata, group=NULL),size=2, stroke=0,shape=21)+
      #scale_fill_gradientn(colours=c("#ea5545", "#f46a9b", "#ef9b20", "#edbf33", "#ede15b", "#bdcf32", "#87bc45", "#27aeef", "#b33dc6"))+
      scale_fill_manual(values = color_scale)+ 
      #scale_fill_gradientn(colors=pal,values = sort(unique(r3$prop)))+ 
      scale_x_continuous(expand = c(0,0),position = 'bottom',
                         breaks = c(-175,-170,-165,-160,-155),sec.axis = dup_axis())+
      coord_sf(crs = '+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs',
               xlim = panel_extent$x-c(000000,100000),
               ylim = panel_extent$y-c(0,300000),
               label_axes = "-NE-")+
      theme(aspect.ratio = 1,panel.grid.major = element_blank(),
            panel.background = element_rect(fill = NA),panel.ontop = TRUE,
            legend.background =  element_rect(fill = "transparent", colour = "transparent"),legend.key.height= unit(20, 'points'),
            legend.key.width= unit(20, 'points'),axis.title = element_blank(),legend.position = 'none',
            panel.border = element_rect(fill = NA, colour = NA),legend.key = element_rect(color="black"),
            legend.spacing.y = unit(8, 'points'),
            axis.text=element_blank(),axis.ticks = element_blank(),
            plot.margin = margin(0.01,0.01,0.01,0.01), 
            axis.ticks.length = unit(-5,"points"),plot.title = element_text(size=18,hjust = 0.5,vjust=-5, face="bold"))+
      scale_y_continuous(expand = c(0,0),position = 'right',sec.axis = dup_axis())+
      guides(fill = guide_legend(order=2,override.aes=list(shape = 22,size=8)),
             color = guide_legend(order=1,override.aes=list(size=8)),
             shape = guide_legend(order=1),override.aes=list(size=8))+
    labs(title=paste0(gsub('_',' + ',samp_df[s,'strat_var'])),fill='')
    

    
    plot_list_nsamples[[s]]<-p1
    
    color_scale <- setNames(as.character(pal), sort(unique(r3$prop)))
    
    p2<-
      ggplot()+
      geom_raster(data=r3,aes(x=x,y=y,fill=prop))+
      geom_polygon(data=r4,aes(x=long,y=lat,group=group), colour="black", fill=NA)+
      #geom_point(data=D8_2, aes(Lon, Lat, fill=Strata, group=NULL),size=2, stroke=0,shape=21)+
      #scale_fill_gradientn(colours=c("#ea5545", "#f46a9b", "#ef9b20", "#edbf33", "#ede15b", "#bdcf32", "#87bc45", "#27aeef", "#b33dc6"))+
      scale_fill_manual(values = color_scale)+ 
      #scale_fill_gradientn(colors=pal,values = sort(unique(r3$prop)))+ 
      scale_x_continuous(expand = c(0,0),position = 'bottom',
                         breaks = c(-175,-170,-165,-160,-155),sec.axis = dup_axis())+
      coord_sf(crs = '+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs',
               xlim = panel_extent$x-c(000000,100000),
               ylim = panel_extent$y-c(0,300000),
               label_axes = "-NE-")+
      theme(aspect.ratio = 1,panel.grid.major = element_blank(),
            panel.background = element_rect(fill = NA),panel.ontop = TRUE,
            legend.background =  element_rect(fill = "transparent", colour = "transparent"),legend.key.height= unit(20, 'points'),
            legend.key.width= unit(20, 'points'),axis.title = element_blank(),legend.position = 'none',
            panel.border = element_rect(fill = NA, colour = NA),legend.key = element_rect(color="black"),
            legend.spacing.y = unit(8, 'points'),
            axis.text=element_blank(),axis.ticks = element_blank(),
            plot.margin = margin(0.01,0.01,0.01,0.01), 
            axis.ticks.length = unit(-5,"points"),plot.title = element_text(size=18,hjust = 0.5,vjust=-5, face="bold"))+
      scale_y_continuous(expand = c(0,0),position = 'right',sec.axis = dup_axis())+
      guides(fill = guide_legend(order=2,override.aes=list(shape = 22,size=8)),
             color = guide_legend(order=1,override.aes=list(size=8)),
             shape = guide_legend(order=1),override.aes=list(size=8))+
      labs(title=paste0(gsub('_',' + ',samp_df[s,'strat_var'])),fill='')  


    plot_list_sampleprop[[s]]<-p2

    
  
  #save plot
# ragg::agg_png(paste0('./figures/species/',sp,'/optimized_stratification_',samp_df[s,'samp_scn'],'.png'), width = 7, height = 7, units = "in", res = 300)
# print(p)
# dev.off()
  }
}
#}


##################3baseline

load('./output/baseline_strata.RData')

baseline_strata$cell_strata<-as.data.frame(baseline_strata$cell_strata)
baseline_strata$cell_strata<-merge(baseline_strata$cell_strata,baseline_strata$n_samples,by.x='Stratum',by.y='stratum')

#n cells by strata
strata_sum<-aggregate(cell ~ Stratum, data = baseline_strata$cell_strata, FUN = length)
names(strata_sum)[2]<-'total_cell'
#merge
baseline_strata$cell_strata<-merge(baseline_strata$cell_strata,strata_sum,by='Stratum')
baseline_strata$cell_strata$prop<-baseline_strata$cell_strata$scnbase/baseline_strata$cell_strata$total_cell
names(baseline_strata$cell_strata)[7]<-'n_samples'

#df to spatialpoint df
coordinates(baseline_strata$cell_strata) <- ~ Lon + Lat
crs(baseline_strata$cell_strata)<-c(crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

#reproject coordinates for plotting purposes
D8_1<-spTransform(baseline_strata$cell_strata,'+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')
D8_2<-data.frame(D8_1)

#x and y cells
xycells<-as.integer(sqrt(dim(D8_1)[1]))

# create a template raster
r1 <- raster(ext=extent(D8_1),ncol=xycells, nrow=xycells) #c(15800,15800) 7000

#create raster
r2<-rasterize(D8_1, r1 ,field=c('Stratum','n_samples','prop'))
crs(r2) <- CRS('+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')
#plot(r2)

r2[r2==99]<-NA
r3<-as.data.frame(r2,xy=TRUE)
r4<-rasterToPolygons(r2,dissolve=TRUE,digits = 1)

#aggregate(r3$layer,by=list(r3$layer),FUN=length)
# 
# p<-
#   ggplot()+
#   geom_raster(data=r3,aes(x=x,y=y,fill=as.character(layer)))+
#   geom_polygon(data=r4,aes(x=long,y=lat,group=group), colour="black", fill=NA)+
#   scale_fill_gradientn(colours=c("#ea5545", "#f46a9b", "#ef9b20", "#edbf33", "#ede15b", "#bdcf32", "#87bc45", "#27aeef", "#b33dc6"),
#                            guide = guide_legend(frame.colour = "black", 
#                                                 ticks.colour = "black"),breaks=sort(unique(D8_2$Strata)),na.value = "transparent",labels=paste0(sort(unique(D8_2$Strata))," (n=",result_list$sample_allocations,')'))+
#       scale_x_continuous(expand = c(0,0),position = 'bottom',
#                          breaks = c(-175,-170,-165,-160,-155),sec.axis = dup_axis())+
#       coord_sf(crs = '+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs',
#                xlim = panel_extent$x-c(100000,100000),
#                ylim = panel_extent$y-c(0,300000),
#                label_axes = "-NE-")+
#       theme(aspect.ratio = 1,panel.grid.major = element_blank(),
#             panel.background = element_rect(fill = NA),panel.ontop = TRUE,
#             legend.background =  element_rect(fill = "transparent", colour = "transparent"),legend.key.height= unit(20, 'points'),
#             legend.key.width= unit(20, 'points'),axis.title = element_blank(),legend.position = 'none',
#             panel.border = element_rect(fill = NA, colour = NA),legend.key = element_rect(color="black"),
#             legend.spacing.y = unit(8, 'points'),
#            axis.text=element_blank(),axis.ticks = element_blank(),
#             plot.margin = margin(0.01,0.01,0.01,0.01), 
#             axis.ticks.length = unit(-5,"points"),plot.title = element_text(size=14,hjust = 0.5,vjust=-10, face="bold"))+
#       scale_y_continuous(expand = c(0,0),position = 'right',sec.axis = dup_axis())+
#       guides(fill = guide_legend(order=2,override.aes=list(shape = 22,size=8)),
#              color = guide_legend(order=1,override.aes=list(size=8)),
#              shape = guide_legend(order=1),override.aes=list(size=8))+
#       labs(title=paste0(gsub('_',' + ',samp_df[s,'strat_var'])),fill='')


r3<-r3[complete.cases(r3$Stratum),] 
r3$Stratum<-as.character(r3$Stratum)
#as.vector(unique(g$data[[1]]["fill"]))

r3$prop<-as.factor(r3$prop)
r3$n_samples<-as.factor(r3$n_samples)
# Create a named vector for scale_colour_manual
color_scale <- setNames(as.character(pal), sort(unique(r3$prop)))

p2<-
  ggplot()+
  geom_raster(data=r3,aes(x=x,y=y,fill=prop))+
  geom_polygon(data=r4,aes(x=long,y=lat,group=group), colour="black", fill=NA)+
  # scale_fill_manual(values=c('82'="#86BA59",'81'= "#4CA5EB",'90'= "#87BC45",'71'= "#629CE7",'70'= "#B33DC6",
  #                            '61'= "#B8CD34",'41'= "#EDC237",'62'= "#B3CB37",'43'= "#EDC940",'20'= "#F46A9B",
  #                            '10'= "#EA5545",'42'= "#EDC63C",'31'= "#EF9F22",'32'= "#EFA224",'50'= "#EDE15B"))+
  scale_fill_manual(values = color_scale)+ 
  scale_x_continuous(expand = c(0,0),position = 'bottom',
                     breaks = c(-175,-170,-165,-160,-155),sec.axis = dup_axis())+
  coord_sf(crs = '+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs',
           xlim = panel_extent$x-c(000000,100000),
           ylim = panel_extent$y-c(0,300000),
           label_axes = "-NE-")+
  theme(aspect.ratio = 1,panel.grid.major = element_blank(),
        panel.background = element_rect(fill = NA),panel.ontop = TRUE,
        legend.background =  element_rect(fill = "transparent", colour = "transparent"),legend.key.height= unit(20, 'points'),
        legend.key.width= unit(20, 'points'),axis.title = element_blank(),
        panel.border = element_rect(fill = NA, colour = NA),legend.key = element_rect(color="black"),
        legend.spacing.y = unit(8, 'points'),
        axis.text=element_blank(),axis.ticks = element_blank(),
        plot.margin = margin(0.01,0.01,0.01,0.01), legend.position = 'none',
        axis.ticks.length = unit(-5,"points"),plot.title = element_text(size=18,hjust = 0.5,vjust=-5, face="bold"))+
  scale_y_continuous(expand = c(0,0),position = 'right',sec.axis = dup_axis())+
  guides(fill = guide_legend(order=2,override.aes=list(shape = 22,size=8)),
         color = guide_legend(order=1,override.aes=list(size=8)),
         shape = guide_legend(order=1),override.aes=list(size=8))+
  labs(title='baseline',fill='')

pal1 <- wes_palette("Zissou1", length(unique(r3$n_samples)), type = "continuous")

color_scale <- setNames(as.character(pal1), sort(unique(r3$n_samples)))

p1<-
  ggplot()+
  geom_raster(data=r3,aes(x=x,y=y,fill=n_samples))+
  geom_polygon(data=r4,aes(x=long,y=lat,group=group), colour="black", fill=NA)+
  # scale_fill_manual(values=c('82'="#86BA59",'81'= "#4CA5EB",'90'= "#87BC45",'71'= "#629CE7",'70'= "#B33DC6",
  #                            '61'= "#B8CD34",'41'= "#EDC237",'62'= "#B3CB37",'43'= "#EDC940",'20'= "#F46A9B",
  #                            '10'= "#EA5545",'42'= "#EDC63C",'31'= "#EF9F22",'32'= "#EFA224",'50'= "#EDE15B"))+
  scale_fill_manual(values = color_scale)+ 
  scale_x_continuous(expand = c(0,0),position = 'bottom',
                     breaks = c(-175,-170,-165,-160,-155),sec.axis = dup_axis())+
  coord_sf(crs = '+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs',
           xlim = panel_extent$x-c(000000,100000),
           ylim = panel_extent$y-c(0,300000),
           label_axes = "-NE-")+
  theme(aspect.ratio = 1,panel.grid.major = element_blank(),
        panel.background = element_rect(fill = NA),panel.ontop = TRUE,
        legend.background =  element_rect(fill = "transparent", colour = "transparent"),legend.key.height= unit(20, 'points'),
        legend.key.width= unit(20, 'points'),axis.title = element_blank(),
        panel.border = element_rect(fill = NA, colour = NA),legend.key = element_rect(color="black"),
        legend.spacing.y = unit(8, 'points'),
        axis.text=element_blank(),axis.ticks = element_blank(),
        plot.margin = margin(0.01,0.01,0.01,0.01), legend.position = 'none',
        axis.ticks.length = unit(-5,"points"),plot.title = element_text(size=18,hjust = 0.5,vjust=-5, face="bold"))+
  scale_y_continuous(expand = c(0,0),position = 'right',sec.axis = dup_axis())+
  guides(fill = guide_legend(order=2,override.aes=list(shape = 22,size=8)),
         color = guide_legend(order=1,override.aes=list(size=8)),
         shape = guide_legend(order=1),override.aes=list(size=8))+
  labs(title='baseline',fill='')

  g <- ggplot_build(p1)
  unique(g$data[[1]]["fill"])
  
  
plot_list_nsamples[['baseline']]<-p1
plot_list_sampleprop[['baseline']]<-p2

cowplot::plot_grid(plot_list_nsamples[['baseline']],plot_list_nsamples[[3]],plot_list_nsamples[[2]],plot_list_nsamples[[1]])






  ragg::agg_png(paste0('./figures/sampling designs nsamples.png'), width = 10, height = 10, units = "in", res = 300)
  cowplot::plot_grid(plot_list_nsamples[['baseline']],plot_list_nsamples[[3]],plot_list_nsamples[[2]],plot_list_nsamples[[1]])
  dev.off()

  ragg::agg_png(paste0('./figures/sampling designs propsamples.png'), width = 10, height = 10, units = "in", res = 300)
  cowplot::plot_grid(plot_list_sampleprop[['baseline']],plot_list_sampleprop[[3]],plot_list_sampleprop[[2]],plot_list_sampleprop[[1]])
  dev.off()
  
    
  ###################
  # Plot comparison sampling effort x strata for each species under singlesp or multisp allocation of samples
  ###################
  
  #sort by common name
  df_spp$common<-spp1
  df_spp<-df_spp[order(df_spp$common),]
  
  for (s in 3:nrow(samp_df)) {
    
    #to use depth 
    s<-3
    
    #sampling design
    samp<-samp_df[s,'samp_scn']
    
    #load multispecies data
    load(paste0('./output/multisp_optimization_static_data.RData')) #df
    
    #load optimized stratification
    load(file = paste0("./output/ms_optim_allocations_",samp_df[s,'samp_scn'],".RData")) #all
    
    strata<-rbind(all$result_list$solution$indices,
                  data.frame(ID=rem_cells,X1=NA))
    colnames(strata)<-c('cell','Strata')
    
    #n cells by strata to calculate proportion of sampling effort as n samples/total cells
    strata_sum<-aggregate(cell ~ Strata, data = strata, FUN = length)
    names(strata_sum)[2]<-'total_cell'
    
    #merge with ss data
    ss<-all$ss_sample_allocations
    ss1<-data.frame(ss[,c(paste0('Str_',1:unique(samp_df$n_strata)))],row.names = ss$species)
    colnames(ss1)<-1:unique(samp_df$n_strata)
    ss2<-reshape2::melt(as.matrix(ss1))
    names(ss2)<-c('species','Strata','ss_samples')
    strata1<-merge(strata,ss2,by='Strata')
    
    #merge with ms data
    ms<-all$ms_sample_allocations
    ms_strata<-data.frame('Strata'=1:unique(samp_df$n_strata),
                          'ms_samples'=as.numeric(ms[1,16:30]))
    
    
    strata2<-merge(strata1,ms_strata,by='Strata')
    strata2<-merge(strata2,strata_sum,by='Strata')
    #strata2$prop<-strata2$n_samples/strata2$total_cell
    strata2$ratio<-log(strata2$ms_samples/strata2$ss_samples)
    dim(strata2)
    
    df1<-df[,c('Lat','Lon','cell')]
    df1<-merge(df1,strata2,by='cell')
    df1$Strata[is.na(df1$Strata)]<-999
    
    #to store plots
    plot_list_n<-list()
    plot_list_d<-list()
    
    for (isp in df_spp$spp) {
      
      #isp<-spp[1]
      
      df2<-subset(df1,species==isp)
      
      #ms CV
      ms_cv<-
        ms[,paste0('CV',match(isp,spp))]
      
      #ms SCV
      ms_scv<-(ms_cv*100)^2
      
      #ss CV
      ss_cv<-
        ss$CV[match(isp,spp)]
      
      #ss SCV
      ss_scv<-(ss_cv*100)^2
      
      #df to spatialpoint df
      coordinates(df2) <- ~ Lon + Lat
      crs(df2)<-c(crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
      
      #reproject coordinates for plotting purposes
      df_1<-spTransform(df2,'+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')
      df_2<-data.frame(df_1)
      
      #x and y cells
      xycells<-as.integer(sqrt(dim(df_1)[1]))
      
      # create a template raster
      r1 <- raster(ext=extent(df_1),ncol=xycells, nrow=xycells) #c(15800,15800) 7000
      
      #create raster
      r2<-rasterize(df_1, r1 ,field=c('Strata','ms_samples','ss_samples','ratio'))
      crs(r2) <- CRS('+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')
      r2[r2==999]<-NA
      
      #create polygon to get boundaries of each strata
      r3<-as.data.frame(r2,xy=TRUE)
      r4<-rasterToPolygons(r2$Strata,dissolve=TRUE,digits = 1)
      
      #color palette
      pal <- wes_palette("Zissou1", length(sort(unique(r3$ss_samples))), type = "continuous")
      color_scale_ss <- setNames(as.character(pal), sort(unique(r3$ss_samples)))
      r3<-r3[complete.cases(r3$Strata),] 
      
      #as factors
      r3$ss_samples<-as.factor(r3$ss_samples)
      #r3$ratio<-as.factor(r3$ratio)
      
      #common name
      com<-spp_name[which(spp_name$spp==isp),'common']
      
      #plot by number of samples
      pn<-
        ggplot()+
        geom_raster(data=r3,aes(x=x,y=y,fill=ss_samples))+
        geom_polygon(data=r4,aes(x=long,y=lat,group=group), colour="black", fill=NA)+
        scale_fill_manual(values = color_scale_ss,name='sampling effort')+
        guides(fill=guide_colorbar(title.position = 'top', title.hjust = 0.5,ticks.colour = NA,frame.colour = 'black'))+
        scale_x_continuous(expand = c(0,0),position = 'bottom',
                           breaks = c(-175,-170,-165,-160,-155),sec.axis = dup_axis())+
        coord_sf(crs = '+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs',
                 xlim = panel_extent$x-c(000000,100000),
                 ylim = panel_extent$y-c(0,300000),
                 label_axes = "-NE-")+
        theme(aspect.ratio = 1,panel.grid.major = element_blank(),
              panel.background = element_rect(fill = NA),panel.ontop = TRUE,
              legend.background =  element_rect(fill = "transparent", colour = "transparent"),legend.key.height= unit(20, 'points'),
              legend.key.width= unit(20, 'points'),axis.title = element_blank(),legend.position = 'none',
              panel.border = element_rect(fill = NA, colour = NA),legend.key = element_rect(color="black"),
              legend.spacing.y = unit(8, 'points'),
              axis.text=element_blank(),axis.ticks = element_blank(),
              plot.margin = margin(0.01,0.01,0.01,0.01), 
              axis.ticks.length = unit(-5,"points"),plot.title = element_text(size=12,hjust = 0.5,vjust=-5, face="bold"))+
        scale_y_continuous(expand = c(0,0),position = 'right',sec.axis = dup_axis())+
        #labs(title=paste0(com,'\n(msSCV=',round(ms_scv,digits = 3),'; ssSCV=',round(ss_scv,digits = 3),')'),fill='')
        labs(title=paste0(com,'\n(msCV=',round(ms_cv,digits = 3),'; ssCV=',round(ss_cv,digits = 3),')'),fill='')
      
      #plot by ratio
      pd<-
        ggplot()+
        geom_raster(data=r3,aes(x=x,y=y,fill=ratio))+
        geom_polygon(data=r4,aes(x=long,y=lat,group=group), color=rgb(128, 128, 128,50, names = NULL, maxColorValue = 200), fill=NA)+
        scale_fill_gradient2(midpoint = 0, low = "#F21A00", mid = "white",
                             high = "#3B9AB2",name='ss samples - ms samples')+
        guides(fill=guide_colorbar(title.position = 'top', title.hjust = 0.5,ticks.colour = NA,frame.colour = 'black'))+
        scale_x_continuous(expand = c(0,0),position = 'bottom',
                           breaks = c(-175,-170,-165,-160,-155),sec.axis = dup_axis())+
        coord_sf(crs = '+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs',
                 xlim = panel_extent$x-c(000000,100000),
                 ylim = panel_extent$y-c(0,300000),
                 label_axes = "-NE-")+
        theme(aspect.ratio = 1,panel.grid.major = element_blank(),
              panel.background = element_rect(fill = NA),panel.ontop = TRUE,
              legend.background =  element_rect(fill = "transparent", colour = "transparent"),legend.key.height= unit(20, 'points'),
              legend.key.width= unit(20, 'points'),axis.title = element_blank(),legend.position = 'none',
              panel.border = element_rect(fill = NA, colour = NA),legend.key = element_rect(color="black"),
              legend.spacing.y = unit(8, 'points'),
              axis.text=element_blank(),axis.ticks = element_blank(),
              plot.margin = margin(0.01,0.01,0.01,0.01), 
              axis.ticks.length = unit(-5,"points"),plot.title = element_text(size=14,hjust = 0.5,vjust=0, face="bold"))+
        scale_y_continuous(expand = c(0,0),position = 'right',sec.axis = dup_axis())+
        #labs(title=paste0(com,'\n(msSCV=',round(ms_scv,digits = 3),'; ssSCV=',round(ss_scv,digits = 3),')'),fill='')
        labs(title=paste0(com,fill=''))
      
      plot_list_n[[com]]<-pn
      plot_list_d[[com]]<-pd
    }
    
    
    #color palette
    pal <- wes_palette("Zissou1", length(sort(unique(r3$ms_samples))), type = "continuous")
    color_scale_ms <- setNames(as.character(pal), sort(unique(r3$ms_samples)))
    
    #as factors
    r3$ms_samples<-as.factor(r3$ms_samples)
    #r3$ratio<-as.factor(r3$ratio)
    
    #plot by number of samples
    pm<-
      ggplot()+
      geom_raster(data=r3,aes(x=x,y=y,fill=ms_samples))+
      geom_polygon(data=r4,aes(x=long,y=lat,group=group), color=rgb(128, 128, 128,50, names = NULL, maxColorValue = 200), fill=NA)+
      scale_fill_manual(values = color_scale_ms,name='sampling effort')+
      guides(fill=guide_colorbar(title.position = 'top', title.hjust = 0.5,ticks.colour = NA,frame.colour = 'black'))+
      scale_x_continuous(expand = c(0,0),position = 'bottom',
                         breaks = c(-175,-170,-165,-160,-155),sec.axis = dup_axis())+
      coord_sf(crs = '+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs',
               xlim = panel_extent$x-c(000000,100000),
               ylim = panel_extent$y-c(0,300000),
               label_axes = "-NE-")+
      theme(aspect.ratio = 1,panel.grid.major = element_blank(),
            panel.background = element_rect(fill = NA),panel.ontop = TRUE,
            legend.background =  element_rect(fill = "transparent", colour = "transparent"),legend.key.height= unit(20, 'points'),
            legend.key.width= unit(20, 'points'),axis.title = element_blank(),legend.position = 'none',
            panel.border = element_rect(fill = NA, colour = NA),legend.key = element_rect(color="black"),
            legend.spacing.y = unit(8, 'points'),
            axis.text=element_blank(),axis.ticks = element_blank(),
            plot.margin = margin(0.01,0.01,0.01,0.01), 
            axis.ticks.length = unit(-5,"points"),plot.title = element_text(size=14,hjust = 0.5,vjust=-5, face="bold"))+
      scale_y_continuous(expand = c(0,0),position = 'right',sec.axis = dup_axis())+
      labs(title=paste0('multispecies\n',gsub('_',' + ',samp_df[s,'strat_var'])),fill='')  
    
    #create legends
    legend_d<-
      ggplot()+
      geom_raster(data=r3,aes(x=x,y=y,fill=as.numeric(ratio)))+
      scale_fill_gradient2(midpoint = mean(range(r3$ratio)), low = "#F21A00", mid = "white",
                           high = "#3B9AB2",breaks=range(as.numeric(r3$ratio)),labels=c("Undersampled","Oversampled"),name='log(ms/ss stations)\n')+
      guides(fill=guide_colorbar(title.position = 'top', title.hjust = 0.5,ticks.colour = NA,frame.colour = 'black'))+
      theme(legend.position = 'right',legend.text = element_text(size=12),legend.title = element_text(size=14),legend.key.size = unit(20,"points"),legend.spacing.y = unit(10, 'points'),
            legend.direction = "vertical")+
      labs(fill='')
    
    legend_prop<-
      ggplot()+
      geom_raster(data=r3,aes(x=x,y=y,fill=as.numeric(ss_samples)))+
      scale_fill_gradientn(colours = pal,breaks=range(as.numeric(r3$ss_samples)),labels=c("Low","High"),name='sampling effort (stations/area)')+
      guides(fill=guide_colorbar(title.position = 'top', title.hjust = 0.5,ticks.colour = NA,frame.colour = 'black'))+
      theme(legend.position = 'right',legend.text = element_text(size=12),legend.title = element_text(size=14),legend.key.size = unit(20,"points"))+
      labs(fill='')
    
    legend1 <- cowplot::get_legend( 
      legend_d + 
        theme(legend.position = "right",
              legend.justification = c(0.72,0.5)) 
    ) 
    plot(legend1)
    
    legend2 <- cowplot::get_legend( 
      legend_prop + 
        theme(legend.position = "bottom") 
    ) 
    
    pgrid1<-cowplot::plot_grid(plotlist = plot_list_d, nrow = 2)
    pgrid2<-cowplot::plot_grid(plotlist = plot_list_n, nrow = 2)
    
    if (s<-3) {
      pgridi1<-cowplot::plot_grid(plotlist = plot_list_d[c(1:7)],nrow=1)
      pgridi1<-cowplot::plot_grid(pgridi1,legend1, ncol = 2, rel_widths =  c(1, .14))
      
      pgridi2<-cowplot::plot_grid(plotlist = plot_list_d[c(8:14)],nrow=1)
      pgridi2<-cowplot::plot_grid(pgridi2,legend1, ncol = 2, rel_widths =  c(1, .14))
      
      pgridi3<-cowplot::plot_grid(plotlist = plot_list_d[sort(names(plot_list_d))[c(4,14,6,11,2)]],nrow=1)
      pgridi3<-cowplot::plot_grid(pgridi3,legend1, ncol = 2, rel_widths =  c(1, .14))
    }
    
    # #save plots
    # ragg::agg_png(paste0('./figures/sampling designs ss ratio_',samp_df[s,'strat_var'],'.png'), width = 20, height = 7, units = "in", res = 300)
    # print(cowplot::plot_grid(pgrid1, legend1, nrow = 2, rel_heights = c(1, .1)))
    # dev.off()
    # 
    # ragg::agg_png(paste0('./figures/sampling designs ss n_',samp_df[s,'strat_var'],'.png'), width = 20, height = 7, units = "in", res = 300)
    # print(cowplot::plot_grid(pgrid2, legend2, nrow = 2, rel_heights = c(1, .1)))
    # dev.off()
    # 
    # ragg::agg_png(paste0('./figures/sampling designs ms_',samp_df[s,'strat_var'],'.png'), width = 5, height = 5, units = "in", res = 300)
    # print(pm)
    # dev.off()
    
  }  
  
  ###################
  # Plot spatial random fields
  ###################
  
  #to store plots
  plot_list_rf<-list()

  for (isp in df_spp$spp) {
    
    #isp<-'Gadus macrocephalus'
    cat(paste(" #############  ",isp ," #############\n"))
    
    #common name
    com<-spp_name[which(spp_name$spp==isp),'common']
    
    #fit file
    ff<-list.files(paste0('./shelf EBS NBS VAST/',isp,'/'),'fit',recursive=TRUE)
    
    #load fit file
    load(paste0('./shelf EBS NBS VAST/',isp,'/',ff)) #fit
    
    #get spatial random fields
    spf<-fit$Report$Omega1_gc
    
    #get dataframe
    D_gt <- spf
    D_gt<-data.frame('cell'=c(1:fit$spatial_list$n_g),D_gt,check.names = FALSE)
    
    #merge with grid
    D_gt1<-merge(D_gt,grid,by='cell')
    
    #df to spatialpoint df
    coordinates(D_gt1) <- ~ Lon + Lat
    crs(D_gt1)<-c(crs='+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')
    
    #reproject coordinates for plotting purposes
    #df_1<-spTransform(D_gt1,'+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')
    df_2<-data.frame(D_gt1)
    
    #x and y cells
    xycells<-as.integer(sqrt(dim(D_gt1)[1]))
    
    # create a template raster
    r1 <- raster(ext=extent(df_1),ncol=xycells, nrow=xycells) #c(15800,15800) 7000
    
    #create raster
    r2<-rasterize(D_gt1, r1 ,field=c('1'))
    crs(r2) <- CRS('+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')
    r2[r2==999]<-NA
    
    #create polygon to get boundaries of each strata
    r3<-as.data.frame(r2,xy=TRUE)
    
    #Alaska layer
    ebs_layers <- akgfmaps::get_base_layers(select.region = "ebs", set.crs = "EPSG:3338")
    ak_sppoly<-as(ebs_layers$akland, 'Spatial')
    
    #plot
    prf<-
      ggplot() +
      geom_raster(data=r3,aes(x=x,y=y,fill=layer))+
      #geom_polygon(data=r4,aes(x=long,y=lat,group=group), colour="black", fill=NA)+
      guides(fill=guide_colorbar(title.position = 'top', title.hjust = 0.5,ticks.colour = NA,frame.colour = 'black'))+
      scale_fill_viridis_c(option = 'A',name=('Spatial Random\nField deviations'),
                           guide = guide_colorbar(  frame.colour = "black",ticks.colour = 'black'),na.value=rgb(1, 0, 0, 0))+
      scale_x_continuous(expand = c(0,0),position = 'bottom',
                         breaks = c(-175,-170,-165,-160,-155),sec.axis = dup_axis())+
      coord_sf(crs = '+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs',
               xlim = panel_extent$x-c(000000,100000),
               ylim = panel_extent$y-c(0,300000),
               label_axes = "-NE-")+
      theme(aspect.ratio = 1,panel.grid.major = element_blank(),
            panel.background = element_rect(fill = NA),panel.ontop = TRUE,
            legend.background =  element_rect(fill = "transparent", colour = "transparent"),legend.key.height= unit(20, 'points'),
            legend.key.width= unit(20, 'points'),axis.title = element_blank(),legend.position = 'none',
            panel.border = element_rect(fill = NA, colour = NA),legend.key = element_rect(color="black"),
            legend.spacing.y = unit(8, 'points'),
            axis.text=element_blank(),axis.ticks = element_blank(),
            plot.margin = margin(0.01,0.01,0.01,0.01), 
            axis.ticks.length = unit(-5,"points"),plot.title = element_text(size=12,hjust = 0.5,vjust=-5, face="bold"))+
      scale_y_continuous(expand = c(0,0),position = 'right',sec.axis = dup_axis())+
      labs(title=paste0('',fill=''))
    
    #store plot
    plot_list_rf[[isp]]<-prf
  } 
  
  
  prandi1<-cowplot::plot_grid(plotlist = plot_list_rf[1:7], nrow = 1)
  prandi2<-cowplot::plot_grid(plotlist = plot_list_rf[8:14], nrow = 1)
  prandi3<-cowplot::plot_grid(plotlist = plot_list_rf[c(4,14,6,11,2)], nrow = 1)  #plot_list_rf[c(4,13,7,12,2)], nrow = 1)


  #plot for common legend
  legend_rf<-
    ggplot()+
    geom_raster(data=r3,aes(x=x,y=y,fill=as.numeric(layer)))+
    scale_fill_viridis_c(breaks=range(as.numeric(r3$layer),na.rm = TRUE),labels=c("Low","High"),option = 'A',name=('         Spatial Random\n         Field deviations\n'),
                         guide = guide_colorbar(  frame.colour = "black",ticks.colour = 'black'),na.value=rgb(1, 0, 0, 0))+
    guides(fill=guide_colorbar(title.position = 'top', title.hjust = 0.5,ticks.colour = NA,frame.colour = 'black'))+
    theme(legend.position = 'left',legend.text = element_text(size=12),legend.title = element_text(size=14),legend.key.size = unit(20,"points"),legend.spacing.y = unit(10, 'points'),)+
    labs(fill='')
  
  #legend
  legend1 <- cowplot::get_legend( 
    legend_rf + 
      theme(legend.position = "right",
            legend.justification = c(0,0.5),
            #legend.position = c(1,0),
            #legend.margin = unit(0,"lines"),
            #legend.box = "vertical",
            #legend.key.size = unit(1,"lines"),
            ) 
  ) 
  plot(legend1)
  
  #prandi<-
  prandi1<-cowplot::plot_grid(prandi1, legend1, ncol = 2, rel_widths = c(1, .14))
  prandi2<-cowplot::plot_grid(prandi2, legend1, ncol = 2, rel_widths = c(1, .14))
  prandi3<-cowplot::plot_grid(prandi3, legend1, ncol = 2, rel_widths = c(1, .14))

  #cowplot::plot_grid(pgridi,prandi,nrow=2)
  
  print(cowplot::plot_grid(pgridi1, prandi1, nrow = 2))
  print(cowplot::plot_grid(pgridi2, prandi2, nrow = 2))
  print(cowplot::plot_grid(pgridi3, prandi3, nrow = 2))

  ragg::agg_png(paste0('./figures/str_ss_ms_rf1b.png'),  width = 20, height = 6, units = "in", res = 300)
  print(cowplot::plot_grid(pgridi1, prandi1, nrow = 2))
  dev.off()
  ragg::agg_png(paste0('./figures/str_ss_ms_rf2b.png'),  width = 20, height = 6, units = "in", res = 300)
  print(cowplot::plot_grid(pgridi2, prandi2, nrow = 2))
  dev.off()
  
  #save plot
  ragg::agg_png(paste0('./figures/str_ss_ms_rfselb.png'),  width = 15, height = 6, units = "in", res = 300)
  print(cowplot::plot_grid(pgridi3, prandi3, nrow = 2))
  dev.off()
  
  # #save plot
  # ragg::agg_png(paste0('./figures/SpatialRandomFields.png'),  width = 20, height = 7, units = "in", res = 300)
  # print(cowplot::plot_grid(pgrid1, legend1, nrow = 2, rel_heights = c(1, .15)))
  # dev.off()
  