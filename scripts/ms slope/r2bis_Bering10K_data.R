####################################################################
####################################################################
##
##    Extract sea bottom temperature (SBT) from netcdf of Bering 10K ROMS
##    Netcdf downloaded from https://data.pmel.noaa.gov/aclim/thredds/
##    Incorporate Temp (SBT) to Bering Sea grid
##    Incorporate Temp (SBT) to data_geostat file from Bering Sea 
##    (shelf Eastern Bering Sea, slope Eastern Bering Sea, northern Bering Sea)
##    Save data_geostat_temp file to fit VAST
##    Daniel Vilas (danielvilasgonzalez@gmail.com/dvilasg@uw.edu/daniel.vilas@noaa.gov)
##    Lewis Barnett, Stan Kotwicki, Zack Oyafuso, Megsie Siple, Leah Zacher, Lukas Defilippo, Andre Punt
##
####################################################################
####################################################################

#clear all objects
rm(list = ls(all.names = TRUE)) 
#free up memrory and report the memory usage
gc() 

#libraries from cran to call or install/load
pack_cran<-c('ncdf4','raster','FNN','lubridate','ggpubr')

#install pacman to use p_load function - call library and if not installed, then install
if (!('pacman' %in% installed.packages())) {
  install.packages("pacman")}

#load/install packages
pacman::p_load(pack_cran,character.only = TRUE)

#setwd - depends on computer using
#out_dir<-'C:/Users/Daniel.Vilas/Work/Adapting Monitoring to a Changing Seascape/' #NOAA laptop  
#out_dir<-'/Users/daniel/Work/Adapting Monitoring to a Changing Seascape/' #mac
#out_dir<-'/Users/daniel/Work/VM' #VM
#setwd(out_dir)

#range years of data
sta_y<-1982
end_y<-2022

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
       #'Lepidopsetta sp.',
       'Chionoecetes bairdi',
       'Sebastes alutus',
       'Sebastes melanostictus',
       'Atheresthes evermanni',
       'Sebastes borealis',
       'Sebastolobus alascanus',
       'Glyptocephalus zachirus',
       'Bathyraja aleutica')

#remove Anoploma and Reinhardtius because habitat preference reasons
#spp<-setdiff(spp, c('Anoplopoma fimbria','Reinhardtius hippoglossoides'))

#common names
spp1<-c('Yellowfin sole',
        'Alaska pollock',
        'Pacific cod',
        'Arrowtooth flounder',
        'Greenland turbot',
        'Northern rock sole',
        'Flathead sole',
        'Alaska plaice',
        'Bering flounder',
        'Arctic cod',
        'Saffron cod',
        'Sablefish',
        'Snow crab',
        'Blue king crab',
        'Red king crab',
        'Tanner crab',
        'Pacific ocean perch',
        'Rougheye and blackspotted rockfish',
        'Kamchatka flounder',
        'Shortraker rockfish',
        'Shortspine thornyhead',
        'Rex sole',
        'Aleutian skate')

#####################################
# Get haul (sampling stations)
#####################################

#read csv file
#haul<-readRDS(paste0('data/data_raw/',file$name))
#haul<-readRDS(paste0('data/data_raw/afsc_haul_raw_2023_2_21.rds'))
haul<-readRDS('data/data_raw/afsc_haul_raw_2023_2_21.rds')

dim(haul);length(unique(haul$hauljoin))

#get year and month from haul
haul$month<-month(as.POSIXlt(haul$date, format="%d/%m/%Y"))
haul$year<-year(as.POSIXlt(haul$date, format="%d/%m/%Y"))

#####################################
# Bering Sea grid
#####################################

#https://github.com/James-Thorson-NOAA/FishStatsUtils/tree/main/data
#load grids
eastern_bering_sea_grid <- FishStatsUtils::eastern_bering_sea_grid
dim(eastern_bering_sea_grid)
northern_bering_sea_grid <- FishStatsUtils::northern_bering_sea_grid
dim(northern_bering_sea_grid)
bering_sea_slope_grid <- FishStatsUtils::bering_sea_slope_grid
dim(bering_sea_slope_grid)

#convert to dataframe and add region
eastern_bering_sea_grid<-as.data.frame(eastern_bering_sea_grid)
eastern_bering_sea_grid$region<-'EBSshelf'
northern_bering_sea_grid<-as.data.frame(northern_bering_sea_grid)
northern_bering_sea_grid$region<-'NBS'
bering_sea_slope_grid<-as.data.frame(bering_sea_slope_grid)
bering_sea_slope_grid$Stratum<-'NA'
bering_sea_slope_grid$region<-'EBSslope'

#rbind regions
grid.ebs<-rbind(northern_bering_sea_grid,
               eastern_bering_sea_grid,
               bering_sea_slope_grid[,c("Lat","Lon","Area_in_survey_km2",'Stratum',"region")])

#check km2 per region (grid data)
aggregate(Area_in_survey_km2 ~ region, grid.ebs, FUN=sum)

#####################################
# Add GEBCO depth (downloaded on August 2022)
#####################################

# #create directory
# dir.create('./bathymetry/',showWarnings = FALSE)
# 
# #get raster depth from gebco (https://download.gebco.net/)
# #get id shared folder from google drive
# id.bering.folder<-files[which(files$name=='Bathymetry'),'id']
# 
# #list of files and folder
# files.1<-googledrive::drive_ls(id.bering.folder$id)
# id.data<-files.1[which(files.1$name=='gebco_2022_n70.0_s50.0_w-180.0_e-155.0.asc'),]
# 
# googledrive::drive_download(file=id.data$id,
#                             path = paste0('./bathymetry/',id.data$name),
#                             overwrite = TRUE)

#read raster GEBCO data
r<-raster('data/data_raw/gebco_2022_n70.0_s50.0_w-180.0_e-155.0.asc')

#extract depth values for each station of grid - using GEBCO data
rr<-raster::extract(r, SpatialPoints(cbind(grid.ebs$Lon,grid.ebs$Lat)))
grid.ebs$DepthGEBCO<--rr
grid.ebs$depth_m <-grid.ebs$DepthGEBCO
summary(grid.ebs)
EBSgrid<-grid.ebs
  
##########################################################################
# Loop over years to get the temperature for each year in each sampling station (from ROMS)
##########################################################################

# #create directory
# dir.create('./bering 10k roms/',showWarnings = FALSE)
# 
# #get id shared folder from google drive
# id.roms.folder<-files[which(files$name=='Bering 10K ROMS'),'id']
# 
# #list of files and folder of netcd files
# files.1<-googledrive::drive_ls(id.roms.folder$id)
# id.data<-files.1[which(files.1$name=='netcdf_historical'),'id']
# nc_histfiles<-googledrive::drive_ls(id.data$id)
# id.data<-files.1[which(files.1$name=='netcdf_forecast'),'id']
# nc_forfiles<-googledrive::drive_ls(id.data$id)
# files.hist<-sort(nc_histfiles$name)
# files.for<-sort(nc_forfiles$name)

# list local netcdf files
base_dir <- "data/data_raw/bering_10k_roms"

hist_dir <- file.path(base_dir, "netcdf_historical")
for_dir  <- file.path(base_dir, "netcdf_forecast")

stopifnot(dir.exists(hist_dir))
stopifnot(dir.exists(for_dir))

files.hist <- sort(list.files(
  hist_dir,
  pattern = "\\.nc$",
  full.names = FALSE
))

files.for <- sort(list.files(
  for_dir,
  pattern = "\\.nc$",
  full.names = FALSE
))

#create df to store results
grid.ebs_year<-data.frame(matrix(nrow = 0,
                                 ncol = ncol(EBSgrid)+2))
colnames(grid.ebs_year)<-c(colnames(EBSgrid),
                           'Temp',
                           'Year')
get_roms_file <- function(y, files.hist, files.for) {
  
  if (y >= 1980 && y <= 1984) return(list(type = "hist", file = files.hist[1]))
  if (y >= 1985 && y <= 1989) return(list(type = "hist", file = files.hist[2]))
  if (y >= 1990 && y <= 1994) return(list(type = "hist", file = files.hist[3]))
  if (y >= 1995 && y <= 1999) return(list(type = "hist", file = files.hist[4]))
  if (y >= 2000 && y <= 2004) return(list(type = "hist", file = files.hist[5]))
  if (y >= 2005 && y <= 2009) return(list(type = "hist", file = files.hist[6]))
  if (y >= 2010 && y <= 2014) return(list(type = "hist", file = files.hist[7]))
  if (y >= 2015 && y <= 2019) return(list(type = "hist", file = files.hist[8]))
  if (y == 2020)              return(list(type = "hist", file = files.hist[9]))
  if (y >= 2021 && y <= 2024) return(list(type = "for",  file = files.for[1]))
  
  stop("Year not covered")
}

#loop over years to incorporate values into the Bering Sea grid
for (y in sta_y:end_y) {
  
  #y<-2019
  
  #print year to check progress
  cat(paste("    ----- year", y, "-----\n"))  
  
  
  sel <- get_roms_file(y, files.hist, files.for)
  
  if (sel$type == "hist") {
    file_path <- file.path(hist_dir, sel$file)
  } else {
    file_path <- file.path(for_dir, sel$file)
  }
  
  if (!file.exists(file_path)) {
    stop(paste("Missing file:", file_path))
  }
  
  # open NetCDF
  nc <- ncdf4::nc_open(file_path)
  
  #nc<-nc_open('/Users/daniel/Downloads/B10K-K20P19_CORECFS_1970-1974_average_Cop_integrated.nc')
  names(nc$var)
  
  #dimensions netcdf files
  #258 rows
  #182 cols
  #46956 cells
  #259 time steps
  
  #get latitude
  lats <- ncvar_get(nc,"lat_rho")
  
  #get longitude
  lons <- ncvar_get(nc,"lon_rho")
  
  #get SBT
  temp<-ncvar_get(nc,'temp')
  
  #get time
  t_axis<-ncvar_get(nc,"ocean_time")
  
  #convert time
  time_axis <- as.POSIXct(t_axis, origin = "1900-01-01", tz = "GMT") 
  
  #get weekly temp slices from specific year y
  nc_y<-ncvar_get(nc, "temp")[,,which(grepl(paste0(y,'-'),time_axis))]
  
  #get mean matrix for this year
  mean_nc<-apply(nc_y,c(1,2),mean,na.rm=TRUE)
  
  #create dataframe with lats, lons and mean year SBT
  df_nc<-data.frame(Lat=as.vector(lats),
                    Lon=as.vector(lons),
                    temp=as.vector(mean_nc))
  
  #longitude are in eastern. get SBT for the western hemisphere (Bering Sea). longitude greater than 180 degrees
  df_nc1<-df_nc[which(df_nc$Lon>=180),]
  
  #convert eastern longitude to western values (higher). longitude should be negative
  df_nc1$Lon<-df_nc1$Lon-180-180
  
  #filter values from the grid 
  df_nc2<-subset(df_nc1,Lat >= min(EBSgrid$Lat) & Lat <= max(EBSgrid$Lat) & Lon >= min(EBSgrid$Lon) & Lon <= max(EBSgrid$Lon))
  
  #remove rows with NAs
  df_nc3<-df_nc2[complete.cases(df_nc2),]
  
  #create spatial object from df
  coordinates(df_nc3) <- ~ Lon + Lat
  
  #create spatial object from grid
  spg <- EBSgrid
  coordinates(spg) <- ~ Lon + Lat
  
  #get the nearests points from one df to other df
  nn<-get.knnx(coordinates(df_nc3),coordinates(spg),1)
  nc_index<-nn$nn.index[,1]
  
  #get SBT
  temps<-as.data.frame(df_nc3)$temp[nc_index]
  EBSgrid$Temp<-temps
  EBSgrid$Year<-y
  
  #incorporate SBT
  grid.ebs_year<-rbind(grid.ebs_year,as.data.frame(EBSgrid))
  
}

#save grid Bering Sea with SBT and depth as dataframe
save(grid.ebs_year,file = 'data/data_processed/grid_EBS_NBS.RData')
#load(file = 'Data/data_processed/grid_EBS_NBS.RData')


#####################################
# LOOP OVER SPP
#####################################

#loop over species to add SBT to data_geostat
for (sp in spp) {
  
  #sp<-spp[3]
  
  #print species to check progress
  cat(paste(" ############# ", sp, " #############\n"))
  
  #open data_geostat
  df1<-readRDS(paste0('data/data_processed/species/',sp,'/data_geostat.rds'))
  
  #create df to store results
  df1_temp<-data.frame(matrix(nrow=0,
                              ncol=ncol(df1)+4))
  colnames(df1_temp)<-c(colnames(df1),"Temp")
  
  for (y in sta_y:end_y) {
    
    #y<-2020
    
    #print year to check progress
    cat(paste("    ---- year", y, "----\n"))
    
    #subset df by year
    df2<-subset(df1,year==y)
    
    #if no data for that year, use stations file
    if (nrow(df2)==0) {
      
      #filter year and remove negative depth values
      st_year1<-subset(grid.ebs_year,Year==y & depth_m > 0)
      df3<-data.frame(matrix(nrow = nrow(st_year1),ncol = ncol(df2)))
      colnames(df3)<-colnames(df2)
      df3$scientific_name<-sp
      df3$year<-y
      df3$lat_start<-st_year1$Lat
      df3$lat_end<-st_year1$Lat
      df3$lon_start<-st_year1$Lon
      df3$lon_end<-st_year1$Lon
      df3$depth_m<-st_year1$depth_m
      df3$Temp<-st_year1$Temp
      df3$bottom_temp_c<-st_year1$Temp
      df2<-df3
      
    }
    
    sel <- get_roms_file(y, files.hist, files.for)
    
    if (sel$type == "hist") {
      file_path <- file.path(hist_dir, sel$file)
    } else {
      file_path <- file.path(for_dir, sel$file)
    }
    
    if (!file.exists(file_path)) {
      stop(paste("Missing file:", file_path))
    }
    
    # open NetCDF
    nc <- ncdf4::nc_open(file_path)
    
    #dimensions netcdf file
    #258 rows
    #182 cols
    #46956 cells
    #259 time steps
    
    #get variables
    #names(nc$var)
    
    #get latitude
    lats <- ncvar_get(nc,"lat_rho")
    
    #get longitude
    lons <- ncvar_get(nc,"lon_rho")
    
    #get SBT
    temp<-ncvar_get(nc,'temp')
    
    #get time
    t_axis<-ncvar_get(nc,"ocean_time")
    
    #convert time
    time_axis <- as.POSIXct(t_axis, origin = "1900-01-01", tz = "GMT") 
    
    #get weekly temp slices from specific year y
    nc_y<-ncvar_get(nc, "temp")[,,which(grepl(paste0(y,'-'),time_axis))]
    
    #get mean matrix for this year
    mean_nc<-apply(nc_y,c(1,2),mean,na.rm=TRUE)
    
    #create dataframe with lats, lons and mean year SBT
    df_nc<-data.frame(Lat=as.vector(lats),
                      Lon=as.vector(lons),
                      temp=as.vector(mean_nc))
    
    #longitude are in eastern. get SBT for the western hemisphere (Bering Sea). longitude greater than 180 degrees
    df_nc1<-df_nc[which(df_nc$Lon>=180),]
    
    #convert eastern longitude to western values (higher). longitude should be negative
    df_nc1$Lon<-df_nc1$Lon-360
    
    #filter values from the grid 
    df_nc2 <- subset(x = df_nc1, subset = Lat >= min(df1$lat_start) 
                     & Lat <= max(df1$lat_start) 
                     & Lon >= min(df1$lon_start) 
                     & Lon <= max(df1$lon_start))
    
    #remove NA rows
    df_nc3<-df_nc2[complete.cases(df_nc2),]
    
    #create a spatial object
    coordinates(df_nc3) <- ~ Lon + Lat
    #plot(df_nc3)
    #as.data.frame(df_nc)$temp[nc_index]
    spg <- df2
    coordinates(spg) <- ~ lon_start + lat_start
    
    #get the nearests points from one df to other df
    nn<-get.knnx(coordinates(df_nc3),coordinates(spg),1)
    nc_index<-nn$nn.index[,1]
    
    #get SBT
    temps<-as.data.frame(df_nc3)$temp[nc_index]
    df2$Temp<-temps
    
    #add results
    df1_temp<-rbind(df1_temp,df2)
    
  }
  
  #scale covariates
  df1_temp$Scalebottom_temp_c<-scale(df1_temp$bottom_temp_c)
  df1_temp$ScaleTemp<-scale(df1_temp$Temp)
  df1_temp$LogDepth<-log(df1_temp$depth_m)
  df1_temp$ScaleLogDepth<-scale(df1_temp$LogDepth)
  
  #xx<-df1_temp[which(is.na(df1_temp$cpue_noha)),]
  #summary(xx$year)
  
  #save data_geostat with SBT
  saveRDS(df1_temp,
          paste0('data/data_processed/species/',sp,'/data_geostat_temp.rds'))
  
}

#check temp ROMS vs temp in situ
#save data_geostat with SBT
data_geostat <-
  readRDS(file = 'data/data_processed/species/Gadus macrocephalus/data_geostat_temp.rds')

#plot check temperatures
plot(x=data_geostat$Temp,y=data_geostat$bottom_temp_c,xlab='ROMS',ylab='insitu')
ggplot(data_geostat, aes(x=Temp, y=bottom_temp_c)) + 
  geom_point(shape=18, color="blue", alpha = 0.5)+
  stat_cor(method = "pearson", label.x = 0, label.y = 20)+
  geom_smooth(method=lm,  linetype="dashed",
              color="darkred") +
  geom_abline(intercept = 0, slope = 1) +
  coord_fixed() +
  xlab('ROMS temp C') +
  ylab('in situ temp C')
ggsave("figures/roms_insitu_temp_compare.png")
