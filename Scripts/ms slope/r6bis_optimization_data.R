####################################################################
####################################################################
##
##    Get true index from the OM, prepare EBS+NBS data for optimization 
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
pack_cran<-c("splines",'SamplingStrata','wesanderson','dplyr','sp','rgeos','scales','rnaturalearth','grid','ggplot2')

#install pacman to use p_load function - call library and if not installed, then install
if (!('pacman' %in% installed.packages())) {
  install.packages("pacman")}

#install coldpool to extract SBT for the EBS
if (!('VAST' %in% installed.packages())) {
  devtools::install_github("james-thorson/VAST@main", INSTALL_opts="--no-staged-install")};library(VAST)

#install akgfmaps to extract shapefile of Alaska
if (!('akgfmaps' %in% installed.packages())) {
  devtools::install_github('afsc-gap-products/akgfmaps')};library(akgfmaps)

#load/install packages
pacman::p_load(pack_cran,character.only = TRUE)

#setwd - depends on computer using
#out_dir<-'C:/Users/Daniel.Vilas/Work/Adapting Monitoring to a Changing Seascape/' #NOAA laptop  
out_dir<-'/Users/daniel/Work/Adapting Monitoring to a Changing Seascape/' #mac
#out_dir<-'/Users/daniel/Work/VM' #VM
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
       #'Lepidopsetta sp.',
       'Chionoecetes bairdi',
       'Sebastes alutus',
       'Sebastes melanostictus',
       'Atheresthes evermanni',
       'Sebastes borealis',
       'Sebastolobus alascanus',
       'Glyptocephalus zachirus',
       'Bathyraja aleutica')

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

###############################
# convergenced spp
###############################

df_conv<-read.csv('./tables/slope_conv.csv')

# spp_conv_slope<-c(
#   df_conv[which(df_conv$slope=='There is no evidence that the model is not converged'),'spp'],
#   'Bathyraja aleutica')

spp_conv_ebsnbs<-c(
  df_conv[which(df_conv$EBS_NBS=='convergence'),'spp'])


###################################
# LOAD GRID EBS (remember to keep the same order as in fit_model if multiple grids)
###################################

#load grid data
#https://github.com/James-Thorson-NOAA/FishStatsUtils/tree/main/data
#https://github.com/danielvilasgonzalez/Bering_redesign/blob/main/Scripts/04_Bering10K_data.R
load('./data processed/grid_EBS_NBS.RData') #grid.ebs_year
names(grid.ebs_year)[7]<-'Depth'

#remove slope grid
#load grid of NBS and EBS
load('./extrapolation grids/northern_bering_sea_grid.rda')
load('./extrapolation grids/eastern_bering_sea_grid.rda')
grid<-as.data.frame(rbind(data.frame(northern_bering_sea_grid,region='NBS'),data.frame(eastern_bering_sea_grid,region='EBS')))
grid$cell<-1:nrow(grid)

#annually grid data
grid.ebs_year1<-grid.ebs_year[which(grid.ebs_year$region!='EBSslope'),]
ncells<-nrow(grid.ebs_year1[which(grid.ebs_year1$Year==1982),])
yrs<-c(1982:2019,2021:2022)

#classify cold and warm years
#sel years (2002:2016)
cyrs<-c(2006:2013)
wyrs<-c(2002:2005,2014:2016)
n_yrs<-length(c(cyrs,wyrs))
yrs<-sort(c(cyrs,wyrs))

#build array for temporal array to store results
temp_dens_vals <- array(NA,
                        dim = c(ncells,
                                length(yrs),
                                length(spp)),
                        dimnames = list(1:ncells,yrs,spp))

# #build array for static array
# static_dens_vals <- array(NA,
#                           dim = c(ncells,
#                                   length(c('Lat','Lon','cell','Depth','meanTemp','meanDensity','varTemp','varDensity','sumDensity','sqsumDensity',"include","meanTempF","LonE")),
#                                   length(spp)),
#                           dimnames = list(1:ncells,
#                                           c('Lat','Lon','cell','Depth','meanTemp','meanDensity','varTemp','varDensity','sumDensity','sqsumDensity',"include","meanTempF","LonE"),
#                                           spp))
#array to store indices
true_index<-array(NA,
                  dim = list(length(yrs),3,length(spp)),
                  dimnames = list(yrs,c('EBS_NBS','NBS','EBS'),spp))

# ###################################
# # LOOP OVER SPECIES
# ###################################

#loop over species
for (sp in spp) {
  
  #sp<-spp[1]
  
  #print scenario to check progress
  cat(paste(" #############   Species", sp, match(sp,spp), 'out of',length(spp),  "  #############\n"))
  
  #create folder optimization data
  dir.create(paste0('./output/species/',sp))
  
  #create folder optimization data
  dir.create(paste0('./output/species/',sp,'/optimization data/'))
  
  
  if (sp %in% spp_conv_ebsnbs) {
    
    ###################################
    # LOAD FIT OBJECTS (fit<-from VAST::fit_model()) and pr_list<-VAST::project_model()
    ###################################
    
    #fit file
    ff<-list.files(paste0('./shelf EBS NBS VAST/',sp,'/'),'fit',recursive=TRUE)
    
    
    #load fit file
    load(paste0('./shelf EBS NBS VAST/',sp,'/',ff)) #fit
    
    #################################################
    # ARRANGE PREDICTED DENSITIES FROM OM
    #################################################
    
    # if (sp %in% c('Atheresthes stomias','Atheresthes evermanni')){
    #   yrs<-c(1991:2019,2021:2022)
    # } else{
    #   yrs<-c(1982:2019,2021:2022)
    # }
    
    #get predicted densities for sp
    temp_dens_vals[,as.character(yrs),sp] <- unlist(fit$Report$D_gct[, 1, as.character(yrs)]) #[kg/km2]
    
    #density_input<-temp_dens_vals
    D_gt<-unlist(fit$Report$D_gct[, 1, as.character(yrs)])
    
    #get true index for NBS_EBS, NBS and EBS
    true_index[paste0(yrs),,sp]<-fit$Report$Index_ctl[1,paste0(yrs),1:3 ]
    
    #dataframe of cells with predictions
    D_gt<-data.frame('cell'=c(1:fit$spatial_list$n_g),D_gt)
    
    #years simulated
    yrs<-as.numeric(yrs)
    
    #rename years predictions 
    colnames(D_gt)<-c('cell',yrs) #,project_yrs
    
    #reshape
    D_gt1<-reshape2::melt(D_gt,id=c('cell'))
    
    #merge cells and predictions
    D_gt2 <- merge(D_gt1, grid, by=c('cell'))
    
    #get map info
    mdl <- make_map_info(Region = fit$settings$Region, 
                         spatial_list = fit$spatial_list,
                         Extrapolation_List = fit$extrapolation_list)
    
    #merge cells and predictions
    D <- merge(D_gt2, mdl$PlotDF, by.x=c('cell',"Lat",'Lon'), by.y=c('x2i',"Lat",'Lon'))
    
    #rename columns
    colnames(D)<-c('cell','Lat','Lon','Year','Density','Area','Stratum','region','Include')
    
    #create biomass
    D$Biomass<-D$Density*D$Area
    D$Biomass<-drop_units(D$Biomass)
    
    #get grid data for yrs in the simulation
    grid.ebs_year2<-grid.ebs_year1[which(grid.ebs_year1$Year %in% yrs),]
    
    #merge grid data and predictions
    D1<-merge(D,grid.ebs_year2,by=c('Lat','Lon','Year'))
    D1$Biomass_sq<-(D1$Biomass)^2
    D1$Density_sq<-(D1$Density)^2
    #subset by year (maybe to change to get for the forecasted ones)
    
    if (sp %in% c('Atheresthes stomias','Atheresthes evermanni')){
      D1<-subset(D1,Year %in% c(1991:2022))
    }
    
    #add regime based on year
    D1$regime<-ifelse(D1$Year %in% cyrs,'cold','warm')
 
    #DYNAMIC
    #static sampling so, we want to aggregate annual predictions: mean density, mean temp, and temp var
    D2dyn<-aggregate(cbind(Temp,Density) ~ Lat+Lon+cell+Depth+regime, data = D1, FUN = mean, na.rm = TRUE)
    D3dyn<-aggregate(cbind(Temp,Density) ~ Lat+Lon+cell+Depth+regime, data = D1, FUN = var, na.rm = TRUE)
    D4dyn<-aggregate(cbind(Density) ~ Lat+Lon+cell+Depth+regime, data = D1, FUN = sum, na.rm = TRUE)
    D41dyn<-aggregate(cbind(Density_sq) ~ Lat+Lon+cell+Depth+regime, data = D1, FUN = sum, na.rm = TRUE)
    colnames(D2dyn)[6:7]<-paste0('mean',colnames(D2dyn)[6:7])
    colnames(D3dyn)[6:7]<-paste0('var',colnames(D3dyn)[6:7])
    colnames(D4dyn)[6]<-paste0('sum',colnames(D4dyn)[6])
    colnames(D41dyn)[6]<-paste0('sum',colnames(D41dyn)[6])
    #merge aggregate values
    D5dyn<-merge(D2dyn,D3dyn,by=c('Lat','Lon','cell','Depth','regime'))
    D51dyn<-merge(D5dyn,D41dyn,by=c('Lat','Lon','cell','Depth','regime'))
    D6dyn<-merge(D51dyn,D4dyn,by=c('Lat','Lon','cell','Depth','regime'))
    
    #STATIC
    #static sampling so, we want to aggregate annual predictions: mean density, mean temp, and temp var
    D2<-aggregate(cbind(Temp,Density) ~ Lat+Lon+cell+Depth, data = D1, FUN = mean, na.rm = TRUE)
    D3<-aggregate(cbind(Temp,Density) ~ Lat+Lon+cell+Depth, data = D1, FUN = var, na.rm = TRUE)
    D4<-aggregate(cbind(Density) ~ Lat+Lon+cell+Depth, data = D1, FUN = sum, na.rm = TRUE)
    D41<-aggregate(cbind(Density_sq) ~ Lat+Lon+cell+Depth, data = D1, FUN = sum, na.rm = TRUE)
    colnames(D2)[5:6]<-paste0('mean',colnames(D2)[5:6])
    colnames(D3)[5:6]<-paste0('var',colnames(D3)[5:6])
    colnames(D4)[5]<-paste0('sum',colnames(D4)[5])
    colnames(D41)[5]<-paste0('sum',colnames(D41)[5])
    #merge aggregate values
    D5<-merge(D2,D3,by=c('Lat','Lon','cell','Depth'))
    D51<-merge(D5,D41,by=c('Lat','Lon','cell','Depth'))
    D6<-merge(D51,D4,by=c('Lat','Lon','cell','Depth'))
    
    #keep only cells with positive cells
    D6dyn$include<-ifelse(D6dyn$Depth>0,TRUE,FALSE)
    D6$include<-ifelse(D6$Depth>0,TRUE,FALSE)
    
    #convert SBT into F to get positive values only
    D6dyn$meanTempF<-(9/5)*D6dyn$meanTemp + 32
    D6$meanTempF<-(9/5)*D6$meanTemp + 32
    
    #add longitude on eastings to get positive values
    D6dyn$LonE<-D6dyn$Lon+180+180
    D6$LonE<-D6$Lon+180+180
    
    #get predictions for sp
    #static_dens_vals[,,sp] <- unlist(D6)
    
    #only one species
    tdf<-temp_dens_vals[,,sp]
    
    #list OM true CPUE and true index
    CPUE_index<-list('CPUE'=D1,
                     'true_index'=true_index[,,sp])
    
  } else {
    
    D1<-grid.ebs_year1
    D1$Biomass<-0
    D1$Density<-0
    D1$Biomass_sq<-(D1$Biomass)^2
    D1$Density_sq<-(D1$Density)^2
    
    #only one species
    tdf<-temp_dens_vals[,,sp]<-0
    true_index[,,sp]<-0
    #list OM true CPUE and true index
    CPUE_index<-list('CPUE'=D1,
                     'true_index'=true_index[,,sp])
  }
  
  #create a list of static df (static and dynamic)
  input_optim<-list('dynamic'=D6dyn,
                    'static'=D6)
  
  #save results list
  save(input_optim,file=paste0('./output/species/',sp,'/optimization data/optimization_static_data_ebsnbs.RData'))
  
  #save results list
  save(CPUE_index,file=paste0('./output/species/',sp,'/optimization data/OM_CPUE_index_ebsnbs.RData'))
  
  #save results list
  save(tdf,file=paste0('./output/species/',sp,'/optimization data/fit_temporal_data_ebsnbs.RData'))
}

#join optimmization data into a one single for STATIC AND DYNAMIC
for (sp in spp) {
  
  #sp<-spp[2]
  
  if (sp==spp[1]) {
    load(paste0('./output/species/',sp,'/optimization data/optimization_static_data_ebsnbs.RData'))
    st<-input_optim[['static']]
    dyn<-input_optim[['dynamic']]
    dfst<-st[,c("Lat","Lon","cell","Depth","meanTemp","varTemp","include","meanTempF","LonE")]  
    dfdyn<-dyn[,c("Lat","Lon","cell","Depth","meanTemp","varTemp","include","meanTempF","LonE",'regime')]  
  }
  
  if (sp %in% setdiff(spp,spp_conv_ebsnbs)){
    load(paste0('./output/species/',sp,'/optimization data/optimization_static_data_ebsnbs.RData'))
    dens<-data.frame(rep(0,ncells),rep(0,ncells))
    names(dens)<-c(paste0(sp,'_sumDensity'),paste0(sp,'_sumDensity_sq'))
    dfst<-cbind(dfst,dens)
    #dens1<-rbind(cbind(dens),cbind(dens,'regime'='warm'))
    dfdyn<-cbind(dfdyn,dens)
    
  } else {
  
    load(paste0('./output/species/',sp,'/optimization data/optimization_static_data_ebsnbs.RData'))
    st<-input_optim[['static']]
    dyn<-input_optim[['dynamic']]
    densst<-data.frame(st$sumDensity,st$sumDensity_sq)
    densdyn<-data.frame(dyn$sumDensity,dyn$sumDensity_sq)
    names(densst)<-c(paste0(sp,'_sumDensity'),paste0(sp,'_sumDensity_sq'))
    names(densdyn)<-c(paste0(sp,'_sumDensity'),paste0(sp,'_sumDensity_sq'))
    dfst<-cbind(dfst,densst)
    dfdyn<-cbind(dfdyn,densdyn)
  }
}

save(dfst,file=paste0('./output slope/multisp_optimization_static_data_ebsnbs_st.RData'))
save(dfdyn,file=paste0('./output slope/multisp_optimization_static_data_ebsnbs_dyn.RData'))
