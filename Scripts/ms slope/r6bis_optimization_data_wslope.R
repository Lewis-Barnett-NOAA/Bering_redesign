####################################################################
####################################################################
##
##    Get true index from the OM, prepare data for optimization
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

#classify cold and warm years
#sel years (2002:2016)
cyrs<-c(2006:2013)
wyrs<-c(2002:2005,2014:2016)
n_yrs<-length(c(cyrs,wyrs))
yrs<-sort(c(cyrs,wyrs))

#####################
# check the slope model that converged
#####################

df_conv<-read.csv('./tables/slope_conv.csv')

spp_conv_ebsnbs<-c(
      df_conv[which(df_conv$EBS_NBS=='convergence'),'spp'])

spp_conv_slope<-c(
  df_conv[which(df_conv$slope_st=='convergence'),'spp'])

spp_sim_dens_slope<-sub("\\/.*", "", list.files('./output/species/',pattern = 'sim_dens_slope',recursive = TRUE))
spp_sim_dens_slope<-spp_sim_dens_slope[!grepl('sim_dens',spp_sim_dens_slope)]
spp_sim_dens_ebsnbs<-sub("\\/.*", "", list.files('./output/species/',pattern = 'sim_dens.RData',recursive = TRUE))
spp_sim_dens_ebsnbs<-spp_sim_dens_ebsnbs[!grepl('sim_dens',spp_sim_dens_ebsnbs)]

length(spp_conv_ebsnbs);length(spp_sim_dens_ebsnbs)
length(spp_conv_slope);length(spp_sim_dens_slope)


###################################
# LOAD GRID EBS (remember to keep the same order as in fit_model if multiple grids)
###################################

#load slope grid
load('./extrapolation grids/bering_sea_slope_grid.rda')
dim(bering_sea_slope_grid)
names(bering_sea_slope_grid)[4]<-'Stratum'
bering_sea_slope_grid$Stratum<-999
#gridslope<-data.frame(bering_sea_slope_grid,region='SLP')

#load EBS+NBS grid
load('./extrapolation grids/northern_bering_sea_grid.rda')
load('./extrapolation grids/eastern_bering_sea_grid.rda')
grid<-as.data.frame(rbind(data.frame(northern_bering_sea_grid,region='NBS'),
                          data.frame(eastern_bering_sea_grid,region='EBS'),
                          data.frame(bering_sea_slope_grid,region='SLP')))
grid$cell<-1:nrow(grid)
grid$cell<-as.numeric(grid$cell)

gridslope<-grid[which(grid$region=='SLP'),]

ggplot()+
  geom_point(data=grid,aes(x=Lon,y=Lat,col=cell))

#load grid all years
load(file = './data processed/grid_EBS_NBS.RData')

#yrs slope
yrs_slope<-c(2002,2004,2008,2010,2012,2016)

#load grid
load('./data processed/grid_EBS_NBS.RData')

#remove grids deeper than XXX because they cannot be sampled
grid.ebs_year1<-grid.ebs_year[which(grid.ebs_year$Year %in% yrs_slope & grid.ebs_year$region =='EBSslope'),]
hist(unique(grid.ebs_year1$depth_m))

gridslope ##########
#400m

grid.ebs_year2<-grid.ebs_year1[which(grid.ebs_year1$Year==yrs_slope[1]),]
nrow(grid.ebs_year2)
nrow(grid.ebs_year2[which(grid.ebs_year2$depth_m > 1000 ),])
nrow(grid.ebs_year2[which(grid.ebs_year2$depth_m <= 400 ),])
grid.ebs_year2[which(grid.ebs_year2$depth_m <= 400  ),]


ggplot()+
  geom_point(data=grid.ebs_year2[which(grid.ebs_year2$depth_m <= 400  ),],aes(x=Lon,y=Lat,col=depth_m))+
  scale_fill_continuous(limits=c(0,2600),high = "#132B43", low = "#56B1F7")

ggplot()+
  geom_point(data=grid.ebs_year2,aes(x=Lon,y=Lat,col=depth_m))+
  scale_color_continuous(limits=c(0,2600),high = "#132B43", low = "#56B1F7")




#converged species by region
spp_conv_ebsnbs<-df_conv[which(df_conv$EBS_NBS=='convergence'),'spp']
spp_conv_slope<-df_conv[which(df_conv$slope_st=='convergence'),'spp']

#years by region
yrs_slope<-c(2002,2004,2008,2010,2012,2016)
#yrs<-c(1982:2019,2021:2022)

#build array for temporal array to store results
temp_dens_vals <- array(NA,
                        dim = c(nrow(gridslope),
                                length(yrs),
                                length(spp)),
                        dimnames = list(1:nrow(gridslope),yrs,spp))

#array to store indices
true_index<-array(NA,
                  dim = list(length(yrs),1,length(spp)),
                  dimnames = list(yrs,'slope',spp))


# ###################################
# # LOOP OVER SPECIES
# ###################################

#loop over species
for (sp in spp) {
  
  #sp<-spp[2]
  #sp<-'Anoplopoma fimbria'
  cat(paste('#######################',sp,'#######################\n'))
  
  dir.create(paste0('./output/species/',sp),showWarnings = FALSE)
  dir.create(paste0('./output/species/',sp,'/optimization data/'),showWarnings = FALSE)
  
  if (sp %in% spp_conv_slope) {
    
    
    #load model  
    load(paste0('./slope EBS VAST/',sp,'/fit_st.RData'))

    #get predicted densities for sp
    temp_dens_vals[,,sp] <- unlist(fit$Report$D_gct[, 1, as.character(yrs)]) #[kg/km2]
    
    #density_input<-temp_dens_vals
    D_gt<-unlist(fit$Report$D_gct[, 1, as.character(yrs)])
    
    
    #get true index for NBS_EBS, NBS and EBS
    true_index[,,sp]<-fit$Report$Index_ctl[1,paste0(yrs),1 ]
    
    #get map info
    mdl <- make_map_info(Region = fit$settings$Region, 
                         spatial_list = fit$spatial_list,
                         Extrapolation_List = fit$extrapolation_list)
    mdl$PlotDF$x2i<-mdl$PlotDF$x2i+53464
    
  } else {
    
    D_gt<-data.frame(matrix(0,nrow = nrow(gridslope),ncol = length(yrs)))
    names(D_gt)<-as.character(yrs)
    
    #get true index for NBS_EBS, NBS and EBS
    true_index[,,sp]<-0
    
    #load slope fit
    load(paste0('./slope EBS VAST/',spp_conv_slope[1],'/fit_st.RData'))
    
    #get map info
    mdl <- make_map_info(Region = fit$settings$Region, 
                         spatial_list = fit$spatial_list,
                         Extrapolation_List = fit$extrapolation_list)
    mdl$PlotDF$x2i<-mdl$PlotDF$x2i+53464

  }  
  
  #nrow(gridslope)
  
  #dataframe of cells with predictions
  D_gt<-data.frame('cell'=c(min(gridslope$cell):max(gridslope$cell)),D_gt)
  
  #years simulated
  #yrs<-as.numeric(yrs)
  
  #rename years predictions 
  colnames(D_gt)<-c('cell',yrs) #,project_yrs
  
  #reshape
  D_gt1<-reshape2::melt(D_gt,id=c('cell'))
  
  #merge cells and predictions
  D_gt2 <- merge(D_gt1, gridslope, by=c('cell'))
  
  #merge cells and predictions
  D <- merge(D_gt2, mdl$PlotDF, by.x=c('cell','Lat','Lon'), by.y=c('x2i','Lat','Lon'))
  
  #rename columns
  colnames(D)<-c('cell','Lat','Lon','Year','Density','Area','Stratum','region','Include')
  
  #create biomass
  D$Biomass<-D$Density*D$Area
  
  if (sp %in% spp_conv_slope) {
    D$Biomass<-drop_units(D$Biomass)
  }
  
  #get grid data for yrs in the simulation
  grid.ebs_year1<-grid.ebs_year[which(grid.ebs_year$Year %in% yrs & grid.ebs_year$region =='EBSslope'),]
  grid.ebs_year1<-grid.ebs_year1[,c("Lat","Lon","Area_in_survey_km2","DepthGEBCO","Temp","Year")]
  names(grid.ebs_year1)[4]<-'Depth'
  
  #merge grid data and predictions
  D1<-merge(D,grid.ebs_year1,by=c('Lat','Lon','Year'))
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
  #D6[which(D6$Depth<=400),]
  
  #only one species
  tdf<-temp_dens_vals[,,sp]
  
  #list OM true CPUE and true index
  CPUE_index<-list('CPUE'=D1,
                   'true_index'=true_index[,,sp])
  
  #create a list of static df (static and dynamic)
  input_optim<-list('dynamic'=D6dyn,
                    'static'=D6)
  
  #save results list
  save(input_optim,file=paste0('./output/species/',sp,'/optimization data/optimization_static_data_slope.RData'))
  
  #save results list
  save(CPUE_index,file=paste0('./output/species/',sp,'/optimization data/OM_CPUE_index_slope.RData'))
  
  #save results list
  save(tdf,file=paste0('./output/species/',sp,'/optimization data/fit_temporal_data_slope.RData'))
  
}


#join optimmization data into a one single 
for (sp in spp) {
  
  #sp<-spp[23]
  
  if (sp==spp[1]) {
    load(paste0('./output/species/',sp,'/optimization data/optimization_static_data_slope.RData'))
    st<-input_optim[['static']]
    dyn<-input_optim[['dynamic']]
    dfst<-st[,c("Lat","Lon","cell","Depth","meanTemp","varTemp","include","meanTempF","LonE")]  
    dfdyn<-dyn[,c("Lat","Lon","cell","Depth","meanTemp","varTemp","include","meanTempF","LonE",'regime')]  
  }
  
  load(paste0('./output/species/',sp,'/optimization data/optimization_static_data_slope.RData'))
  st<-input_optim[['static']]
  dyn<-input_optim[['dynamic']]
  densst<-data.frame(st$sumDensity,st$sumDensity_sq)
  densdyn<-data.frame(dyn$sumDensity,dyn$sumDensity_sq)
  names(densst)<-c(paste0(sp,'_sumDensity'),paste0(sp,'_sumDensity_sq'))
  names(densdyn)<-c(paste0(sp,'_sumDensity'),paste0(sp,'_sumDensity_sq'))
  dfst<-cbind(dfst,densst)
  dfdyn<-cbind(dfdyn,densdyn)
}

save(dfst,file=paste0('./output slope/multisp_optimization_static_data_slope_st.RData'))
save(dfdyn,file=paste0('./output slope/multisp_optimization_static_data_slope_dyn.RData'))
#load(file=paste0('./output slope/multisp_optimization_static_data_slope_dyn.RData'))
#load(file=paste0('./output/multisp_optimization_static_data_slope.RData'))


######################
# COMBINE DATA FROM SLOPE AND EBS_NBS FOR OPTIMIZATION
#####################

#DYNAMIC
load(paste0('./output slope/multisp_optimization_static_data_ebsnbs_dyn.RData'))
head(dfdyn)
dim(dfdyn)
df_ebs_nbs<-dfdyn
df_ebs_nbs<-df_ebs_nbs[order(df_ebs_nbs$cell),]
load(paste0('./output slope/multisp_optimization_static_data_slope_dyn.RData'))
head(dfdyn)
dim(dfdyn)
df_slope<-dfdyn
df_slope<-df_slope[order(df_slope$cell),]

df<-rbind(df_ebs_nbs,df_slope)
save(df,file=paste0('./output slope/multisp_optimization_static_data_ebsnbs_slope_dyn.RData'))
load(paste0('./output slope/multisp_optimization_static_data_ebsnbs_slope_dyn.RData'))

#STATIC
load(paste0('./output slope/multisp_optimization_static_data_ebsnbs_st.RData'))
head(dfst)
dim(dfst)
df_ebs_nbs<-dfst
df_ebs_nbs<-df_ebs_nbs[order(df_ebs_nbs$cell),]
load(paste0('./output slope/multisp_optimization_static_data_slope_st.RData'))
head(dfst)
dim(dfst)
df_slope<-dfst
df_slope<-df_slope[order(df_slope$cell),]
df<-rbind(df_ebs_nbs,df_slope)
save(df,file=paste0('./output slope/multisp_optimization_static_data_ebsnbs_slope_st.RData'))
load(paste0('./output slope/multisp_optimization_static_data_ebsnbs_slope_st.RData'))

