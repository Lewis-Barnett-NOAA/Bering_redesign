####################################################################
####################################################################
##    
##    Simulate data from OM for historical and projected years
##    Reshape output
##    Daniel Vilas (danielvilasgonzalez@gmail.com/dvilasg@uw.edu/daniel.vilas@noaa.gov)
##    Lewis Barnett, Zack Oyafuso, Megsie Siple
##
##    danielvilasgonzalez@gmail.com/dvilasg@uw.edu
##
####################################################################
####################################################################

#clear all objects
rm(list = ls(all.names = TRUE)) 

#free up memrory and report the memory usage
gc() 

#libraries from cran to call or install/load
pack_cran<-c('ggplot2','units','splines','raster','sp','foreach','doParallel')

#install pacman to use p_load function - call library and if not installed, then install
if (!('pacman' %in% installed.packages())) {
  install.packages("pacman")}

#install VAST if it is not
if (!('VAST' %in% installed.packages())) {
  devtools::install_github("james-thorson/VAST@main", INSTALL_opts="--no-staged-install")};library(VAST)

#load/install packages
pacman::p_load(pack_cran,character.only = TRUE)

#setwd - depends on computer using
out_dir<-'/Users/daniel/Work/Adapting Monitoring to a Changing Seascape/' #mac
setwd(out_dir)

#list of sp
spp<-list.dirs('./data processed/species/',full.names = FALSE,recursive = FALSE)

#add common name
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

#selected species

#read coinvergence and st slope
df_conv<-read.csv('./tables/slope_conv.csv')
slp_conv<-df_conv[which(df_conv$slope_st=='convergence'),'spp']
ebsnbs_conv<-df_conv[which(df_conv$EBS_NBS=='convergence'),'spp']

#create folder simulation data
dir.create(paste0('./output slope//species/'))

###################################
# Grid EBS+NBS
###################################

 #load grid of NBS and EBS
 load('./extrapolation grids/bering_sea_slope_grid.rda')
 grid<-as.data.frame(rbind(data.frame(bering_sea_slope_grid,region='SLP')))
 grid$cell<-1:nrow(grid)
 
 #load grid
 load('./data processed/grid_EBS_NBS.RData')
 yrs<-1982:2022
 grid_ebs<-grid.ebs_year[which(grid.ebs_year$region != 'EBSslope' & grid.ebs_year$Year %in% yrs),]
 dim(grid_ebs)
 
 #load baseline strata and specify corner stations
 load('./output/baseline_strata.RData')
 baseline_strata$locations[grep('GF|HG|JI|IH|ON|QP|PO',baseline_strata$locations$stationid),]
 baseline_strata$locations$corner<-ifelse(grepl('GF|HG|JI|IH|ON|QP|PO',baseline_strata$locations$stationid),'TRUE','FALSE')
 
#  
# #number of simulations
n_sim_hist<- 100
# n_sim_proj<- 100
# 
# #store index and dens
dens_index_hist_OM<-list()
# 
# #array to store simulated densities/CPUE
 sim_hist_dens_spp<-array(NA,
                     dim=c(nrow(grid),length(unique(yrs)),n_sim_hist,length(spp)),
                    dimnames=list(1:nrow(grid),unique(yrs),1:n_sim_hist,spp))
# 
# #array to store simulated densities/CPUE
# sim_proj_dens_spp<-array(NA,
#                          dim=c(nrow(grid),length(project_yrs),1,nrow(df_sbt),length(spp)),
#                          dimnames=list(1:nrow(grid),project_yrs,1,1:nrow(df_sbt),spp))

yrs<-2002:2016

#save 100 simulated historical densities for all species
#save(sim_hist_dens_spp, file = paste0("./output/species/sim_hist_dens_spp.RData"))
#save true densities and index for all species
#save(dens_index_hist_OM, file = paste0("./output/species/dens_index_hist_OM.RData")) 

######################
# Simulate historic data for EBS and NBS for new species
######################

#grid
load('./extrapolation grids/northern_bering_sea_grid.rda')
load('./extrapolation grids/eastern_bering_sea_grid.rda')
grid<-as.data.frame(rbind(data.frame(northern_bering_sea_grid,region='NBS'),data.frame(eastern_bering_sea_grid,region='EBS')))
grid$cell<-1:nrow(grid)

#yrs
yrs<-c(1982:2022)

# #array to store simulated densities/CPUE
sim_hist_dens_spp<-array(NA,
                         dim=c(nrow(grid),length(unique(yrs)),n_sim_hist,length(spp)),
                         dimnames=list(1:nrow(grid),unique(yrs),1:n_sim_hist,spp))
######################
# Simulate historical data
######################

#loop over spp that were not done in the first manuscript
for (sp in spp[c(17,18,21,23)]) {
  
  #sp<-spp[1] #20
  
  # if (sp %in% c('Atheresthes stomias','Atheresthes evermanni')) {
  #   yrs<-1991:2022
  # } else {
  #   yrs<-1982:2022
  # }
  # 
    
  mod1<-'fit.RData'
  
  #create folder simulation data
  dir.create(paste0('./output/species/',sp,'/'))
  
  #get list of fit data
  ff<-list.files(paste0('./shelf EBS NBS VAST/',sp),mod1,recursive = TRUE)
  
  if (length(ff)==0) {
    next
  } else {
    
  }
  
  #load fit file
  load(paste0('./shelf EBS NBS VAST/',sp,'/',ff)) #fit
  #getLoadedDLLs() #if check loaded DLLs
  #check_fit(fit$parameter_estimates)
  
  ##reload model
  fit<-
    reload_model(x = fit)
  
  #store index and dens
  index<-fit$Report$Index_ctl
  dens<-fit$Report$D_gct
  dens_index_hist_OM[[sp]]<-list('index'=index,'dens'=dens)
  
  #check observation and predicted densities at each obs
  #observations
  # file<-files.5[grep('data_geostat',files.5$name),]
  # 
  # #download file
  # googledrive::drive_download(file=file$id,
  #                             path = paste0('./shelf EBS NBS VAST/',sp,'/data_geostat_temp.rds'),
  #                             overwrite = TRUE)
  # 
  # data_geostat<-readRDS(paste0('./shelf EBS NBS VAST/',sp,'/data_geostat_temp.rds'))
  
  #predictions
  #d_i<-fit$Report$D_i #nrow(fit$data_frame)
  #length(d_i)==nrow(data_geostat)
  
  #################
  # get predTF (required argument to get predictions on grid when simulating data)
  #################
  
  #read data_geostat_temp file
  #load(paste0('./slope EBS VAST/',sp,'/data_geostat_temp.RData'))
  data_geostat1<-readRDS(paste0('./shelf EBS NBS VAST/',sp,'/data_geostat_temp.rds'))
  
  #data_geostat<-data_geostat1[which(data_geostat1$Region %in% c("slope")),]
  
  #rbind grid and data_geostat to get prediction into grid values when simulating data
  data_geostat<-data_geostat1[which(data_geostat1$Region %in% c("Eastern Bering Sea Crab/Groundfish Bottom Trawl Survey",
                                                                 "Northern Bering Sea Crab/Groundfish Survey - Eastern Bering Sea Shelf Survey Extension")),]

  #to get predictions in locations but not influencing fit
  pred_TF <- rep(1, nrow(data_geostat1))
  pred_TF[1:nrow(data_geostat)] <- 0
  
  #array to store simulated densities/CPUE
  sim_dens<-array(NA,
                  dim=c(nrow(grid),length(unique(yrs)),n_sim_hist),
                  dimnames=list(1:nrow(grid),unique(yrs),1:n_sim_hist))
  
  #create folder simulation data
  dir.create(paste0('./output/species/',sp,'/simulated historical data/'))
  
  for (isim in 1:n_sim_hist) { #simulations
    
    #isim<-1
    
    #print simulation to check progress
    cat(paste(" #############   Species", sp, match(sp,spp), 'out of',length(spp),  "  #############\n",
              " #############  historical simulation", isim, "of",n_sim_hist, " #############\n"))
    
    #simulate data from OM
    Sim1 <- FishStatsUtils::simulate_data(fit = fit, #kg/km2
                                          type = 1,
                                          random_seed = isim)
    
    # if (sp=='Atheresthes evermanni') {
    #   #select simulated data that belong to grid points
    #   sim_bio <-matrix(data = Sim1$b_i[pred_TF == 1], #kg
    #                    nrow = nrow(grid),
    #                    ncol = length(c(1991:2019,2021:2022)))
    #   
    #   sim_bio <-
    #       cbind(matrix(NA,nrow = nrow(grid),ncol=length(1982:1990)),
    #       sim_bio[,c(1:29)],
    #       matrix(NA,nrow = nrow(grid),ncol=length(2020)),
    #       sim_bio[,c(30:31)])
    #   
    # } else if (sp=='Atheresthes stomias') {
    #   sim_bio <-matrix(data = Sim1$b_i[pred_TF == 1], #kg
    #                    nrow = nrow(grid),
    #                    ncol = length(c(1982:2022)))
    #   sim_bio[,c(1:9,39)]<-NA
    # } else{
    sim_bio <-matrix(data = Sim1$b_i[pred_TF == 1], #kg
                     nrow = nrow(grid),
                     ncol = length(yrs))
    
    #sim_bio[,c(39)]<-NA
    #}
    
    #biomass (kg) to CPUE (kg/km2)
    sim_dens[,,isim]<-sim_bio/grid$Area_in_survey_km2
    
  }
  
  #save data
  save(sim_dens, file = paste0("./output/species/",sp,'/simulated historical data/sim_dens.RData'))
  
  #store
  sim_hist_dens_spp[,,,sp]<-sim_dens
}

######################
# Reshape simulated historical data
######################

# #selected species not previously in the EBS - NBS
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

# Initializing parallel backend
cl <- makeCluster(1)
registerDoParallel(cl)

#n_sim
n_sim<-100

#array to store simulated densities/CPUE
sim_dens1 <- array(NA,
                   dim = c(nrow(grid), length(spp), length(unique(yrs)), n_sim),
                   dimnames = list(1:nrow(grid), spp, unique(yrs), 1:n_sim))

#parallel loop over spp
foreach(sp = ebsnbs_conv) %do% {
  
  #sp<-spp[1]
  
  #load data
  load(paste0('./output/species/', sp, '/simulated historical data/sim_dens.RData'))
  
  #parallel loop over years and simulations
  foreach(y = yrs) %:%
    foreach(sim = 1:n_sim) %do% {
      #y<-'1982';sim<-'1'
      
      #store results
      sim_dens1[, sp, as.character(y), as.character(sim)] <- sim_dens[, as.character(y), as.character(sim)]
    }
}

# Stopping the parallel backend
stopCluster(cl)

#store HIST simulated data
save(sim_dens1, file = paste0('./output slope//species/ms_sim_dens.RData'))  
#load(file = paste0('./output slope//species/ms_sim_dens.RData'))  

########################################################################################
# JOIN EBS+NBS and SLP ARRAY
########################################################################################

#sim_dens1
load(file = paste0('./output slope//species/ms_sim_dens.RData'))  
ebsnbs_simdens<-sim_dens1

#sim_dens1
load(file = paste0('./output slope//species/ms_sim_dens_slope.RData'))  
slp_simdens<-sim_dens1
dimnames(slp_simdens)

# Replace NAs with 0 in the 4D array for species that are not present in the region
ebsnbs_simdens[is.na(ebsnbs_simdens)] <- 0
slp_simdens[is.na(slp_simdens)] <- 0

#selected species not previously in the EBS - NBS
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

#n_sim
n_sim<-100

#array to store simulated densities/CPUE
sim_dens1 <- array(NA,
                   dim = c(56505, length(spp), length(1982:2022), n_sim),
                   dimnames = list(1:56505, spp, 1982:2022, 1:n_sim))

for (sp in spp) {
  for (y in 1982:2022) {
    
    cat(paste(sp,y,'\n'))
    
    
    for (sim in 1:n_sim) {
      
      
      if (y %in% c(2002:2016)) {
        #store results
        sim_dens1[, sp, as.character(y), as.character(sim)] <-c(ebsnbs_simdens[,sp,as.character(y), as.character(sim)],
                                                                    slp_simdens[,sp,as.character(y), as.character(sim)])
      } else {
        #store results
        sim_dens1[, sp, as.character(y), as.character(sim)] <-c(ebsnbs_simdens[,sp,as.character(y), as.character(sim)],
                                                                rep(NA,3041))}
    }
  }
}

#store HIST simulated data
save(sim_dens1, file = paste0('./output slope//species/ms_sim_dens_all.RData'))  

#checks
dimnames(sim_dens1)
sim_dens1[53465:53470,'Gadus macrocephalus', '2000', '1'] #should be NA
sim_dens1[1:20,'Sebastolobus alascanus', '2000', '1'] #should be 0
sim_dens1[53465:53470,'Gadus macrocephalus', '2002', '1'] #should be positive
sim_dens1[53465:53470,'Gadus macrocephalus', '2003', '1'] #should be positive
