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
out_dir<-'C:/Users/Daniel.Vilas/Work/Adapting Monitoring to a Changing Seascape/' #NOAA laptop  
#out_dir<-'/Users/daniel/Work/Adapting Monitoring to a Changing Seascape/' #mac
#out_dir<-'/Users/daniel/Work/VM' #VM
setwd(out_dir)

#list of sp
spp<-list.dirs('./data processed/species/',full.names = FALSE,recursive = FALSE)

#add common name
#selected species
spp<-c('Limanda aspera',
       'Gadus chalcogrammus',
       'Gadus macrocephalus',
       'Atheresthes stomias',
       #'Reinhardtius hippoglossoides',
       'Lepidopsetta polyxystra',
       'Hippoglossoides elassodon',
       'Pleuronectes quadrituberculatus',
       'Hippoglossoides robustus',
       'Boreogadus saida',
       'Eleginus gracilis',
       #'Anoplopoma fimbria',
       'Chionoecetes opilio',
       'Paralithodes platypus',
       'Paralithodes camtschaticus',
       'Chionoecetes bairdi',
       'Atheresthes evermanni')

#remove Anoploma and Reinhardtius because habitat preference reasons
spp<-setdiff(spp, c('Anoplopoma fimbria','Reinhardtius hippoglossoides'))

#common names
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
        'Saffon cod',
        #'Sablefish',
        'Snow crab',
        'Blue king crab',
        'Red king crab',
        'Tanner crab',
        'Kamchatka flounder')

#df sp scientific and common
df_spp<-data.frame('spp'=spp,
                   'common'=spp1)

#create folder simulation data
dir.create(paste0('./output/species/'))

#yrs
yrs<-1982:2022

#how manyt projected years we want
n_proj<-5

#project_yrs
project_yrs<-((yrs[length(yrs)])+1):(yrs[length(yrs)]+n_proj)

###################################
# Download OMs from Drive
###################################

#get files from google drive and set up
files<-googledrive::drive_find()
2 #for dvilasg@uw.edu

#get id shared folder from google drive
id.bering.folder<-files[which(files$name=='Bering redesign RWP project'),'id']
#list of files and folder
files.1<-googledrive::drive_ls(id.bering.folder$id)
id.data<-files.1[which(files.1$name=='manuscripts'),'id']
files.2<-googledrive::drive_ls(id.data$id)
id.data<-files.2[which(files.2$name=='static survey'),'id']
files.3<-googledrive::drive_ls(id.data$id)
id.data<-files.3[which(files.3$name=='OM EBS+NBS'),'id']
files.4<-googledrive::drive_ls(id.data$id)

#get list of fit data
dir.create(paste0('./shelf EBS NBS VAST/'))

###################################
# Grid EBS+NBS
###################################

#load grid of NBS and EBS
 load('./extrapolation grids/northern_bering_sea_grid.rda')
 load('./extrapolation grids/eastern_bering_sea_grid.rda')
 grid<-as.data.frame(rbind(data.frame(northern_bering_sea_grid,region='NBS'),data.frame(eastern_bering_sea_grid,region='EBS')))
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

###################################
# SBT projections
###################################
 
#save SBT table
load('./tables/SBT_projection.RData')#df_sbt
 
#name scenario
df_sbt$sbt<-paste0('SBT',df_sbt$sbt_n)
df_sbt$sbt2<-paste0(df_sbt$sbt,'_',df_sbt$Scenario)
 
#number of simulations
n_sim_hist<- 100
n_sim_proj<- 100

#store index and dens
dens_index_hist_OM<-list()

#array to store simulated densities/CPUE
sim_hist_dens_spp<-array(NA,
                    dim=c(nrow(grid),length(unique(yrs)),n_sim_hist,length(spp)),
                    dimnames=list(1:nrow(grid),unique(yrs),1:n_sim_hist,spp))

#array to store simulated densities/CPUE
sim_proj_dens_spp<-array(NA,
                         dim=c(nrow(grid),length(project_yrs),1,nrow(df_sbt),length(spp)),
                         dimnames=list(1:nrow(grid),project_yrs,1,1:nrow(df_sbt),spp))

######################
# Simulate historical data
######################

#loop over spp
for (sp in spp) {
  
  sp<-spp[15]
  
  #create folder simulation data
  dir.create(paste0('./output/species/',sp,'/'))
  
  #get list of fit data
  ff<-list.files(paste0('./shelf EBS NBS VAST/',sp),'fit',recursive = TRUE)
  
  #load fit file
  load(paste0('./shelf EBS NBS VAST/',sp,'/',ff)) #fit
  #getLoadedDLLs() #if check loaded DLLs
  
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
  df1<-readRDS(paste0('./data processed/species/',sp,'/data_geostat_temp.rds'))
  df2<-subset(df1,year %in% yrs)
  
  #select rows and rename
  df3<-df2[,c("lat_start","lon_start","year",'scientific_name','weight_kg','effort','depth_m','LogDepth',"ScaleLogDepth",'Scalebottom_temp_c','bottom_temp_c','survey_name')]
  colnames(df3)<-c('Lat','Lon','Year','Species','Weight_kg','Swept_area','Depth','LogDepth','ScaleLogDepth','ScaleBotTemp','SBT_insitu','Region')
  
  #data geostat
  df4<-subset(df3,Region %in% c("Eastern Bering Sea Crab/Groundfish Bottom Trawl Survey",
                                "Northern Bering Sea Crab/Groundfish Survey - Eastern Bering Sea Shelf Survey Extension"))
  
  data_geostat<-df4[complete.cases(df4[,c('Weight_kg')]),]
  
  #covariate data - filter by year and complete cases for env variables
  #covariate_data<-subset(df2,Year>=yrs_region[1] & Year<=yrs_region[2])
  covariate_data<-df3[complete.cases(df3[,c('SBT_insitu')]),] #,'ScaleLogDepth'
  
  #add grid to get prediction for simulate data on each cell of the grid (sim$b_i)
  grid_df<-data.frame(Lat=grid_ebs$Lat,
                      Lon=grid_ebs$Lon,
                      Year=grid_ebs$Year,
                      Species=rep(sp,times=nrow(grid_ebs)),
                      Weight_kg=mean(data_geostat$Weight_kg),
                      Swept_area=grid_ebs$Area_in_survey_km2,
                      Depth=grid_ebs$Depth,
                      SBT_insitu=grid_ebs$Temp,
                      Region=grid_ebs$region,
                      stringsAsFactors = T)
  
  #ha to km2
  data_geostat$Effort<-data_geostat$Effort/100
  
  #rbind grid and data_geostat to get prediction into grid values when simulating data
  data_geostat1<-rbind(data_geostat[,c("Lat","Lon","Year","Species","Weight_kg","Swept_area","Depth","SBT_insitu","Region")],
                       grid_df)
  
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
    
    #select simulated data that belong to grid points
    sim_bio <-matrix(data = Sim1$b_i[pred_TF == 1], #kg
                     nrow = nrow(grid),
                     ncol = length(unique(yrs)))
    
    
    #biomass (kg) to CPUE (kg/km2)
    sim_dens[,,isim]<-sim_bio/grid$Area_in_survey_km2
    
  }

  #save data
  save(sim_dens, file = paste0("./output/species/",sp,'/simulated historical data/sim_dens.RData'))

  #store
  sim_hist_dens_spp[,,,sp]<-sim_dens
}

#save 100 simulated historical densities for all species
#save(sim_hist_dens_spp, file = paste0("./output/species/sim_hist_dens_spp.RData"))
#save true densities and index for all species
#save(dens_index_hist_OM, file = paste0("./output/species/dens_index_hist_OM.RData")) 

######################
# Reshape simulated historical data
######################

spp<-spp_conv_ebsnbs


# Initializing parallel backend
cl <- makeCluster(detectCores()-1)  # Using all available cores
registerDoParallel(cl)

#array to store simulated densities/CPUE
sim_dens1 <- array(NA,
                   dim = c(nrow(grid), length(spp), length(unique(yrs)), n_sim_hist),
                   dimnames = list(1:nrow(grid), spp, unique(yrs), 1:n_sim_hist))

#parallel loop over spp
foreach(sp = spp) %do% {
  
  sp<-spp_conv_ebsnbs[17]
  
  #load data
  load(paste0('./output/species/', sp, '/simulated historical data/sim_dens.RData'))
  
  #parallel loop over years and simulations
  foreach(y = yrs) %:%
    foreach(sim = 1:n_sim_hist) %do% {
      #y<-'1982';sim<-'1'
      
      #store results
      sim_dens1[, sp, as.character(y), as.character(sim)] <- sim_dens[, as.character(y), as.character(sim)]
    }
}

# Stopping the parallel backend
stopCluster(cl)

#store HIST simulated data
save(sim_dens1, file = paste0('./output/species/ms_sim_dens.RData'))  


######################
# Simulate projected data
######################

#get raster stack
stack_files<-list.files('./data processed/SBT projections/')
 
#loop over spp
for (sp in spp) {
  
  #sp<-spp[14]
  
  #create folder simulation data
  dir.create(paste0('./output/species/',sp,'/'))
  
  #get list of fit data
  ff<-list.files(paste0('./shelf EBS NBS VAST/',sp),'fit',recursive = TRUE)
  
  #load fit file
  load(paste0('./shelf EBS NBS VAST/',sp,'/',ff)) #fit 
  #create folder simulation data
  dir.create(paste0('./output/species/',sp,'/simulated projected data/'))
  
  #loop over scenarios
  for (sbt in unique(df_sbt$sbt_n)) {
    
    #sbt<-unique(df_sbt$sbt_n)[8]
    
    #print scenario to check progress
    cat(paste(" #############     PROJECTING    #############\n",
              " #############   Species", sp, match(sp,spp), 'out of',length(spp),  "  #############\n",
              " #############  SBT", sbt, " #############\n"))
    
    #open stack of rasters
    st<-stack_files[grepl(paste0('SBT_',sbt),stack_files)][1]
    st<-stack(paste0('./data processed/SBT projections/',st))

    #raster to points
    points<-data.frame(rasterToPoints(st))
    
    #create a df to store
    points3<-data.frame(matrix(nrow = 0,ncol = ncol(fit$covariate_data)))
    names(points3)<-names(fit$covariate_data)
    
    for (y in project_yrs) {
      
      #y<-project_yrs[1]
      
      #get points for year
      points1<-points[,c('x','y',paste0('y',y))]
      names(points1)<-c('Lon',"Lat",'BotTemp')
      
      #reproject df#
      coordinates(points1)<- ~ Lon + Lat
      proj4string(points1) <- CRS('+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') 
      points1<-data.frame(spTransform(points1,CRSobj = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")))
      names(points1)<-c('BotTemp','Lon',"Lat")
      
      #create a new df
      points2<-data.frame(cbind(Year=y,Lat=points1$Lat,Lon=points1$Lon,ScaleLogDepth=NA,LogDepth=NA,ScaleBotTemp=NA,BotTemp=points1$BotTemp,CPUE_kg=NA))
      
      #add year
      points3<-rbind(points3,points2)
      
    }
    
    #add covariate data
    new_data<-rbind(fit$covariate_data,points3)
    
    #to avoid crashes projections are done in two groups of 50 instead of 1 of 100
    
    
    #to store simulated data
    dens_index_proj_OM<-list()
    
    #project model example
     pm<-VAST::project_model(x = fit,
                             working_dir = paste0('./shelf EBS NBS VAST/',sp,'/'),
                             n_proj = n_proj,
                             n_samples = n_sim_proj/2, #n_sim_proj=100
                             new_covariate_data = new_data,
                            historical_uncertainty = 'none')
    
     #get simulated projected densities and index
     for (iproj in 1:(n_sim_proj/2)) {
       
       #iproj<-1
       
       index<-pm[[iproj]]$Index_ctl
       dens<-pm[[iproj]]$D_gct[,1,]
       dens_index_proj_OM[[paste0('sim',iproj)]]<-list('index'=index,'dens'=dens)
     }
     
     #remove projections
     rm(pm)
     
     #save true densities and index under 8 SBT scenarios for all species
     save(dens_index_proj_OM, file = paste0("./output/species/",sp,"/simulated projected data/SBT",sbt," dens_index_proj_OM_50.RData")) 
     
     #to store simulated data
     dens_index_proj_OM<-list()
     
     #project model example
     pm<-VAST::project_model(x = fit,
                             working_dir = paste0('./shelf EBS NBS VAST/',sp,'/'),
                             n_proj = n_proj,
                             n_samples = n_sim_proj/2, #n_sim_proj?
                             new_covariate_data = new_data,
                             historical_uncertainty = 'none')
     
     #get simulated projected densities and index
     for (iproj in 1:(n_sim_proj/2)) {
       
       #iproj<-1
       
       index<-pm[[iproj]]$Index_ctl
       dens<-pm[[iproj]]$D_gct[,1,]
       dens_index_proj_OM[[paste0('sim',iproj+50)]]<-list('index'=index,'dens'=dens)
     }
     
     #save true densities and index under 8 SBT scenarios for all species
     save(dens_index_proj_OM, file = paste0("./output/species/",sp,"/simulated projected data/SBT",sbt," dens_index_proj_OM_100.RData")) 
     
    rm(pm)
    
    gc()
  }
}

######################
# Reshape simulated projected data
######################

#loop over 8 temperature scenarios
for (sbt in unique(df_sbt$sbt_n)) {
  
  simdata<-array(NA,
                 dim = c(nrow(grid), length(spp), length(unique(2023:2027)), n_sim_proj),
                 dimnames = list(1:nrow(grid),spp, as.character(2023:2027), 1:n_sim_proj))
  
  proj_ind<-array(NA,
                  dim = c(length(spp), length(unique(2023:2027)), n_sim_proj),
                  dimnames = list(spp, as.character(2023:2027), 1:n_sim_proj))
  
  #loop over species
  for (sp in spp) {
    
    #sp<-spp[1];sbt<-df_sbt$sbt_n[1]
    
    #load first 50 simulations
    load(file = paste0("./output/species/",sp,"/simulated projected data/SBT",sbt," dens_index_proj_OM_50.RData")) #dens_index_proj_OM
    #load('/Users/Daniel.Vilas/Downloads/SBT1 dens_index_proj_OM_50.RData') #dens_index_proj_OM
    dens_index_proj_OM_50<-dens_index_proj_OM
    rm(dens_index_proj_OM)
    #load following 50 simulations
    load(file = paste0("./output/species/",sp,"/simulated projected data/SBT",sbt," dens_index_proj_OM_100.RData")) #dens_index_proj_OM
    #load('/Users/Daniel.Vilas/Downloads/SBT1 dens_index_proj_OM_100.RData') #dens_index_proj_OM
    dens_index_proj_OM_100<-dens_index_proj_OM
    rm(dens_index_proj_OM)
    
    for (i in 1:50) {
      
      cat(paste(" #############  sp",sp,'- sbt',sbt ,'- sim',i," #############\n"))
      
      for (y in as.character(2023:2027)) {
        
        #y<-as.character(2023:2027)[1]
        
        #densities
        simdata[,sp,y,i]<-dens_index_proj_OM_50[[i]]$dens[,as.character(2023:2027)][,y]
        simdata[,sp,y,i+50]<-dens_index_proj_OM_100[[i]]$dens[,as.character(2023:2027)][,y]
        #index
        proj_ind[sp,,i]<-dens_index_proj_OM_50[[i]]$index[,as.character(2023:2027),'Stratum_1']
        proj_ind[sp,,i+50]<-dens_index_proj_OM_100[[i]]$index[,as.character(2023:2027),'Stratum_1']
        
      }
    }
  }
  
  #store PROJ simulated data
  save(simdata, file = paste0("./output/species/SBT",sbt," ms_sim_proj_dens.RData"))  
  save(proj_ind, file = paste0("./output/species/SBT",sbt," ms_sim_proj_ind.RData"))  
  
}
