###########################
# sensitivity analysis
###########################

#clear all objects
rm(list = ls(all.names = TRUE)) 
#free up memrory and report the memory usage
gc() 

#libraries from cran to call or install/load
pack_cran<-c('ncdf4','raster','FNN','lubridate','ggpubr',"splines",'ggplot2','dplyr','doParallel')

#install pacman to use p_load function - call library and if not installed, then install
if (!('pacman' %in% installed.packages())) {
  install.packages("pacman")}

#load/install packages
pacman::p_load(pack_cran,character.only = TRUE)

#install coldpool to extract SBT for the EBS
if (!('VAST' %in% installed.packages())) {
  devtools::install_github("james-thorson/VAST@main", INSTALL_opts="--no-staged-install")};library(VAST)

#setwd - depends on computer using
#out_dir<-'C:/Users/Daniel.Vilas/Work/Adapting Monitoring to a Changing Seascape/' #NOAA laptop  
# out_dir<-'/Users/daniel/Work/Adapting Monitoring to a Changing Seascape/' #mac
# setwd(out_dir)

#version VAST (cpp)
version<-'VAST_v14_0_1'

#number of knots
knots<-'300' #200

#range years of data
# sta_y<-1982
# end_y<-2022

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

#read catchability data
#s12 Selectivity ratio > 1 means the slope gear and protocol had higher selectivity
#so, we need to divide slope data by the sr index
#471 is for Alaska skate - while we are using Aleutian skate 472
#data_sratio<-readRDS('data/data_raw/shelf_slope_sratio_bootstrap.rds')
data_sratio<-readRDS('data/data_raw/shelf_slope_sratio_bootstrap.rds')
unique(data_sratio$SPECIES_CODE)

#convert SR of one speccies into another
data_sratio[which(data_sratio$SPECIES_CODE=='471'),'SPECIES_CODE']<-'472'

#read length raw data
#data_length<-readRDS('data/data_raw/ak_bts_ebs_nbs_slope.rds') #data_length
data_length<-readRDS('data/data_raw/ak_bts_ebs_nbs_slope.rds') #data_length
head(data_length)
head(data_length$specimen)
dim(data_length$specimen)
head(data_length$catch)

unique(data_length$size$SPECIES_CODE)
head(data_length$specimen)

#get cruisejoin for the ebs
head(data_length$cruise)
cruisejoin_ebs<-subset(data_length$cruise,SURVEY=='EBS')[,'CRUISEJOIN']

#get hauls for the ebs
hauls_ebs<-subset(data_length$haul,CRUISEJOIN %in% cruisejoin_ebs)

#convert time
time_axis <- as.POSIXct(hauls_ebs$START_TIME, origin = "1900-01-01", tz = "GMT") 
hauls_ebs$YEAR <- format(time_axis, "%Y")

#check hauls number per year in ebs
aggregate(HAULJOIN ~ YEAR,hauls_ebs,FUN=length)

#code species
spp_code<-unique(data_length$species[,c('SPECIES_CODE',"REPORT_NAME_SCIENTIFIC")])
names(spp_code)<-c('species_code',"scientific_name")
spp_code1<-spp_code[which(spp_code$scientific_name %in% 
                            spp),]
#add Alaska skate
spp_code1<-rbind(spp_code1,c(472,'Bathyraja aleutica'))

#merge sr data to species
data_sratio<-merge(data_sratio,spp_code1,by.x='SPECIES_CODE',by.y='species_code')
#1000 samples per size and species combination
aggregate(SPECIES_CODE ~ SIZE_BIN +scientific_name,data_sratio,FUN=length)

#get mean
data_sratio<-subset(data_sratio,s12<100)
data_sratio1<-aggregate(s12 ~ SIZE_BIN +scientific_name,data_sratio,FUN=mean)

#list for store results
ind_list<-list()

for (sp in unique(data_sratio1$scientific_name)) {
  
  sp<-unique(data_sratio1$scientific_name)[1]
  
  cat(paste('################',sp,'####################\n'))

  #apply SR to data_geostat
  data_geostat1<-readRDS(paste0('data/data_processed/species/',sp,'/data_geostat_slope_adj.rds'))
  #data_geostat1<-subset(data_geostat,survey_name=='Eastern Bering Sea Slope Bottom Trawl Survey')

  # #convert grams to kg/ha
  # data_geostat1$ADJ_KG_HA10<-data_geostat1$cpue_kgha*sr10
  # data_geostat1$ADJ_KG_HA90<-data_geostat1$cpue_kgha*sr90
  # 
  # #if bathyraja because of adjustments
  # if (sp=='Bathyraja aleutica') {
  #   data_geostat1$ADJ_KG_HA10<-data_geostat1$ADJ_KG_HA10/1000
  #   data_geostat1$ADJ_KG_HA90<-data_geostat1$ADJ_KG_HA90/1000
  # }
  
  #create df
  df_ind<-data.frame('yrs'=2002:2016)
  df_ind$species<-sp
  
  for (s in c('s10','s90')) {
    
    #s<-'s10'
    
    if (s=='s10') {
      data_geostat2<-data_geostat1[,c("lat_start","lon_start","year",'scientific_name','ADJ_KG_HA10','effort','depth_m','survey_name')]
      colnames(data_geostat2)<-c('Lat','Lon','Year','Species','Cpue_kgha','Effort','Depth','Region')
    } else {
      data_geostat2<-data_geostat1[,c("lat_start","lon_start","year",'scientific_name','ADJ_KG_HA90','effort','depth_m','survey_name')]
      colnames(data_geostat2)<-c('Lat','Lon','Year','Species','Cpue_kgha','Effort','Depth','Region')
    }
    
    #get weight
    data_geostat2$Weight_kg<-data_geostat2$Cpue_kgha*data_geostat2$Effort
    
    #data geostat
    yrs_region<-unique(data_geostat2$Year)
    data_geostat2<-data_geostat2[complete.cases(data_geostat2$Weight_kg),]
    
    #ha to km2 ------ so kg/km2
    data_geostat2$Effort<-data_geostat2$Effort/100
    
    #get cpue
    data_geostat2$CPUEkgkm<-data_geostat2$Weight_kg/data_geostat2$Effort
    
    # #add grid to get prediction for simulate data on each cell of the grid (sim$b_i)
    # bering_sea_slope_grid <- FishStatsUtils::bering_sea_slope_grid
    # names(bering_sea_slope_grid)[4]<-'Stratum'
    # bering_sea_slope_grid$Stratum<-99
    # 
    # #load grid per year for all EBS
    # load(file = 'data/data_processed/grid_EBS_NBS.RData') #grid.ebs_year$region
    # grid_ebs<-subset(grid.ebs_year,region=='EBSslope' & Year %in% 2002:2016) #yrs
    # 
    # #grid with info to get prediction on each cell of the SBS grid
    # grids<-data.frame(Lat=grid_ebs$Lat,
    #                   Lon=grid_ebs$Lon,
    #                   Year=grid_ebs$Year,
    #                   Species=rep(sp,times=nrow(grid_ebs)),
    #                   Weight_kg=mean(data_geostat$CPUEkgkm),
    #                   #Weight_kg=0,
    #                   Effort=grid_ebs$Area_in_survey_km2,
    #                   Depth=grid_ebs$DepthGEBCO,
    #                   #BotTemp=grid_ebs$Temp,
    #                   Region=grid_ebs$region,
    #                   CPUEkgkm=mean(data_geostat$CPUEkgkm),
    #                   stringsAsFactors = T)
    # 
    # #grids<-subset(grids,Year %in% unique(data_geostat$Year))
    # summary(grids)
    
    # # #rbind grid and data_geostat to get prediction into grid values when simulating data
    # data_geostat1<-rbind(data_geostat[,c("Lat","Lon","Year","Species","Weight_kg","Effort","Depth","Region",'CPUEkgkm')],
    #                      grids)
    
    #scale depth
    data_geostat2$ScaleLogDepth<-scale(log(data_geostat2$Depth))
    
    #create folder to store results
    # dir.create(paste(out_dir,fol_region,sp,sep='/'),
    #            showWarnings = FALSE)
    #save data
    #save(data_geostat1, file = paste(out_dir,fol_region,sp,'data_geostat_temp_adj.RData',sep='/'))
    
    # Calculate the percentage of zeros for each group
    # percent_zeros <- data_geostat2 %>%
    #   group_by(Year) %>%
    #   summarize(percentage_zeros = mean(Weight_kg == 0) * 100)
    # 
    # # Print the results
    # print(percent_zeros)
    # 
    #regions (predefined in VAST)
    region<-'bering_sea_slope'#c("bering_sea_slope")
    
    #conditional on sp
    if (sp %in% c('Anoplopoma fimbria','Atheresthes stomias')) {
      aniso<-FALSE
    } else {
      aniso<-TRUE
    }
    
    #conditional on sp
    if (sp %in% c('Reinhardtius hippoglossoides','Bathyraja aleutica',"Hippoglossoides elassodon","Sebastes alutus","Sebastes melanostictus","Sebastolobus alascanus")) {
      fieldconfig <- matrix( c(0,"IID",0,"Identity", 0,"IID",0,"Identity"), 
                             ncol=2, 
                             nrow=4, 
                             dimnames=list(c("Omega","Epsilon","Beta","Epsilon_year"),c("Component_1","Component_2")))
    } else {
      fieldconfig <- matrix( c("IID","IID",0,"Identity", "IID","IID",0,"Identity"), 
                             ncol=2, 
                             nrow=4, 
                             dimnames=list(c("Omega","Epsilon","Beta","Epsilon_year"),c("Component_1","Component_2")))
    }
    
    #VAST model settings
    settings <- make_settings(n_x=knots,
                              Region=region, #c("bering_sea_slope","eastern_bering_sea",'northern_bering_sea'
                              purpose="index2", 
                              bias.correct=FALSE,
                              knot_method='grid',
                              use_anisotropy= aniso, #FALSE
                              #FieldConfig = matrix( c(0,"IID",0,"Identity", 0,"IID",0,"Identity"), 
                              FieldConfig = fieldconfig,
                              RhoConfig=c("Beta1"=2,"Beta2"=2,"Epsilon1"=4,"Epsilon2"=4),
                              Version = version,
                              #fine_scale=TRUE,
                              ObsModel = c(2,1),
                              max_cells = Inf,
                              Options = c("Calculate_Range" =  F, 
                                          "Calculate_effective_area" = F)) 
    
    # dim(data_geostat);dim(data_geostat1)
    # 
    # #Kmeans_knots-200
    # if (!file.exists(paste(out_dir,fol_region,sp,'Kmeans_knots-',knots,'.RData',sep='/')) ) {
    #   file.copy(paste(out_dir,fol_region,sp,'Kmeans_knots-',knots,'.RData',sep='/'),
    #             paste(out_dir,fol_region,sp,'Kmeans_knots-',knots,'.RData',sep='/'))}
    
    #covariate data
    #if (cov=='depth') {
      
      #covariate data - filter by year and complete cases for env variables
      covariate_data<-data_geostat2[complete.cases(data_geostat2[,c('Depth')]),]
      covariate_data$Year<-NA #because depth is not variant over time
      
      #formula and predictors settings for each model
      formula<-' ~ bs(ScaleLogDepth, degree=2, intercept=FALSE)'
      X1config_cp = array( c(1,1), dim=c(1,1))
      
      #predictor settings
      X2config_cp = X1config_cp
      
    # } else if (cov=='cpe') {
    #   
    #   covariate_data <- data.frame(Year = c(coldpool:::cold_pool_index$YEAR, 2020),
    #                                Lat = mean(data_geostat1$Lat),
    #                                Lon = mean(data_geostat1$Lon),
    #                                cpe = c(cpe, 0))
    #   
    #   # Load covariates
    #   formula <- ~ cpe
    #   Xconfig_zcp <- array(2, dim=c(2,1,1) )
    #   X1config_cp <- as.matrix(2)
    #   X2config_cp <- as.matrix(2)
    # }
    # 
    # #to get predictions in locations but not influencing fit
    # pred_TF <- rep(1, nrow(data_geostat1))
    # pred_TF[1:nrow(data_geostat)] <- 0
    
    if (sp %in% c("Hippoglossoides elassodon","Sebastes melanostictus","Sebastolobus alascanus")) {
      steps<-5
    } else{
      steps<-1
    }
    
    #fit
    fit <- tryCatch( {fit_model(settings=settings,
                                Lat_i=data_geostat2$Lat, 
                                Lon_i=data_geostat2$Lon,
                                t_i=data_geostat2$Year,
                                b_i=data_geostat2$Weight_kg,
                                c_iz = as.numeric(factor(data_geostat2$Species))-1,
                                a_i=data_geostat2$Effort,
                                input_grid=bering_sea_slope_grid,
                                getJointPrecision = TRUE,
                                test_fit=FALSE,
                                #create_strata_per_region = TRUE,  
                                covariate_data = covariate_data,
                                X1_formula =  formula,
                                X2_formula = formula, 
                                newtonsteps = steps)},
                     error = function(cond) {
                       message("Did not converge. Here's the original error message:")
                       message(cond)
                       cat(paste(" ERROR MODEL IS NOT CONVERGING "))  
                       # Choose a return value in case of error
                       return(NULL)
                     })

    #add index
    df_ind$ind<-fit$Report$Index_ctl[1,,1]
    names(df_ind)[ncol(df_ind)]<-s
    
    
  }
  
  #load EBS+NBS index
  load(paste0('./output/slope/vast/',sp,'/fit_st.RData'))
  slope<-data.frame(fit$Report$Index_ctl[1,,])
  rownames(slope)<-NULL
  colnames(slope)<-c('SBS')
  df_ind<-cbind(df_ind,slope)
  
  #load EBS+NBS index
  load(paste0('./output/vast/',sp,'/fit.RData'))
  ebs_ind<-data.frame(fit$Report$Index_ctl[1,as.character(2002:2016),1:3])
  rownames(ebs_ind)<-NULL
  colnames(ebs_ind)<-c('EBS_NBS','NBS','EBS')
  df_ind<-cbind(df_ind,ebs_ind)
  
  #append df
  ind_list[[sp]]<-df_ind
}


# Combine data for all species in ind_list
ind_list2 <- do.call(rbind, lapply(names(ind_list), function(s) {
  ind_list1 <- ind_list[[s]]
  ind_list1 <- drop_units(ind_list1)
  
  # Calculate combined indices
  ind_list1$EBS_NBS_SBS10 <- ind_list1$s10 + ind_list1$EBS_NBS
  ind_list1$EBS_NBS_SBS90 <- ind_list1$s90 + ind_list1$EBS_NBS
  ind_list1$EBS_NBS_SBS <- ind_list1$SBS + ind_list1$EBS_NBS
  
  # Reshape for plotting
  ind_list1_melted <- reshape2::melt(ind_list1, id.vars = c('yrs', 'species'))
  return(ind_list1_melted)
}))

# Plot
ggplot(data = subset(ind_list2, variable %in% c('EBS_NBS_SBS', 'EBS_NBS_SBS10', 'EBS_NBS_SBS90'))) +
  geom_line(aes(x = yrs, y = value / 1000, color = variable)) +
  scale_color_manual(
    values = c('EBS_NBS_SBS' = 'black', 'EBS_NBS_SBS10' = 'lightgreen', 'EBS_NBS_SBS90' = 'darkgreen'),
    labels = c('EBS_NBS_SBS' = 'mean', 'EBS_NBS_SBS10' = 'p10', 'EBS_NBS_SBS90' = 'p90')
  ) +
  facet_wrap(~ species,scales='free_y') +  # facet by species
  theme_bw() +
  theme(strip.background = element_blank())+
  labs(x = 'Year', y = 't', color = 'Combined index SR')
