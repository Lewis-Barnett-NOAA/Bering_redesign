########################
##
## slope - shelf combination and exploration
##
########################

#clear all objects
rm(list = ls(all.names = TRUE)) 
#free up memrory and report the memory usage
gc() 

#set seed
set.seed(6)

#libraries from cran to call or install/load
pack_cran<-c("splines",'ggplot2','dplyr','doParallel')

#install pacman to use p_load function - call library and if not installed, then install
if (!('pacman' %in% installed.packages())) {
  install.packages("pacman")}

#install coldpool to extract SBT for the EBS
if (!('VAST' %in% installed.packages())) {
  devtools::install_github("james-thorson/VAST@main", INSTALL_opts="--no-staged-install")};library(VAST)

#load/install packages
pacman::p_load(pack_cran,character.only = TRUE)

#setwd
#out_dir<-'C:/Users/Daniel.Vilas/Work/Adapting Monitoring to a Changing Seascape/'
out_dir<- '/Users/daniel/Work/Adapting Monitoring to a Changing Seascape/'
setwd(out_dir)

#version VAST (cpp)
version<-'VAST_v14_0_1'

#number of knots
knots<-'300' #200

#list of sp
splist<-list.dirs('./data processed/',full.names = FALSE,recursive = FALSE)

#folder region - only slope
fol_region<-c('slope EBS VAST','slope_outshelf EBS VAST')[1]
dir.create(paste0('./',fol_region))

#load grid
load('./extrapolation grids/lastversion_grid_EBS.RData')
load('./data processed/grid_EBS_NBS.RData')

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
 
splist<-list() 
 
spp_vect<-c("Atheresthes evermanni","Atheresthes stomias",
            "Gadus chalcogrammus","Gadus macrocephalus",
            "Hippoglossoides elassodon","Reinhardtius hippoglossoides",
            'Bathyraja aleutica')


for (sp in spp) {

 #example
 #sp<-'Reinhardtius hippoglossoides'
 #sp<-'Atheresthes stomias'

  #sp<-spp[1]
   
  
  if (sp %in% spp_vect) {
    df1<-readRDS(paste0('./data processed/species/',sp,'/data_geostat_slope_adj.rds'))
    
  } else {
    
    df1<-readRDS(paste0('./data processed/species/',sp,'/data_geostat.rds'))
    df1<-cbind(df1,"ADJ_WEIGHT_FREQ"=NA,"ADJ_KG_HA"=NA)
  }
  

#for slope data
df11<-subset(df1,survey_name== "Eastern Bering Sea Slope Bottom Trawl Survey")
df11$survey_name[df11$survey_name == 'Eastern Bering Sea Slope Bottom Trawl Survey'] <- 'slope'
#yrs only for slope
yrs<-unique(df11$year)

# #for shelf data
# df12<-subset(df1,survey_name== "Eastern Bering Sea Crab/Groundfish Bottom Trawl Survey")
# df12$survey_name[df12$survey_name == 'Eastern Bering Sea Crab/Groundfish Bottom Trawl Survey'] <- 'EBS shelf'
# 
# #for shelf data deeper than 106 (3rd quartile)
# df13<-subset(df1,survey_name== "Eastern Bering Sea Crab/Groundfish Bottom Trawl Survey" & depth_m>=100)
# df13$survey_name[df13$survey_name == 'Eastern Bering Sea Crab/Groundfish Bottom Trawl Survey'] <- '>100 EBS shelf'

#rbind region specific df
df1<-df11
df1<-subset(df1, year %in% yrs)

#store df
splist[[sp]]<-df1

} 
 
#rbind list dfs
df2<-dplyr::bind_rows(splist, .id = "column_label")

#check
splist$`Gadus macrocephalus`
splist$`Glyptocephalus zachirus`

ebs_layers <- akgfmaps::get_base_layers(select.region = "ebs", set.crs = "EPSG:3338")
ebs_layers$survey.strata <- sf::st_transform(ebs_layers$survey.strata, crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")#'+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')

#plot colored deeper than 100 meters
# ggplot()+
#   geom_point(data=subset(df12,year==2016),aes(x=lon_start,y=lat_start,group=year))+
#   geom_point(data=subset(df11,year==2016),aes(x=lon_start,y=lat_start,group=year),col='blue',alpha=0.8)+
#   geom_sf(data=ebs_layers$survey.strata,fill = NA)+
#   geom_point(data=subset(df12,year==2016 & depth_m >100),aes(x=lon_start,y=lat_start,group=year),col='green',alpha=0.8)+
#   facet_wrap(~year)

##plot boxplot by region and species
df2$survey_name <- factor(df2$survey_name, levels = c('EBS shelf','>100 EBS shelf','slope'))
ggplot()+
  geom_boxplot(data=df2,aes(x=as.factor(year),y=log(cpue_kgha+1),group=interaction(year,survey_name,scientific_name),color=survey_name))+
  facet_wrap(~scientific_name,scales='free',nrow=5)

#selected species - remove spp without observations in the slope
spp_slope<-c(#'Limanda aspera',
       'Gadus chalcogrammus',
       'Gadus macrocephalus',
       'Atheresthes stomias',
       'Reinhardtius hippoglossoides',
       #'Lepidopsetta polyxystra',
       'Hippoglossoides elassodon',
       #'Pleuronectes quadrituberculatus',
       #'Hippoglossoides robustus',
       #'Boreogadus saida',
       #'Eleginus gracilis',
       'Anoplopoma fimbria',
       'Chionoecetes opilio',
       #'Paralithodes platypus',
       #'Paralithodes camtschaticus',
       'Chionoecetes bairdi',
       'Sebastes alutus',
       'Sebastes melanostictus',
       'Atheresthes evermanni',
       'Sebastes borealis',
       'Sebastolobus alascanus',
       'Glyptocephalus zachirus',
       'Bathyraja aleutica')


#number of simulations
n_sim_hist<-100
 
#fit with depth or CPE
cov<-c('depth','cpe')[1]

#fitfiles<-list.files('./slope EBS VAST/',recursive = TRUE,pattern = 'fit.RData')
#spp<-gsub('/fit.RData','',fitfiles)
df_conv<-data.frame(spp=c(spp))

#prepare dataframe optimization
#df_conv$slope<-NA
df_conv$slope_st<-NA

#loop over species
for (sp in spp_slope) { #[c(10,12:15)]

#example
#sp<-'Sebastolobus alascanus'
#sp<-spp_slope[9]  
#sp<-spp_slope[5]  

cat(paste0('############### ',sp,' #########################\n'))

#df1<-readRDS(paste0('./data processed/',sp,'/data_geostat_temp.rds'))
#select rows and rename

#filter by sp
df3<-subset(df2,scientific_name==sp)

if (sp %in% spp_vect) {
  
  df3<-df3[,c("lat_start","lon_start","year",'scientific_name','ADJ_KG_HA','effort','depth_m','survey_name')]
  colnames(df3)<-c('Lat','Lon','Year','Species','Cpue_kgha','Effort','Depth','Region')
  
} else {
  
  df3<-df3[,c("lat_start","lon_start","year",'scientific_name','cpue_kgha','effort','depth_m','survey_name')]
  colnames(df3)<-c('Lat','Lon','Year','Species','Cpue_kgha','Effort','Depth','Region')
  
  
}

#get weight
df3$Weight_kg<-df3$Cpue_kgha*df3$Effort

#data geostat
yrs_region<-unique(df3$Year)
data_geostat<-df3[complete.cases(df3$Weight_kg),]

if (fol_region=='slope EBS VAST') {
  data_geostat<-subset(data_geostat,Region=='slope')}

#ha to km2 ------ so kg/km2
data_geostat$Effort<-data_geostat$Effort/100

#get cpue
data_geostat$CPUEkgkm<-data_geostat$Weight_kg/data_geostat$Effort

#add grid to get prediction for simulate data on each cell of the grid (sim$b_i)
load('./extrapolation grids/bering_sea_slope_grid.rda')
names(bering_sea_slope_grid)[4]<-'Stratum'
bering_sea_slope_grid$Stratum<-99

#load grid per year for all EBS
load(file = './data processed/grid_EBS_NBS.RData') #grid.ebs_year$region
grid_ebs<-subset(grid.ebs_year,region=='EBSslope' & Year %in% 2002:2016) #yrs

#grid with info to get prediction on each cell of the SBS grid
grids<-data.frame(Lat=grid_ebs$Lat,
                    Lon=grid_ebs$Lon,
                    Year=grid_ebs$Year,
                    Species=rep(sp,times=nrow(grid_ebs)),
                    Weight_kg=mean(data_geostat$CPUEkgkm),
                    #Weight_kg=0,
                    Effort=grid_ebs$Area_in_survey_km2,
                    Depth=grid_ebs$DepthGEBCO,
                    #BotTemp=grid_ebs$Temp,
                    Region=grid_ebs$region,
                    CPUEkgkm=mean(data_geostat$CPUEkgkm),
                    stringsAsFactors = T)

#grids<-subset(grids,Year %in% unique(data_geostat$Year))
summary(grids)
 
# #rbind grid and data_geostat to get prediction into grid values when simulating data
data_geostat1<-rbind(data_geostat[,c("Lat","Lon","Year","Species","Weight_kg","Effort","Depth","Region",'CPUEkgkm')],
                      grids)

#scale depth
data_geostat1$ScaleLogDepth<-scale(log(data_geostat1$Depth))

#create folder to store results
dir.create(paste(out_dir,fol_region,sp,sep='/'),
           showWarnings = FALSE)
#save data
save(data_geostat1, file = paste(out_dir,fol_region,sp,'data_geostat_temp_adj.RData',sep='/'))

# Calculate the percentage of zeros for each group
percent_zeros <- data_geostat %>%
  group_by(Year) %>%
  summarize(percentage_zeros = mean(Weight_kg == 0) * 100)

# Print the results
print(percent_zeros)

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

dim(data_geostat);dim(data_geostat1)

#Kmeans_knots-200
if (!file.exists(paste(out_dir,fol_region,sp,'Kmeans_knots-',knots,'.RData',sep='/')) ) {
  file.copy(paste(out_dir,fol_region,sp,'Kmeans_knots-',knots,'.RData',sep='/'),
            paste(out_dir,fol_region,sp,'Kmeans_knots-',knots,'.RData',sep='/'))}

#covariate data
  if (cov=='depth') {
    
    #covariate data - filter by year and complete cases for env variables
    covariate_data<-data_geostat1[complete.cases(data_geostat1[,c('Depth')]),]
    covariate_data$Year<-NA #because depth is not variant over time
    
    #formula and predictors settings for each model
    formula<-' ~ bs(ScaleLogDepth, degree=2, intercept=FALSE)'
    X1config_cp = array( c(1,1), dim=c(1,1))
  
    #predictor settings
    X2config_cp = X1config_cp
    
  } else if (cov=='cpe') {
    
    covariate_data <- data.frame(Year = c(coldpool:::cold_pool_index$YEAR, 2020),
                                 Lat = mean(data_geostat1$Lat),
                                 Lon = mean(data_geostat1$Lon),
                                 cpe = c(cpe, 0))
    
    # Load covariates
    formula <- ~ cpe
    Xconfig_zcp <- array(2, dim=c(2,1,1) )
    X1config_cp <- as.matrix(2)
    X2config_cp <- as.matrix(2)
  }

#to get predictions in locations but not influencing fit
pred_TF <- rep(1, nrow(data_geostat1))
pred_TF[1:nrow(data_geostat)] <- 0

if (sp %in% c("Hippoglossoides elassodon","Sebastes melanostictus","Sebastolobus alascanus")) {
  steps<-5
} else{
  steps<-1
}

#fit
fit <- tryCatch( {fit_model(settings=settings,
                            Lat_i=data_geostat1$Lat, 
                            Lon_i=data_geostat1$Lon,
                            t_i=data_geostat1$Year,
                            b_i=data_geostat1$Weight_kg,
                            c_iz = as.numeric(factor(data_geostat1$Species))-1,
                            a_i=data_geostat1$Effort,
                            input_grid=bering_sea_slope_grid,
                            getJointPrecision = TRUE,
                            test_fit=FALSE,
                            #create_strata_per_region = TRUE,  
                            covariate_data = covariate_data,
                            X1_formula =  formula,
                            X2_formula = formula, 
                            newtonsteps = steps,
                            PredTF_i = pred_TF,
                            #X_gtp = X_gtp,
                            working_dir = paste(out_dir,fol_region,sp,'/',sep='/'))},
                 error = function(cond) {
                   message("Did not converge. Here's the original error message:")
                   message(cond)
                   cat(paste(" ERROR MODEL IS NOT CONVERGING "))  
                   # Choose a return value in case of error
                   return(NULL)
                 })
  
  check_fit(fit$parameter_estimates)
  
  #convergence
  df_conv[which(df_conv$spp==sp),'slope_st']<-fit$parameter_estimates$Convergence_check
  

  # #save model if fit object complet
  # if (class(fit)=='fit_model') {
  #   
  #   save(list = 'fit',file=paste(out_dir,fol_region,sp,'fit_st.RData',sep = '/'))}
  # 
  #array to store simulated densities/CPUE
  sim_dens<-array(NA,
                  dim=c(nrow(bering_sea_slope_grid),length(unique(data_geostat1$Year)),n_sim_hist),
                  dimnames=list(1:nrow(bering_sea_slope_grid),sort(unique(data_geostat1$Year)),1:n_sim_hist))
  
  #create folder simulation data
  dir.create(paste0('./output/species/',sp,'/simulated historical data slope/'))

  #save if fit model object complet
  if (class(fit)=='fit_model') {
    
    save(list = 'fit',file=paste(out_dir,fol_region,sp,'fit_st.RData',sep = '/'))

    #load('./slope EBS VAST/Anoplopoma fimbria/fit.RData')
    
    #loop over simulations
    for (isim in 1:n_sim_hist) { #simulations
      
      #isim<-1
      
      #print simulation to check progress
      cat(paste(" #############   Species", sp, match(sp,spp_slope), 'out of',length(spp_slope),  "  #############\n",
                " #############  historical simulation", isim, "of",n_sim_hist, " #############\n"))
      
      #simulate data from OM
      Sim1 <- FishStatsUtils::simulate_data(fit = fit, #kg/km2
                                            type = 1,
                                            random_seed = isim)
      
      #select simulated data that belong to grid points
      sim_bio <-matrix(data = Sim1$b_i[pred_TF == 1], #kg
                       nrow = nrow(bering_sea_slope_grid),
                       ncol = length(unique(unique(data_geostat1$Year))))
      
      #biomass (kg) to CPUE (kg/km2)
      sim_dens[,,isim]<-sim_bio/bering_sea_slope_grid$Area_in_survey_km2
      
    }
  }

  dir.create(paste0("./output/species/",sp))
  dir.create(paste0("./output/species/",sp,'/simulated historical data/'))
  #save data
  save(sim_dens, file = paste0("./output/species/",sp,'/simulated historical data/sim_dens_slope.RData'))
  
  #store
  #sim_hist_dens_spp[,,,sp]<-sim_dens
}


#####################
# check the slope model that converged
#####################

#fitfiles<-list.files('./slope EBS VAST/',recursive = TRUE,pattern = 'fit.RData')
#spp<-gsub('/fit.RData','',fitfiles)
df_conv<-data.frame(spp=c(spp))

#prepare dataframe optimization
#df_conv$slope<-NA
df_conv$slope_st<-NA
df_conv$EBS_NBS<-NA

for (sp in spp) {
  
  #sp<-spp[7]
  
  cat(paste0('#####  ',sp,'  #######\n'))
  
  #f<-fitfiles[1]
  if (length(list.files(paste0('./slope EBS VAST/',sp,'/'),pattern = 'fit_st.RData'))!=0) {
    load(paste0('./slope EBS VAST/',sp,'/fit_st.RData'))
  }
  
  if (length(list.files(paste0('./slope EBS VAST/',sp,'/'),pattern = 'fit_st.RData'))==0) {
    df_conv[which(df_conv$spp==sp),'slope_st']<-'no model'
  } else if (is.null(fit)) {
    df_conv[which(df_conv$spp==sp),'slope_st']<-'non convergence'
  } else if (is.null(fit$parameter_estimates$Convergence_check)) {
    df_conv[which(df_conv$spp==sp),'slope_st']<-fit$Report
  }else{
    df_conv[which(df_conv$spp==sp),'slope_st']<-fit$parameter_estimates$Convergence_check
  }
  
  #non ST if nonconvergence in st
  # if ( df_conv[which(df_conv$spp==sp),'slope']!='There is no evidence that the model is not converged') {
  
  # if (length(list.files(paste0('./slope EBS VAST/',sp,'/'),pattern = 'fit.RData'))!=0) {
  #   load(paste0('./slope EBS VAST/',sp,'/fit.RData'))
  # }
  # 
  # #EBS+NBS fit
  # if (file.exists(paste0('./shelf EBS NBS VAST//',sp,'/fit.RData'))) {
  #   
  #   #load fit file
  #   load(paste0('./shelf EBS NBS VAST//',sp,'/fit.RData'))
  #   
  #   #dimensions and check fit
  #   #dim(fit$Report$D_gct) #53464
  #   #check_fit(fit$parameter_estimates)
  #   
  #   if (is.null(fit)) {
  #     df_conv[which(df_conv$spp==sp),'EBS_NBS']<-'non convergence'
  #   } else if (is.null(fit$parameter_estimates$Convergence_check)) {
  #     df_conv[which(df_conv$spp==sp),'EBS_NBS']<-fit$Report
  #   }else{
  #     df_conv[which(df_conv$spp==sp),'EBS_NBS']<-fit$parameter_estimates$Convergence_check
  #   }
  #   
  # } else {
  #   
  #   df_conv[which(df_conv$spp==sp),'EBS_NBS']<-'no model'
    
  }
  


#sort table by sci name
df_conv<-df_conv[order(df_conv$spp),]

# Replace specific values across all columns using ifelse
df_conv[] <- lapply(df_conv, function(x) ifelse(x == "There is no evidence that the model is not converged", 
                                                "convergence", x))

# Replace specific values across all columns using ifelse
df_conv[] <- lapply(df_conv, function(x) ifelse(x %in% c("The model is likely not converged",'Model is not converged'), 
                                                "non-convergence", x))


df_conv
rownames(df_conv)<-NULL
write.csv(df_conv,file=('./tables/slope_conv.csv'))
#save 100 simulated historical densities for all species
#save(sim_hist_dens_spp, file = paste0("./output/species/sim_hist_dens_spp.RData"))
#save true densities and index for all species
#save(dens_index_hist_OM, file = paste0("./output/species/dens_index_hist_OM.RData"))

######################
# RESHAPE SIMULATED HISTORICAL DATA
######################

# Initializing parallel backend
cl <- makeCluster(detectCores()-1)  # Using all available cores
registerDoParallel(cl)

n_sim<-100

#array to store simulated densities/CPUE
sim_dens1 <- array(NA,
                   dim = c(nrow(bering_sea_slope_grid), length(spp), length(2002:2016), n_sim),
                   dimnames = list(1:nrow(bering_sea_slope_grid), spp, sort(2002:2016), 1:n_sim))

#parallel loop over spp
foreach(sp = spp_slope) %do% {
  
  #sp<-spp[1]
  
  #load data
  load(paste0('./output/species/', sp, '/simulated historical data/sim_dens_slope.RData'))
  
  #parallel loop over years and simulations
  foreach(y = 2002:2016) %:%
    foreach(sim = 1:n_sim) %do% {
      #y<-'2002';sim<-'1'
      
      #store results
      sim_dens1[, sp, as.character(y), as.character(sim)] <- sim_dens[, as.character(y), as.character(sim)]
    }
}

# Stopping the parallel backend
stopCluster(cl)

#store HIST simulated data
save(sim_dens1, file = paste0('./output slope/species/ms_sim_dens_slope.RData'))  
#load(file = paste0('./output slope//species/ms_sim_dens_slope.RData'))


################################################
# CHECK FIT DURING MODEL RUNS
################################################



# Create a table grob
table_plot <- gridExtra::tableGrob(df_conv)

# Plot the table
grid.newpage()
grid.draw(table_plot)

#check sim
#simulate data from OM
Sim1 <- FishStatsUtils::simulate_data(fit = fit, #kg/km2
                                      type = 1,
                                      random_seed = 1)

#select simulated data that belong to grid points
sim_bio <-matrix(data = Sim1$b_i[pred_TF == 1], #kg
                 nrow = nrow(bering_sea_slope_grid),
                 ncol = length(unique(unique(data_geostat1$Year))))

