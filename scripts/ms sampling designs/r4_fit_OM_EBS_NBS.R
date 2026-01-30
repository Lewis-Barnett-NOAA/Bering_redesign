####################################################################
####################################################################
##    
##    fit single sp VAST model for the EBS shelf and NBS using temp (SBT, BotTemp) with cubic effect 
##    Daniel Vilas (danielvilasgonzalez@gmail.com/dvilasg@uw.edu/daniel.vilas@noaa.gov)
##    Lewis Barnett, Stan Kotwicki, Zack Oyafuso, Megsie Siple, Leah Zacher, Lukas Defilippo, Andre Punt
##
##    *evaluate parameters : https://github.com/James-Thorson-NOAA/VAST/blob/main/R/make_parameters.R
##    https://rdrr.io/github/James-Thorson/VAST/man/check_fit.html
##
####################################################################
####################################################################

#clear all objects
rm(list = ls(all.names = TRUE)) 
#free up memrory and report the memory usage
gc() 

#libraries from cran to call or install/load
pack_cran<-c("splines",'dplyr')

#install pacman to use p_load function - call library and if not installed, then install
if (!('pacman' %in% installed.packages())) {
  install.packages("pacman")}

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
version<-"VAST_v14_0_1" #if using "VAST_v13_1_0" follow covariate values

#number of knots
knots<-'500' #1000 

#years
#yrs<-1982:2022
yrs<-setdiff(1982:2022,2020) #remove 2020 because there were no survey in this year due to COVID

#list of sp
spp<-list.dirs('./data processed/species/',full.names = FALSE,recursive = FALSE)

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
       'Chionoecetes bairdi',
       'Sebastes alutus',
       #'Sebastes melanostictus',
       'Atheresthes evermanni')

#folder region
fol_region<-c('shelf EBS NBS VAST')

#load grid
load('./data processed/grid_EBS_NBS.RData')

#dir create for slope region results
dir.create(paste(out_dir,fol_region,sep='/'))

#dir create for splist
for (sp in spp) {
  dir.create(paste(out_dir,fol_region,sp,sep='/'))
}

#loop over species to fit models
for (sp in spp) {

#=sp<-spp[18]

#print year to check progress
cat(paste("\n","    ----- ", sp, " -----\n"))  

#read data_geostat_temp file
df1<-readRDS(paste0('./data processed/species/',sp,'/data_geostat_temp.rds'))

#df1[which(df1$year==2020),'bottom_temp_c']<-NA
df2<-subset(df1,year %in% c(yrs,2020))

#select rows and rename
df3<-df2[,c("lat_start","lon_start","year",'scientific_name','weight_kg','effort','depth_m','LogDepth',"ScaleLogDepth",'Scalebottom_temp_c','bottom_temp_c','survey_name')]
colnames(df3)<-c('Lat','Lon','Year','Species','Weight_kg','Swept_area','Depth','LogDepth','ScaleLogDepth','ScaleBotTemp','SBT_insitu','Region')

#data geostat
df4<-subset(df3,Region %in% c("Eastern Bering Sea Crab/Groundfish Bottom Trawl Survey",
                              "Northern Bering Sea Crab/Groundfish Survey - Eastern Bering Sea Shelf Survey Extension"))

data_geostat<-df4[complete.cases(df4[,c('Weight_kg')]),]
data_geostat<-subset(data_geostat,Year %in% yrs)

#if kamtchatka arrowtooth flounder only use data from 1991 because of missidentification issue
if (sp=='Atheresthes evermanni') {
  data_geostat<-subset(data_geostat,Year %in% 1991:2022)
}

#covariate data - filter by year and complete cases for env variables
#covariate_data<-subset(df2,Year>=yrs_region[1] & Year<=yrs_region[2])
covariate_data<-df3[complete.cases(df3[,c('SBT_insitu')]),] #,'ScaleLogDepth'

#get grid_ebs_nbs
grid_ebs<-grid.ebs_year[which(grid.ebs_year$region != 'EBSslope' & grid.ebs_year$Year %in% unique(data_geostat$Year)),]

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
data_geostat$Swept_area<-data_geostat$Swept_area/100 #(from ha to km²)

#rbind grid and data_geostat to get prediction into grid values when simulating data
data_geostat1<-rbind(data_geostat[,c("Lat","Lon","Year","Species","Weight_kg","Swept_area","Depth","SBT_insitu","Region")],
                     grid_df)

#to get predictions in locations but not influencing fit
pred_TF <- rep(1, nrow(data_geostat1))
pred_TF[1:nrow(data_geostat)] <- 0

#save data
saveRDS(data_geostat1,paste(out_dir,fol_region,sp,'data_geostat_temp.rds',sep='/'))

# Calculate the percentage of zeros for each group
print(
  percent_zeros <- data_geostat %>%
    group_by(Year) %>%
    summarize(percentage_zeros = mean(Weight_kg == 0) * 100)
)

#regions (predefined in VAST)
region<-c("northern_bering_sea","eastern_bering_sea")

#VAST model settings
settings <- make_settings(n_x=knots, 
                          Region=region, #c("bering_sea_slope","eastern_bering_sea",'northern_bering_sea'
                          purpose="index2", 
                          bias.correct=FALSE,
                          knot_method='grid',
                          use_anisotropy=TRUE,
                          FieldConfig = matrix( c("IID","IID",0,"Identity", "IID","IID",0,"Identity"), 
                                                ncol=2, 
                                                nrow=4, 
                                                dimnames=list(c("Omega","Epsilon","Beta","Epsilon_year"),c("Component_1","Component_2"))),
                          #FieldConfig = c("Omega1"="IID", "Epsilon1"="IID", "Omega2"="IID", "Epsilon2"="IID",'Beta1'=0,'Beta2'=0),
                          RhoConfig=c("Beta1"=2,"Beta2"=2,"Epsilon1"=4,"Epsilon2"=4), #NON CONVERGENCE for'Lepidopsetta polyxystra','Paralithodes platypus', and Epsilon1 = 2 (RW)
                          Version = version,
                          #fine_scale=TRUE,
                          ObsModel = c(2,1), #obs
                          max_cells = Inf,
                          Options = c("Calculate_Range" =  F, 
                                      "Calculate_effective_area" = F)) 

#formula and predictors settings for each model
X1_formula<-' ~ bs(SBT_insitu, degree=3, intercept=FALSE)'
X1config_cp = array( c(1,1), dim=c(1,1))

#formula for positive catch rates equal to presence/absence
X2_formula<-X1_formula

#predictor settings
X2config_cp = X1config_cp

#fit model #### ADD TryCatch{(),}
fit <- tryCatch( {fit_model(settings=settings,
                            Lat_i=data_geostat1$Lat, 
                            Lon_i=data_geostat1$Lon,
                            t_i=data_geostat1$Year,
                            b_i=data_geostat1$Weight_kg,
                            c_iz = as.numeric(factor(data_geostat1$Species))-1,
                            a_i=data_geostat1$Swept_area,
                            #input_grid=grid.ebs,
                            getJointPrecision = TRUE,
                            test_fit=FALSE,
                            create_strata_per_region = TRUE,  
                            covariate_data = covariate_data[,c('Year',"Lat","Lon",'SBT_insitu',"Weight_kg")], 
                            X1_formula =  X1_formula,
                            X2_formula = X2_formula, 
                            newtonsteps = 1,
                            PredTF_i = pred_TF,
                            #X_gtp = X_gtp,
                            working_dir = paste(out_dir,fol_region,sp,'/',sep='/'))},
                 error = function(cond) {
                   message("Did not converge. Here's the original error message:")
                   message(cond)
                   # Choose a return value in case of error
                   return(NULL)
                 })

#save fit
save(list = "fit", file = paste(out_dir,fol_region,sp,'fit.RData',sep='/')) #paste(yrs_region,collapse = "")

#check obs vs pred
if (!is.null(fit)) {
  #add predictions
  data_geostat1$pred<-fit$Report$D_i
  names(data_geostat1)[ncol(data_geostat1)]<-'obs'
  
  print(
    #plot comparison pred/obs
    ggplot(data = data_geostat1, aes(x = obs)) +
      geom_histogram(aes(color = "obs"), bins = 20, alpha = 0.5, fill='white',position = "identity") +
      geom_histogram(data = data_geostat1, aes(x = Weight_kg, color = "pred"), bins = 20, alpha = 0.5,fill='white', position = "identity") +
      scale_color_manual(values = c("obs" = "blue", "pred" = "red"),name='') +
      labs(fill = "") +
      facet_wrap(~Year) +
      theme_bw()
  )
}

#remove memory
gc()
}
