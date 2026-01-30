####################################################################
####################################################################
##    
##    simulate data and survey for historical and projected years
##    prepare estimates to compute design-based indices
##    danielvilasgonzalez@gmail.com/dvilasg@uw.edu
##    
##    systematic / spatially balanced / random
##
####################################################################
####################################################################

#clear all objects
rm(list = ls(all.names = TRUE)) 

#free up memrory and report the memory usage
gc() 

#libraries from cran to call or install/load
pack_cran<-c('ggplot2','units','splines','raster','sp','Spbsampling','sf','doParallel','foreach')

#install pacman to use p_load function - call library and if not installed, then install
if (!('pacman' %in% installed.packages())) {
  install.packages("pacman")}

#install VAST if it is not
if (!('VAST' %in% installed.packages())) {
  devtools::install_github("james-thorson/VAST@main", INSTALL_opts="--no-staged-install")};library(VAST)

#load/install packages
pacman::p_load(pack_cran,character.only = TRUE)

#setwd
out_dir<-'C:/Users/Daniel.Vilas/Work/Adapting Monitoring to a Changing Seascape/'
out_dir<-'/Users/daniel/Work/Adapting Monitoring to a Changing Seascape/'
setwd(out_dir)

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
       'Chionoecetes bairdi')


#remove Anoploma and Reinhardtius because habitat preference reasons
spp<-setdiff(spp, c('Anoplopoma fimbria','Reinhardtius hippoglossoides'))

#yrs
yrs<-setdiff(1982:2022,2020)

#how manyt projected years we want
n_proj<-5

#project_yrs
project_yrs<-((yrs[length(yrs)])+1):(yrs[length(yrs)]+n_proj)

###################################
# LOAD GRID EBS (remember to keep the same order as in fit_model if multiple grids)
###################################

#load grid of NBS and EBS
load('./extrapolation grids/northern_bering_sea_grid.rda')
load('./extrapolation grids/eastern_bering_sea_grid.rda')
grid<-as.data.frame(rbind(data.frame(northern_bering_sea_grid,region='NBS'),data.frame(eastern_bering_sea_grid,region='EBS')))
grid$cell<-1:nrow(grid)
grid2<-grid
#add col and row number
x1<-grid[,c('Lon','Lat','cell')]
names(x1)<-c('x','y','z')
coordinates(x1)=~x + y
crs(x1)<-c(crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
x2<-spTransform(x1,'+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')
x3<-data.frame(x2)
x3$x<-as.integer(x3$coords.x1)
x3$y<-as.integer(x3$coords.x2)
lon<-sort(unique(x3$x),decreasing = FALSE) #1556
lat<-sort(unique(x3$y),decreasing = TRUE) #1507
lons<-data.frame(x=lon,col=1:length(lon))
lats<-data.frame(y=lat,row=1:length(lat))
x4<-merge(x3,lons,by='x',all.x=TRUE)
x5<-merge(x4,lats,by='y',all.x=TRUE)
x5<-x5[,c('y','x','z','col','row')]
colnames(x5)<-c('Lat','Lon','cell','col','row')
grid<-x5[,c('Lat','Lon','cell','col','row')]

###################################
# YEARS AND BASELINE
###################################

#load grid
load('./data processed/grid_EBS_NBS.RData')
#yrs<-setdiff(1982:2022,2020)
grid_ebs<-grid.ebs_year[which(grid.ebs_year$region != 'EBSslope' & grid.ebs_year$Year %in% yrs),]
dim(grid_ebs)

#load grid of NBS and EBS
load('./extrapolation grids/northern_bering_sea_grid.rda')
load('./extrapolation grids/eastern_bering_sea_grid.rda')
grid<-as.data.frame(rbind(data.frame(northern_bering_sea_grid,region='NBS'),data.frame(eastern_bering_sea_grid,region='EBS')))
grid$cell<-1:nrow(grid)
grid2<-grid

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
# BASELINE STRATA
###################################

load('./output/baseline_strata.RData')
#add percent of total area per strata
baseline_strata$strata_areas$pct<-baseline_strata$strata_areas$Area_in_survey_km2/sum(baseline_strata$strata_areas$Area_in_survey_km2)

###################################
# SBT scenarios
###################################

#load SBT scenarios table
load('./tables/SBT_projection.RData')#df_sbt

#name SBT scenarios
df_sbt$sbt<-paste0('SBT',df_sbt$sbt_n)
df_sbt$sbt2<-paste0(df_sbt$sbt,'_',df_sbt$Scenario)

#number of historical simulations and projected simulations
n_sim_hist<- 100
n_sim_proj<- 100
 
#number of surveys
n_sur<-100

####################################
# CRABS
####################################

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

crabs<-c('PBL_BKC','PBL_RKC','STM_BKC','SNW_CRB','TNR_CRB')
names(area)<-crabs
crabs_spp<-c('Paralithodes platypus','Paralithodes camtschaticus','Paralithodes platypus','Chionoecetes opilio','Chionoecetes bairdi')

areacrab<-list()

for (i in 1:length(gdb_layers$name)) {
  
  #i<-3
  
  #load grid of NBS and EBS
  load('./extrapolation grids/northern_bering_sea_grid.rda')
  load('./extrapolation grids/eastern_bering_sea_grid.rda')
  grid<-as.data.frame(rbind(data.frame(northern_bering_sea_grid,region='NBS'),data.frame(eastern_bering_sea_grid,region='EBS')))
  grid$cell<-1:nrow(grid)
  #grid2<-grid
  coordinates(grid)<-~Lon + Lat
  proj4string(grid) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") 
  pts<-spTransform(grid,CRSobj = crs(gdb_table1))
  #pts<-as.data.frame(pts)
  
  
  #st to sp
  gdb_tablea<-as(get(paste0('gdb_table',i)),'Spatial')
  #plot(pts)
  #plot(gdb_tablea)
  
  if (length(gdb_tablea$Shape_Area)!=1) {
    iarea<-sum(gdb_tablea$Shape_Area/1000000)
  } else{
    iarea<-gdb_tablea$Shape_Area/1000000
  }
  
  areacrab[[i]]<-iarea
  
  
  #plot(gdb_tablea[2])
  
  #points over polygon
  xx<-over(pts,gdb_tablea)
  
  # Check which points fall within the sf object using st_within
  points_within_polygon <- as.data.frame(pts)[!is.na(xx), 'cell']
  
  #name<-
  grid2$newcolumn<-FALSE
  grid2$newcolumn[points_within_polygon]<-TRUE
  names(grid2)[ncol(grid2)]<-gdb_layers$name[i]
}

areacrab[[1]]<-NULL
areacrab[[4]]<-areacrab[[5]]
names(areacrab)<-crabs

#get cells in each region
PBL_KC_cells<-grid2[which(grid2$Pribilof_BKC_strata==TRUE),'cell']
STM_BKC_cells<-grid2[which(grid2$StMatt_BKC_strata==TRUE),'cell']
EBS_C_cells<-grid2[which(grid2$EBS_CO_CB_strata==TRUE),'cell']

################
# HISTORICAL
################
 
#create a df of 40years x 100sur/sim (it changes on the projected since it simulations have different indeces)
n_sur<-100
sur_df<-data.frame(num=1:(length(yrs)*n_sur),
           year=rep(yrs,times=n_sur),
           sur=rep(1:n_sur,each=length(yrs)))

#simulated densities
load(file = paste0('./output/species/ms_sim_dens.RData'))  #sim_dens1

#ms_sim_survey folder
dir.create('./output/ms_sim_survey/')

#create folder
dir.create(paste0('./output/ms_sim_survey_hist/'))

#loop over n combinations of simulated
for (sim in 1:n_sim_hist) {
  
  #sim<-1
  
  # Convert 0 to '001'
  sim_fol <- sprintf("%03d", sim)
  
  #create folder
  dir.create(paste0('./output/ms_sim_survey_hist/sim',sim_fol))
  
  #array to store
  # index_hist<-array(NA,
  #                   dim=list(length(spp)+length(crabs),3,length(yrs),3,n_sur,nrow(samp_df)),
  #                   dimnames=list(c(spp,crabs),c('STRS_mean','STRS_var','CV_sim'),paste0('y',yrs),c('sys','rand','sb'),1:n_sur,samp_df$samp_scn))
  
  index_hist<-array(NA,
                    dim=list(length(spp),3,length(yrs),3,n_sur,nrow(samp_df)),
                    dimnames=list(c(spp),c('STRS_mean','STRS_var','CV_sim'),paste0('y',yrs),c('sys','rand','sb'),1:n_sur,samp_df$samp_scn))
  
  #loop over sampling design
    for (samp in samp_df$samp_scn)  {
      
      #samp<-'scn1'
      #start_time_parallel <- Sys.time()
      
      #number of sampling design
      s<-match(samp,samp_df$samp_scn)
      
      #when base sampling other files
      if (grepl('base',samp)) {
        
        #conditions on baseline scenarios
        if (samp == 'scnbase') {
          baseline_strata$locations2<-baseline_strata$locations
        } else if (samp == 'scnbase_bis') {
          baseline_strata$locations2<-baseline_strata$locations[which(baseline_strata$locations$corner=='FALSE'),]
        } 
        
        #sort by strata
        #baseline_strata$strata_areas<-baseline_strata$strata_areas[order(baseline_strata$strata_areas$X1),]
        #baseline_strata$locations2<-baseline_strata$locations2[order(baseline_strata$locations2$stratum),]
        names(baseline_strata$strata_areas)[1]<-'X1'
        names(baseline_strata$locations2)[ncol(baseline_strata$locations2)]<-'stratum'
        baseline_strata$strata_areas$Area_in_survey_km2<-baseline_strata$strata_areas$Area_in_survey_km2
        
        #area by strata
        strata_areas <- baseline_strata$strata_areas
        sum(baseline_strata$strata_areas$Area_in_survey_km2)
        
        #strata data
        survey_detail <- data.frame("Stratum" = baseline_strata$strata_areas$X1, #strata
                                    'Nh' = baseline_strata$strata_areas$pct*53464, #number of cells
                                    "nh" = data.frame(table(baseline_strata$locations2$stratum))[,c('Freq')]) #number of sample allocations
        
        #weight of strata for each
        survey_detail$Wh <- survey_detail$Nh / sum(survey_detail$Nh)
        survey_detail$wh <- with(survey_detail, nh/Nh)
        
      } else {
        
        #load optimization results
        load(paste0("./output/ms_optim_allocations_",samp_df[s,'samp_scn'],".RData")) #all
        
        #area
        area_cell<-merge(all$result_list$solution$indices, grid2, by.x='ID',by.y='cell')
        
        #area by strata
        strata_areas <- aggregate(Area_in_survey_km2 ~ X1, 
                                  FUN = sum,
                                  data = area_cell)
        
        #strata data
        survey_detail <- data.frame("Stratum" = all$samples_strata$strata, #strata
                                    'Nh' = as.integer(table(all$result_list$solution$indices$X1)), #number of cells
                                    "nh" = all$samples_strata$n_samples) #number of sample allocations
        
        #weight of strata for each
        survey_detail$Wh <- survey_detail$Nh / sum(survey_detail$Nh)
        survey_detail$wh <- with(survey_detail, nh/Nh)
        
      }   
      
      sum(strata_areas$Area_in_survey_km2)
      
      #array to store simulated densities/CPUE
      alloc<-ifelse(samp=='scnbase_bis',494,520)
      
      #load survey allocations by sampling design
      load(file = paste0('./output/survey_allocations_',samp,'.RData')) #scn_allocations
      dimnames(scn_allocations)[[3]]<-c('sys','rand','sb')
      
        #array to store results  
        sim_survey <- array(NA,
                            dim = c(alloc, length(spp)+2, n_sur, length(unique(yrs)),length(c('sys','rand','sb'))),
                            dimnames = list(1:alloc, c('cell','strata',spp), 1:n_sur,unique(yrs), c('sys','rand','sb')))
        
        
        
        #loop over n combinations of simulated
        for (n in sur_df$num) {
    
          #n<-sur_df$num[1]
          
          #year of simulation
          y<-as.character(sur_df[which(sur_df$num==n),'year'])
          #isurvey of simulation
          sur<-sur_df[which(sur_df$num==n),'sur']
          
          #simulated densities of survey and year
          sim_dens2<-sim_dens1[,,y,sim]
          
          #loop over station allocation approac
          for (apr in c('sys','rand','sb')) {
            
            #apr<-'sys'
    
            #print process        
            cat(paste(" #############  ",samp,'- simdata',sim,'- survey',sur, '- year',y ,'- allocation',apr," #############\n"))
            
            #get densities based on station allocations
            sim_survey<-data.frame(cbind(strata=scn_allocations[scn_allocations[,'sur',apr]==n,c('strata'),apr],
                                    dens=sim_dens2[scn_allocations[scn_allocations[,'sur',apr]==n,c('cell'),apr],]),check.names = FALSE)
            
            sim_survey1<-reshape2::melt(sim_survey,id.vars=c('strata'))
            
            #mean, sum and var by strata and year (variable)
            sim_survey2<-aggregate(x=sim_survey1$value,
                           by=list(strata=sim_survey1$strata,sp=sim_survey1$variable),
                           FUN = function(x) c('mean' = mean(x,na.rm=T), 'sum' = sum(x),'var' = var(x,na.rm=T) ))
            
            #create df
            zzz<-data.frame('strata'=sim_survey2$strata,'sp'=sim_survey2$sp,'mean'=sim_survey2$x[,c('mean')],'var'=sim_survey2$x[,c('var')]) #/length(yy$value)
            zzzz<-merge(zzz,strata_areas,by.x='strata',by.y='X1',all.x=TRUE)
            zzzz<-merge(zzzz,survey_detail,by.x='strata',by.y='Stratum',all.x=TRUE)
            
            #add index strata for sum to compute index (mean strata density * area of strata) kg!
            zzzz$index_strata<-zzzz$mean*zzzz$Area_in_survey_km2
            
            #add strata var 
            zzzz$strs_var<-zzzz$var*(zzzz$Area_in_survey_km2^2)/zzzz$nh #sum(survey_detail$Nh) 
            
            #sum of strata var and mean density across years (kg/km2)
            zzzz1 <- aggregate(zzzz[,c('strs_var','index_strata')], by= list(zzzz$sp),FUN = sum)
            
            #get CV across years
            zzzz1$cv<- sqrt(zzzz1$strs_var) / zzzz1$index_strata
            
            #mean CV 
            mean(zzzz1$cv,na.rm=TRUE)
            
            #get outputs
            STRS_mean <- zzzz1$index_strata
            STRS_var <- zzzz1$strs_var
            CV <- sqrt(STRS_var) / STRS_mean
            
            # ######
            # #CRABS
            # ######
            # 
            # for (c in crabs) {
            #   
            #   #c<-crabs[1]
            #   
            #   if (grepl('PBL',c)) {
            #     cells<-PBL_KC_cells
            #   } else if (grepl('CRB',c)) {
            #     cells<-EBS_C_cells
            #   } else {
            #     cells<-STM_BKC_cells
            #   }
            #   
            #   crab_sp<-crabs_spp[match(c,crabs)]
            #   
            #   #get densities based on station allocations
            #   sim_survey_crabs<-data.frame(cbind(strata=scn_allocations[scn_allocations[,'sur',apr]==n & scn_allocations[,'cell',apr] %in% cells,c('strata'),apr],
            #                                      dens=sim_dens2[scn_allocations[scn_allocations[,'sur',apr]==n & scn_allocations[,'cell',apr] %in% cells,c('cell'),apr],crab_sp]),check.names = FALSE)
            #   names(sim_survey_crabs)[ncol(sim_survey_crabs)]<-crab_sp
            #   
            #   
            #   sim_survey1<-reshape2::melt(sim_survey_crabs,id.vars=c('strata'))
            #   sim_survey1$strata<-'crab'
            #   
            #   #mean, sum and var by strata and year (variable)
            #   sim_survey2<-aggregate(x=sim_survey1$value,
            #                          by=list(strata=sim_survey1$strata,sp=sim_survey1$variable),
            #                          FUN = function(x) c('mean' = mean(x,na.rm=T), 'sum' = sum(x),'var' = var(x,na.rm=T) ))
            #   zzz<-data.frame('strata'=sim_survey2$strata,'sp'=sim_survey2$sp,'mean'=sim_survey2$x[,c('mean')],'var'=sim_survey2$x[,c('var')]) #/length(yy$value)
            #   
            #   #add index strata for sum to compute index (mean strata density * area of strata) kg!
            #   zzz$index_strata<-zzz$mean*areacrab[[c]]
            #   
            #   #add strata var 
            #   zzz$strs_var<-zzz$var*(areacrab[[c]]^2)/nrow(sim_survey_crabs) #sum(survey_detail$Nh) 
            #   
            #   #get CV across years
            #   zzz$cv<- sqrt(zzz$strs_var) / zzz$index_strata
            #   
            #   
            #   #get outputs
            #   STRS_mean <- c(STRS_mean,zzz$index_strata)
            #   STRS_var <- c(STRS_var,zzz$strs_var)
            #   CV <- c(CV,sqrt(zzz$strs_var) / zzz$index_strata)
            #   
            # }
            
            
            #store outputs
            index_hist[,'STRS_mean',paste0('y',y),apr,sur,samp]<-STRS_mean
            index_hist[,'STRS_var',paste0('y',y),apr,sur,samp]<-STRS_var
            index_hist[,'CV_sim',paste0('y',y),apr,sur,samp]<-CV
        
          }
        }
  }
  
  save(index_hist, file = paste0('./output/ms_sim_survey_hist/sim',sim_fol,'/index_hist.RData'))  

}

################
# PROJECTED
################

#dir create sim survey projected
dir.create('./output/ms_sim_survey_proj')

#create a df of 5years x 100sur/sim x x 8sbt scenarios (it changes on the projected since it simulations have different indeces)
n_sur<-100

sur_df<-cbind(expand.grid('year'=project_yrs,'sur'=1:n_sim_proj,'sbt'=df_sbt$sbt_n),'num'=1:4000)


#simulated densities
#load(file = paste0('./output/species/ms_sim_dens.RData'))  #sim_dens1

#ms_sim_survey folder
#dir.create('./output/ms_sim_survey/')

#loop over n combinations of simulated
for (sim in 1:n_sim_proj) {
  
  #sim<-1
  
  # Convert 0 to '001'
  sim_fol <- sprintf("%03d", sim)
  
  #create folder
  dir.create(paste0('./output/ms_sim_survey_proj/sim',sim_fol))

  
#loop over projections
for (sbt in df_sbt$sbt_n) {
  
  #sbt<-df_sbt$sbt_n[1]
    
  #num surveys
  nums<-sur_df[which(sur_df$sbt==sbt),'num']
  
  #sbt<-df_sbt$sbt_n[1]
  #load densities projections
  load(paste0('./output/species/SBT',sbt,' ms_sim_proj_dens.RData'))
  
  #array to store
  # index_proj<-array(NA,
  #                   dim=list(length(spp)+length(crabs),3,length(project_yrs),3,n_sur,nrow(samp_df)),
  #                   dimnames=list(c(spp,crabs),c('STRS_mean','STRS_var','CV_sim'),paste0('y',project_yrs),c('sys','rand','sb'),1:n_sur,samp_df$samp_scn))
  index_proj<-array(NA,
                    dim=list(length(spp),3,length(project_yrs),3,n_sur,nrow(samp_df)),
                    dimnames=list(c(spp),c('STRS_mean','STRS_var','CV_sim'),paste0('y',project_yrs),c('sys','rand','sb'),1:n_sur,samp_df$samp_scn))
  
  
  #array to store results  
  # sim_survey <- array(NA,
  #                     dim = c(alloc, length(spp)+2, n_sur, length(unique(project_yrs)),length(samp_df$samp_scn),length(c('sys','rand','sb'))),
  #                     dimnames = list(1:alloc, c('cell','strata',spp), 1:n_sur,unique(project_yrs), samp_df$samp_scn, c('sys','rand','sb')))
  
  
  #loop over sampling design
  for (samp in samp_df$samp_scn)  {
    
    #samp<-'scnbase'
    #start_time_parallel <- Sys.time()
    
    #number of sampling design
    s<-match(samp,samp_df$samp_scn)
    
    #when base sampling other files
    if (grepl('base',samp)) {
      
      #conditions on baseline scenarios
      if (samp == 'scnbase') {
        baseline_strata$locations2<-baseline_strata$locations
      } else if (samp == 'scnbase_bis') {
        baseline_strata$locations2<-baseline_strata$locations[which(baseline_strata$locations$corner=='FALSE'),]
      } 
      
      #sort by strata
      #baseline_strata$strata_areas<-baseline_strata$strata_areas[order(baseline_strata$strata_areas$X1),]
      #baseline_strata$locations2<-baseline_strata$locations2[order(baseline_strata$locations2$stratum),]
      names(baseline_strata$strata_areas)[1]<-'X1'
      names(baseline_strata$locations2)[ncol(baseline_strata$locations2)]<-'stratum'
      
      #area by strata
      strata_areas <- baseline_strata$strata_areas
      
      #strata data
      survey_detail <- data.frame("Stratum" = baseline_strata$strata_areas$X1, #strata
                                  'Nh' = baseline_strata$strata_areas$pct*53464, #number of cells
                                  "nh" = data.frame(table(baseline_strata$locations2$stratum))[,c('Freq')]) #number of sample allocations
      
      #weight of strata for each
      survey_detail$Wh <- survey_detail$Nh / sum(survey_detail$Nh)
      survey_detail$wh <- with(survey_detail, nh/Nh)
      
    } else {
      
      #load optimization results
      load(paste0("./output/ms_optim_allocations_",samp_df[s,'samp_scn'],".RData")) #all
      
      #area
      area_cell<-merge(all$result_list$solution$indices, grid2, by.x='ID',by.y='cell')
      
      #area by strata
      strata_areas <- aggregate(Area_in_survey_km2 ~ X1, 
                                FUN = sum,
                                data = area_cell)
      
      #strata data
      survey_detail <- data.frame("Stratum" = all$samples_strata$strata, #strata
                                  'Nh' = as.integer(table(all$result_list$solution$indices$X1)), #number of cells
                                  "nh" = all$samples_strata$n_samples) #number of sample allocations
      
      #weight of strata for each
      survey_detail$Wh <- survey_detail$Nh / sum(survey_detail$Nh)
      survey_detail$wh <- with(survey_detail, nh/Nh)
      
    }   
    
    sum(strata_areas$Area_in_survey_km2)
    
    #array to store simulated densities/CPUE
    alloc<-ifelse(samp=='scnbase_bis',494,520)
    
    #load survey allocations by sampling design
    load(file = paste0('./output/survey_allocations_',samp,'.RData')) #scn_allocations
    dimnames(scn_allocations)[[3]]<-c('sys','rand','sb')
      
    #loop over n combinations of simulated
    for (n in nums) {
      
      #n<-nums[1]
      
      #year of simulation
      y<-as.character(sur_df[which(sur_df$num==n),'year'])
      #isurvey of simulation
      sur<-sur_df[which(sur_df$num==n),'sur']
      
      #simulated densities of survey and year
      sim_dens2<-simdata[,,y,sim]
      
      #loop over station allocation approac
      for (apr in c('sys','rand','sb')) {
        
        #apr<-'sys'
        
        #print process        
        cat(paste(" #############  ",samp,'- simdata',sim,'- sbt',sbt,'- survey',sur, '- year',y ,'- allocation',apr," #############\n"))
        
        #get densities based on station allocations
        sim_survey<-data.frame(cbind(strata=scn_allocations[scn_allocations[,'sur',apr]==n,c('strata'),apr],
                                     dens=sim_dens2[scn_allocations[scn_allocations[,'sur',apr]==n,c('cell'),apr],]),check.names = FALSE)
        
        sim_survey1<-reshape2::melt(sim_survey,id.vars=c('strata'))
        
        #mean, sum and var by strata and year (variable)
        sim_survey2<-aggregate(x=sim_survey1$value,
                               by=list(strata=sim_survey1$strata,sp=sim_survey1$variable),
                               FUN = function(x) c('mean' = mean(x,na.rm=T), 'sum' = sum(x),'var' = var(x,na.rm=T) ))
        
        #create df
        zzz<-data.frame('strata'=sim_survey2$strata,'sp'=sim_survey2$sp,'mean'=sim_survey2$x[,c('mean')],'var'=sim_survey2$x[,c('var')]) #/length(yy$value)
        zzzz<-merge(zzz,strata_areas,by.x='strata',by.y='X1',all.x=TRUE)
        zzzz<-merge(zzzz,survey_detail,by.x='strata',by.y='Stratum',all.x=TRUE)
        
        #add index strata for sum to compute index (mean strata density * area of strata) kg!
        zzzz$index_strata<-zzzz$mean*zzzz$Area_in_survey_km2
        
        #add strata var 
        zzzz$strs_var<-zzzz$var*(zzzz$Area_in_survey_km2^2)/zzzz$nh #sum(survey_detail$Nh) 
        
        #sum of strata var and mean density across years (kg/km2)
        zzzz1 <- aggregate(zzzz[,c('strs_var','index_strata')], by= list(zzzz$sp),FUN = sum)
        
        #get CV across years
        zzzz1$cv<- sqrt(zzzz1$strs_var) / zzzz1$index_strata
        
        #mean CV 
        mean(zzzz1$cv,na.rm=TRUE)
        
        #get outputs
        STRS_mean <- zzzz1$index_strata
        STRS_var <- zzzz1$strs_var
        CV <- sqrt(STRS_var) / STRS_mean
        
        # ######
        # #CRABS
        # ######
        # 
        # for (c in crabs) {
        #   
        #   #c<-crabs[1]
        #   
        #   if (grepl('PBL',c)) {
        #     cells<-PBL_KC_cells
        #   } else if (grepl('CRB',c)) {
        #     cells<-EBS_C_cells
        #   } else {
        #     cells<-STM_BKC_cells
        #   }
        #   
        #   crab_sp<-crabs_spp[match(c,crabs)]
        #   
        #   #get densities based on station allocations
        #   sim_survey_crabs<-data.frame(cbind(strata=scn_allocations[scn_allocations[,'sur',apr]==n & scn_allocations[,'cell',apr] %in% cells,c('strata'),apr],
        #                                      dens=sim_dens2[scn_allocations[scn_allocations[,'sur',apr]==n & scn_allocations[,'cell',apr] %in% cells,c('cell'),apr],crab_sp]),check.names = FALSE)
        #   names(sim_survey_crabs)[ncol(sim_survey_crabs)]<-crab_sp
        #   
        #   
        #   sim_survey1<-reshape2::melt(sim_survey_crabs,id.vars=c('strata'))
        #   sim_survey1$strata<-'crab'
        #   
        #   #mean, sum and var by strata and year (variable)
        #   sim_survey2<-aggregate(x=sim_survey1$value,
        #                          by=list(strata=sim_survey1$strata,sp=sim_survey1$variable),
        #                          FUN = function(x) c('mean' = mean(x,na.rm=T), 'sum' = sum(x),'var' = var(x,na.rm=T) ))
        #   zzz<-data.frame('strata'=sim_survey2$strata,'sp'=sim_survey2$sp,'mean'=sim_survey2$x[,c('mean')],'var'=sim_survey2$x[,c('var')]) #/length(yy$value)
        #   
        #   #add index strata for sum to compute index (mean strata density * area of strata) kg!
        #   zzz$index_strata<-zzz$mean*areacrab[[c]]
        #   
        #   #add strata var 
        #   zzz$strs_var<-zzz$var*(areacrab[[c]]^2)/nrow(sim_survey_crabs) #sum(survey_detail$Nh) 
        #   
        #   #get CV across years
        #   zzz$cv<- sqrt(zzz$strs_var) / zzz$index_strata
        #   
        #   
        #   #get outputs
        #   STRS_mean <- c(STRS_mean,zzz$index_strata)
        #   STRS_var <- c(STRS_var,zzz$strs_var)
        #   CV <- c(CV,sqrt(zzz$strs_var) / zzz$index_strata)
        #   
        # }
        
        #store outputs
        index_proj[,'STRS_mean',paste0('y',y),apr,sur,samp]<-STRS_mean
        index_proj[,'STRS_var',paste0('y',y),apr,sur,samp]<-STRS_var
        index_proj[,'CV_sim',paste0('y',y),apr,sur,samp]<-CV
        
      }
     }
    }
  save(index_proj, file = paste0('./output/ms_sim_survey_proj/sim',sim_fol,'/SBT',sbt,' index_proj.RData'))  
  
  } 
}

        

############################################################################
############################################################################
############################################################################
#                       EXPERIMENT NUMBER OF SURVEYS
############################################################################
############################################################################
############################################################################

#simulated densities
load(file = paste0('./output/species/ms_sim_dens.RData'))  #sim_dens1

#1year - 1simulated dens - 1 species
dimnames(sim_dens1)
sim_dens2<-sim_dens1[,'Gadus macrocephalus','1982','1']

#get survey designs for one stratification design
#load survey allocations by sampling design
load(file = paste0('./output/survey_allocations_scn3.RData')) #scn_allocations
dimnames(scn_allocations)
scn_allocations1<-data.frame(scn_allocations[,,'rand'])
unique(scn_allocations1$sur) #4000 survey designs


#load optimization results
load(paste0("./output/ms_optim_allocations_scn3.RData")) #all

#area
area_cell<-merge(all$result_list$solution$indices, grid2, by.x='ID',by.y='cell')

#area by strata
strata_areas <- aggregate(Area_in_survey_km2 ~ X1, 
                          FUN = sum,
                          data = area_cell)

#strata data
survey_detail <- data.frame("Stratum" = all$samples_strata$strata, #strata
                            'Nh' = as.integer(table(all$result_list$solution$indices$X1)), #number of cells
                            "nh" = all$samples_strata$n_samples) #number of sample allocations

#weight of strata for each
survey_detail$Wh <- survey_detail$Nh / sum(survey_detail$Nh)
survey_detail$wh <- with(survey_detail, nh/Nh)

cv.all<-c()

#loop over station allocation approac
for (sur in 1:4000) {
  
  #sur<-1
  
  apr<-'rand'
  
  #print process        
  #cat(paste(" #############  ",samp,'- simdata',sim,'- sbt',sbt,'- survey',sur, '- year',y ,'- allocation',apr," #############\n"))
  
  #get densities based on station allocations
  sim_survey<-data.frame(cbind(strata=scn_allocations1[which(scn_allocations1$sur==sur),c('strata')],
                               dens=sim_dens2[scn_allocations1[which(scn_allocations1$sur==sur),c('cell')]]),check.names = FALSE)
  
  sim_survey1<-reshape2::melt(sim_survey,id.vars=c('strata'))
  
  #mean, sum and var by strata and year (variable)
  sim_survey2<-aggregate(x=sim_survey1$value,
                         by=list(strata=sim_survey1$strata,sp=sim_survey1$variable),
                         FUN = function(x) c('mean' = mean(x,na.rm=T), 'sum' = sum(x),'var' = var(x,na.rm=T) ))
  
  #create df
  zzz<-data.frame('strata'=sim_survey2$strata,'sp'=sim_survey2$sp,'mean'=sim_survey2$x[,c('mean')],'var'=sim_survey2$x[,c('var')]) #/length(yy$value)
  zzzz<-merge(zzz,strata_areas,by.x='strata',by.y='X1',all.x=TRUE)
  zzzz<-merge(zzzz,survey_detail,by.x='strata',by.y='Stratum',all.x=TRUE)
  
  #add index strata for sum to compute index (mean strata density * area of strata) kg!
  zzzz$index_strata<-zzzz$mean*zzzz$Area_in_survey_km2
  
  #add strata var 
  zzzz$strs_var<-zzzz$var*(zzzz$Area_in_survey_km2^2)/zzzz$nh #sum(survey_detail$Nh) 
  
  #sum of strata var and mean density across years (kg/km2)
  zzzz1 <- aggregate(zzzz[,c('strs_var','index_strata')], by= list(zzzz$sp),FUN = sum)
  
  #get CV across years
  zzzz1$cv<- sqrt(zzzz1$strs_var) / zzzz1$index_strata
  
  #mean CV 
  mean(zzzz1$cv,na.rm=TRUE)
  
  #get outputs
  STRS_mean <- zzzz1$index_strata
  STRS_var <- zzzz1$strs_var
  CV <- sqrt(STRS_var) / STRS_mean
  
  cv.all<-c(cv.all,CV)

}

######

cv.all<-data.frame(cv.all)
names(cv.all)<-'cv'

cv.all$dummy<-1

set.seed(6)

p<-
ggplot() +
  geom_hline(yintercept = mean(cv.all[sample(nrow(cv.all), 100), 'cv']),linetype='dashed')+
  geom_boxplot(data=cv.all[sample(nrow(cv.all), 100), ], aes(y=cv, x=factor('100', levels=c('100', '200', '500', '1000', '4000')))) +
  geom_boxplot(data=cv.all[sample(nrow(cv.all), 200), ], aes(y=cv, x=factor('200', levels=c('100', '200', '500', '1000', '4000')))) +
  geom_boxplot(data=cv.all[sample(nrow(cv.all), 500), ], aes(y=cv, x=factor('500', levels=c('100', '200', '500', '1000', '4000')))) +
  geom_boxplot(data=cv.all[sample(nrow(cv.all), 1000), ], aes(y=cv, x=factor('1000', levels=c('100', '200', '500', '1000', '4000')))) +
  geom_boxplot(data=cv.all[sample(nrow(cv.all), 4000), ], aes(y=cv, x=factor('4000', levels=c('100', '200', '500', '1000', '4000')))) +
  labs(x='number of surveys',y=expression(widehat(CV)))+
  theme_bw()

#save index plot
ragg::agg_png(paste0('./figures/number_simulated surveys.tiff'), width = 7, height = 4, units = "in", res = 300)
p
dev.off()















