####################################################################
####################################################################
##
##    Run sampling optimization based on predicted densities from VAST OM EBS+NBS
##    and calculate stratification boundaries and sample allocations for each sampling design
##
##    by best stratification, we mean the stratification that ensures the minimum sample cost, 
##    sufficient to satisfy precision constraints set on the accuracy of the estimates of the survey target variables Y’s
##    constraints expressed as maximum allowable coefficients of variation in the different domains of interest
##
##    https://cran.r-project.org/web/packages/SamplingStrata/vignettes/SamplingStrata.html
##    (using devtools::install_github(repo = "zoyafuso-NOAA/SamplingStrata"))
##    Daniel Vilas (daniel.vilas@noaa.gov/dvilasg@uw.edu)
##    Lewis Barnett
##    Zack Oyafuso (some code borrowed from his previous analysis in the GOA)
##
####################################################################
####################################################################

#clear all objects
rm(list = ls(all.names = TRUE)) 
#free up memrory and report the memory usage
gc() 

#setseed
set.seed(6)

#libraries from cran to call or install/load
pack_cran<-c("splines",'SamplingStrata','wesanderson','dplyr','sp',
             'sf','maptools','rgeos','scales','raster',
             'rnaturalearth','grid','ggplot2','spatstat','ragg',
             'ggthemes','cowplot')

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

#parallel
#cl <- parallel::makeCluster(parallel::detectCores() - 2)
#doParallel::registerDoParallel(cl)

#setwd - depends on computer using
#out_dir<-'C:/Users/Daniel.Vilas/Work/Adapting Monitoring to a Changing Seascape/' #NOAA laptop  
out_dir<-'/Users/daniel/Work/UW-NOAA/Adapting Monitoring to a Changing Seascape/' #mac
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

spp_name<-data.frame('spp'=spp,
                     'common'=spp1) 


#df spp, number and target variables
df_spp<-data.frame('spp'=spp,
                   'n'=c(1:length(spp)),
                   'Y'=paste0('Y',c(1:length(spp))))

#number sp
n_spp<-length(spp)

###################################
# LOAD GRID EBS (remember to keep the same order as in fit_model if multiple grids)
###################################

load(file = './data processed/grid_EBS_NBS.RData') #grid.ebs_year$region
grid_ebs<-subset(grid.ebs_year,region=='EBSslope' & Year=='1982' & DepthGEBCO<=400)
slp_cells<-rownames(grid_ebs)

#load slope grid
load('./extrapolation grids/bering_sea_slope_grid.rda')
dim(bering_sea_slope_grid)
names(bering_sea_slope_grid)[4]<-'Stratum'
bering_sea_slope_grid$Stratum<-999
#gridslope<-data.frame(bering_sea_slope_grid,region='SBS')

#load EBS+NBS grid
load('./extrapolation grids/northern_bering_sea_grid.rda')
load('./extrapolation grids/eastern_bering_sea_grid.rda')
grid<-as.data.frame(rbind(data.frame(northern_bering_sea_grid,region='NBS'),
                          data.frame(eastern_bering_sea_grid,region='EBS'),
                          data.frame(bering_sea_slope_grid,region='SBS')))
ncell_ebsnbs<-nrow(rbind(data.frame(northern_bering_sea_grid,region='NBS'),
                         data.frame(eastern_bering_sea_grid,region='EBS')))
ncell_nbs<-nrow(rbind(data.frame(northern_bering_sea_grid,region='NBS')))
grid$cell<-1:nrow(grid)
grid$cell<-as.numeric(grid$cell)

#add col and row number
x1<-grid[,c('Lon','Lat','cell')]
names(x1)<-c('x','y','z')
coordinates(x1)=~x + y
#crs(x1)<-c(crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
proj4string(x1)<-CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
x2<-spTransform(x1,'+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')
x3<-data.frame(x2)
# x3$x<-as.integer(x3$coords.x1)
# x3$y<-as.integer(x3$coords.x2)
lon<-sort(unique(x3$x),decreasing = FALSE) #1556
lat<-sort(unique(x3$y),decreasing = TRUE) #1507
lons<-data.frame(x=lon,col=1:length(lon))
lats<-data.frame(y=lat,row=1:length(lat))
x4<-merge(x3,lons,by='x',all.x=TRUE)
x5<-merge(x4,lats,by='y',all.x=TRUE)
x5<-x5[,c('y','x','z','optional','col','row'),]
colnames(x5)<-c('Lat','Lon','cell','optional','col','row')
grid<-x5[,c('Lat','Lon','cell','col','row')]

# Define plot extent (through trial end error) units km (for plotting purposes)
panel_extent <- data.frame(x = c(-1716559.21, -77636.05), #x = c(-1326559.21, -87636.05),
                           y = c(483099.5, 2194909.7)) #y = c(533099.5, 1894909.7))


#####################################
# Polygon regions shapefiles (EBS, NBS and slope)
#####################################

#name shapefiles 
shfiles<-c('EBSshelfThorson','NBSThorson','EBSslopeThorson')

#loop over shapefiles
for (i in shfiles) {
  
  #shapefile EBS
  sh<-rgdal::readOGR(dsn='./shapefiles/',layer = i)
  
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
bs_sh1<-raster::union(EBSshelf_sh,NBS_sh)
bs_sh<-raster::union(bs_sh1,EBSslope_sh)

###################################
# FXNs - extracted from ZO code
###################################

calc_expected_CV <- function (strata) {
  
  n_spp <- length(grep(x = names(strata), pattern = "M"))
  n_h <- strata$Allocation
  N_h <- strata$Population
  
  cv <- vector(length = n_spp)
  names(cv) <- paste0("Y", 1:n_spp)
  
  for (ispp in 1:n_spp) {
    S_h <- strata[, paste0("S", ispp)] #SD from 
    M_h <- strata[, paste0("M", ispp)] #mean from 
    
    Y_h <- N_h * M_h
    Var_h <- (N_h^2) * (1 - n_h/N_h) * ((S_h^2)/n_h)
    CV <- sqrt(sum(Var_h))/sum(Y_h)
    
    cv[ispp] <- CV
  }
  
  cv <- round(cv, 3)
  return(cv)
}

###################################
# FIND SLOPE CELLS DEEPER than 400m
###################################

load(file = './data processed/grid_EBS_NBS.RData') #grid.ebs_year$region
grid_slp<-subset(grid.ebs_year,region=='EBSslope' & Year=='1982')
dim(grid_slp)
dim(grid_slp[which(grid_slp$DepthGEBCO<=400),])
ok_slp_cells<-as.numeric(row.names(grid_slp)[which(grid_slp$DepthGEBCO<=400)])
rem_slp_cells<-as.numeric(row.names(grid_slp)[which(grid_slp$DepthGEBCO>400)])

###################################
# Get SAMPLES DENSITY for the EBS and find for BSS and NBS
###################################

# EBS
ebs_samples <- 376
ebs_area <- sum(eastern_bering_sea_grid[,'Area_in_survey_km2'])
ebs_dens <- ebs_samples / ebs_area   # samples per km2

# SBS
grid_slp1 <- subset(grid_slp, DepthGEBCO <= 400)
sbs_area <- sum(grid_slp1[,'Area_in_survey_km2'])
sbs_samples <- sbs_area * ebs_dens

# NBS
nbs_area <- sum(northern_bering_sea_grid[,'Area_in_survey_km2'])
nbs_samples <- nbs_area * ebs_dens

# Summary
region_samples <- data.frame(
  region = c("EBS", "SBS", "NBS"),
  Area_km2 = c(ebs_area, sbs_area, nbs_area),
  samples = c(ebs_samples, sbs_samples, nbs_samples)
)

region_samples
region_samples$samples <- round(region_samples$samples)
region_samples

n_EBS <- region_samples$samples[region_samples$region == "EBS"]
n_SBS <- region_samples$samples[region_samples$region == "SBS"]
n_NBS <- region_samples$samples[region_samples$region == "NBS"]
# sample counts by combination
n_samples_vec <- c(
  "EBS"           = n_EBS,
  "EBS+NBS"       = n_EBS + n_NBS,
  "EBS+SBS"       = n_EBS + n_SBS,
  "EBS+NBS+SBS"   = n_EBS + n_NBS + n_SBS
)

###################################
# Sampling designs
###################################

#sampling scenarios
samp_df<-expand.grid(type=c('static','dynamic'),#c('all','cold','warm'),
                     region=c('EBS','EBS+NBS','EBS+SBS','EBS+NBS+SBS'),
                     strat_var=c('Depth'), #,'varTemp_forced','Depth_forced' #LonE and combinations
                     target_var=c('sumDensity'), #,'sqsumDensity'
                     n_samples=NA, #c(300,500) 520 (EBS+NBS+CRAB);26 (CRAB); 350 (EBS-CRAB); 494 (NBS-CRAB)
                     n_strata=c(10),
                     domain=1) #c(5,10,15)


# assign n_samples based on region
samp_df$n_samples <- n_samples_vec[samp_df$region]
samp_df


#samples slope to add dummy approach
samp_slope <- subset(samp_df, grepl("SBS", region))
samp_slope$strat_var<-paste0(samp_slope$strat_var,'_dummy')

#add with dummy approach
samp_df<-rbind(samp_df,samp_slope)

#add scenario number
samp_df$samp_scn<-paste0(paste0('scn',1:nrow(samp_df)))

#save table that relate survey design (here scn) to variables
save(samp_df,file='./tables/samp_df_dens.RData')

#########################
# loop over optimized sampling designs
#########################

#s<-5 cannot find a solution

#loop through sampling designs
for (s in c(1:nrow(samp_df))) { #nrow(samp_df)
  
  #s<- 2
  
  #print scenario to check progress
  cat(paste("\n #############  Sampling Scenario", samp_df[s,"samp_scn"], " #############\n"))
  
  #domain
  dom<-samp_df[s,'domain']
  idom<-samp_df[s,'idomain']
  
  ###############
  # load ms data and settings
  ###############

  if (samp_df[s,'type']=='static') {
    #load multispecies data
    load(paste0('./output slope/multisp_optimization_static_data_ebsnbs_slope_st.RData')) #df
    df<-data.frame(df[,c(1:9)],
                   regime='all',
                   df[,c(10:ncol(df))],check.names = FALSE)
    regime<-c('all')
  } else {
    #load multispecies data
    load(paste0('./output slope/multisp_optimization_static_data_ebsnbs_slope_dyn.RData')) #df
    regime<-c('cold','warm')
  }
  
  #add dummy
  df$LonE<-ifelse(df$cell>53464,9999999999,0)
  names(df)[9]<-'dummy'
  
    #if ebs
  if (samp_df[s,'region']=='EBS') {
    df1<-
      df[which(df$cell<=53464 & df$cell>=15180+1),]
    #if ebs and nbs
  } else if (samp_df[s,'region']=='EBS+NBS') {
    df1<-
      df[which(df$cell<=53464),]
    #if ebs and slope
  } else if (samp_df[s,'region']=='EBS+SBS') {
    df1<-
      df[which(df$cell>=15180+1),]
    #if all region
  } else {
    df1<-df
  }
  
  #target variables
  tar_var<-paste0(rep(df_spp$Y,each=2),c('','_SQ_SUM'))
  names(df1)[((ncol(df1)-length(tar_var))+1):ncol(df1)]<-tar_var
  ispp<-n_spp

  #removed cells because of depth - include with negative depth and slope>400m
  ok_cells <- df1[which(df1$include == TRUE ), 'cell'] #cells with negative depth
  length(ok_cells)
  ok_cells<-setdiff(ok_cells,rem_slp_cells) #cells in the slope deeper than 400m
  length(ok_cells)
  #56437-(3041-1283)
  
  #years
  n_years<-length(2002:2016)
    
    for (r in regime) {
  
      #r<-regime[1]
      #summary(static_df1)
      #subset cells with appropiate depth
      static_df1<-subset(df1,cell %in% ok_cells)
      #dim(static_df1)
      #dim(df1)
      
      #n cells
      n_cells<-nrow(static_df1)
      

      #filter by regime if dynamic
      #if (length(regime)==2) {
      static_df1<-subset(static_df1,regime==r)
      #}
      
      #domain_input
      domain_input<-rep(1, nrow(static_df1))
      
      #static_df1[!complete.cases(static_df1), ]
      #filter 
      
      #static_df1<-static_df1[,colSums(static_df1[,11:ncol(static_df1)]) != 0]      
      
      
      # Select columns 1 to 10
      selected_columns <- static_df1[, 1:10]
      
      # Identify columns from 11 to ncol(df1) with non-zero column sums
      non_zero_columns <- static_df1[, 11:ncol(static_df1)][, colSums(static_df1[, 11:ncol(static_df1)]) != 0]
      
      # Combine the selected columns
      static_df1 <- cbind(selected_columns, non_zero_columns)
      
      # Extract the two parts of the vector
      ys <- paste0('Y',1:(length(colnames(static_df1)[11:(ncol(static_df1))])/2))
      sq_sums <- paste0('Y',1:(length(colnames(static_df1)[11:(ncol(static_df1))])/2),c('_SQ_SUM'))
      
      # Interleave the two parts
      colnames(static_df1)[11:(ncol(static_df1))]<- c(rbind(ys, sq_sums))
      tar_var<-colnames(static_df1)[11:(ncol(static_df1))]
      #names(df)[((ncol(df)-length(tar_var))+1):ncol(df)]<-tar_var
      ispp<-n_spp<-length(tar_var)/2
  
      #get stratification factor values
      if(grepl("_", samp_df[s,'strat_var'])){
        stratum_var_input<-data.frame(X1 = static_df1[,paste0(sub("\\_.*", "", samp_df[s,'strat_var']))],
                                      X2 = static_df1$dummy)
      } else {
        stratum_var_input<-data.frame(X1 = static_df1[,paste0(sub("\\_.*", "", samp_df[s,'strat_var']))])
      }
      
      #target variables
      target_var_input<-static_df1[,tar_var]
      n_samples<-srs_n<-samp_df[s,'n_samples']
      
      #create df
      frame <- data.frame(domainvalue = domain_input, #domain
                          id = static_df1$cell, #id as cells
                          stratum_var_input, #Stratification variables
                          WEIGHT=n_years, #weight for spp depending on the presence of years
                          target_var_input) #target variables 
      
      # #correct years
      #         frame$WEIGHT<-ifelse(frame$id %in% 1:15180,6,
      #                       ifelse(frame$id %in% nbs_cells,5, 40))
      
      
      ###################################
      # Simple random sampling design CV constraints
      ###################################
      
      #Initiate CVs to be those calculated under simple random sampling (SRS)    
      #build strata frame (if 2 factors stratification or 1 factors) 
      if (grepl('_',samp_df[s,'strat_var'])) {
        #stratification variables 
        srs_stats <- buildStrataDF(dataset = cbind( frame[, -grep(x = names(frame), pattern = "X")],X1 = 1,X2=1))
      } else {
        srs_stats <- buildStrataDF(dataset = cbind( frame[, -grep(x = names(frame), pattern = "X")],X1 = 1))
      }
      
      ## SRS statistics
      srs_var <- srs_stats[, paste0("S", 1:ispp)]^2 * (1 -  srs_n/ n_cells) / srs_n
      srs_cv <- sqrt(srs_var) / srs_stats[, paste0("M", 1:ispp)]
      
      #CV
      cv_df <- list()
      cv_df[["DOM"]] <-c(1:dom)
      for (iispp in 1:ispp) cv_df[[paste0("CV", iispp)]] <- as.numeric(srs_cv[,iispp])
      cv_df[["domainvalue"]] <-c(1:dom)
      cv_df <- as.data.frame(cv_df)
      cv_df[is.na(cv_df)]<-1
      
      #store Cv
      cv_initial<-cv_df
      
      #number of samples ###520 in 2022 (NBS and EBSshelf) #376 in 2018 (EBSshelf)
      n_samples <- samp_df[s,'n_samples']
      
      #number of stratas 
      no_strata<-samp_df[s,'n_strata']
      
      #number of samples
      srs_n <- as.numeric(n_samples * table(frame$domainvalue) / n_cells)
      
      ###################################
      # Run optimization
      ###################################
      
      #for plot 
      #load('./data processed/grid_EBS_NBS.RData')
      #gridi<-
        #unique(grid.ebs_year[,c('Lat','Lon','DepthGEBCO')])
      #gridi1<-subset(gridi,DepthGEBCO <= 400)
      
      #count
      count<-0
      
      #while loop until the desired number of strata  
      flag<-TRUE
      while (flag) {
        
        count<-count+1
        
        #print state of spp[1] to get an approximation of the state
        cat(paste("\n #############   OPTIMIZING STRATA - CV of ",spp[1],' ', cv_df[,2],"  #############\n"))
        cat(paste("\n #############   COUNT", count ,"  #############\n"))
        
        #run optimization
        solution <- optimStrata(method = "continuous", #continous variables
                                errors = cv_df,  #precision level - maximum allowable coefficient of variation set by the simple random sampling 
                                framesamp = frame, #df of input variables 
                                iter = 150,#150, #30, #300 #maximum number of iterations
                                pops = 50,#50,#10, #100  #dimension of each generations
                                elitism_rate = 0.1, #0.1
                                mut_chance = 1 / (no_strata[1] + 1), #mutation chance
                                nStrata = c(no_strata), #maximum strata
                                showPlot = TRUE, #FALSE
                                writeFiles = FALSE)
        
        #aggregate(solution$indices$ID ~ solution$indices$X1,FUN=length)
        
        #plot strata
        solution$framenew$strata<-paste0(solution$framenew$DOMAINVALUE,'_',solution$framenew$STRATO)
        print(
        ggplot()+
          geom_point(data=cbind(static_df1[,c('Lat','Lon')],solution$framenew),aes(x=Lon,y=Lat,color=strata))
        )
        
        #solution$aggr_strata
        #flag to keep the loop
        flag<-ifelse(nrow(solution$aggr_strata)!=samp_df$n_strata[s]*dom,TRUE,FALSE)
        
         #if condition reduce or increase CV to achieve the objective
         if (nrow(solution$aggr_strata)<samp_df$n_strata[s]*dom) {
           cv_df[,c(2:(ispp+1))]<-cv_df[,c(2:(ispp+1))]-(cv_df[,c(2:(ispp+1))]*0.001) #reduce 0.1% CV ###0.001
         } else if (nrow(solution$aggr_strata)>samp_df$n_strata[s]*dom){
          cv_df[,c(2:(ispp+1))]<-cv_df[,c(2:(ispp+1))]+cv_df[,c(2:(ispp+1))]*0.10} #increase 1%
      }
  
  
  
  
      
      ###################################
      # Store solutions from optimizations
      ###################################
      
      #store strata CV
      cv_strata_final<-cv_df
      
      solution$aggr_strata$STRATO <- as.integer(solution$aggr_strata$STRATO)
      solution$aggr_strata <- 
        solution$aggr_strata[order(solution$aggr_strata$DOM1,
                                   solution$aggr_strata$STRATO), ]
      
      
      # plotStrata2d(solution$framenew,
      #               solution$aggr_strata,
      #              domain = 1,
      #              vars=c('X1','X2'),
      #              labels = c('Depth','Lat'))
      
      
      sum_stats <- SamplingStrata::summaryStrata(solution$framenew,
                                                 solution$aggr_strata,
                                                 progress=FALSE)
      sum_stats$stratum_id <- 1:nrow(sum_stats)
      sum_stats$wh <- sum_stats$Allocation / sum_stats$Population
      sum_stats$Wh <- sum_stats$Population / nrow(frame)
      sum_stats <- cbind(sum_stats,
                         subset(x = solution$aggr_strata,
                                select = -c(STRATO, N, COST, CENS, DOM1, X1)))
      
      plot_solution <- solution$indices
      
      #save optimization stats  
      sum_stats <- summaryStrata(solution$framenew,
                                 solution$aggr_strata,
                                 progress=FALSE)
      sum_stats$stratum_id <- 1:nrow(sum_stats)
      sum_stats$Population <- sum_stats$Population / n_years
      sum_stats$wh <- sum_stats$Allocation / sum_stats$Population
      sum_stats$Wh <- sum_stats$Population / n_cells
      sum_stats <- cbind(sum_stats,
                         subset(x = solution$aggr_strata,
                                select = -c(STRATO, N, COST, CENS, DOM1, X1)))
      
      #add scn and sp
      sum_stats$samp_scn<-samp_df[s,'samp_scn']
      
      #if one stratifying factor add columns  
      if (!grepl('_',samp_df[s,'strat_var'])) {
        
        sum_stats<-data.frame(sum_stats[,c("Domain","Stratum","Population","Allocation","SamplingRate","Lower_X1","Upper_X1")],
                              "Lower_X2"=NA,"Upper_X2"=NA,
                              sum_stats[,c("stratum_id","wh","Wh","M1",'S1',"SOLUZ","samp_scn")])
      }
  
      #store results
      result_list <- list(solution = solution,
                          sum_stats = sum_stats,
                          cvs = as.numeric(calc_expected_CV(sum_stats)),
                          n = sum(sum_stats$Allocation),
                          sol_by_cell = plot_solution)
      
      #######################
      ##   7) Single-Species Optimization ----
      ##   Calculate single-species CV subject to the initial stratification
      #######################
  
      
      ss_sample_allocations <- expand.grid(n = n_samples, species = spp)
      
      for (iispp in 1:ispp) {
        #iispp<-17
        
        temp_n <- result_list$n
        
        ## Subset density data for species ispp
        ss_df <- subset(x = frame, 
                        select = c("domainvalue", "id", "WEIGHT", "X1", #"X2", 
                                   paste0("Y", iispp), paste0("Y", iispp, "_SQ_SUM")))
        names(ss_df)[grep(x = names(ss_df), pattern = "Y")] <- c("Y1", "Y1_SQ_SUM")
        
        ## Create CV inputs to the Bethel algorithm; initialize at SRS CV
          error_df <- data.frame("DOM" = c(1:dom),
                                'CV1'=as.numeric(srs_cv[,iispp]),
                                 "domainvalue"  = c(1:dom))
          #names(error_df)[2] <- "CV1"
          
          ## subset stratum stats for the species of interest as inputs to the 
          ## Bethel algorithm
          temp_stratif <- 
              solution$aggr_strata[, c("STRATO", "N", 
                                       paste0("M", iispp), paste0("S", iispp), 
                                       "COST", "CENS", "DOM1", "X1" ,"SOLUZ")]
          
          if (dom==2) {
            temp_stratif$DOM2<-temp_stratif$DOM1
          }
          
          
          temp_stratif$N <- temp_stratif$N / n_years
          #temp_stratif$DOM1 <- 1
          names(temp_stratif)[3:4] <- paste0(c("M", "S"), 1)
          
          # checkInput(errors = checkInput(errors = error_df, 
          #                                strata = temp_stratif, 
          #                                sampframe = ss_df))
          #> 
          
          error_df[is.na(error_df)]<-1
          
          ## run bethel at the SRS CV
          temp_bethel <- SamplingStrata::bethel(
            errors = error_df,
            stratif = temp_stratif, 
            realAllocation = T, 
            printa = T)
          
          ## Save the current n and cv constraint
          temp_n <- sum(ceiling(temp_bethel))
          updated_cv_constraint <-ifelse(dom==2,
                                         as.numeric(attributes(temp_bethel)$outcv[c(1,4), "PLANNED CV "]),
                                         as.numeric(attributes(temp_bethel)$outcv[, "PLANNED CV "]))
          
            #as.numeric(attributes(temp_bethel)$outcv[c(1,4), "PLANNED CV "])
          
          ## modify CV, rerun bethel, save temp_n, run until temp_n == n_samples
          while (temp_n != n_samples){
            over_under <- temp_n > n_samples
            CV_adj <- ifelse(over_under == TRUE, 
                             yes = 1.01,
                             no = 0.999)
            
            updated_cv_constraint <- updated_cv_constraint * CV_adj
            
            error_df[, "CV1"] <- updated_cv_constraint
            
            temp_bethel <- SamplingStrata::bethel(stratif = temp_stratif,
                                                  errors = error_df, 
                                                  printa = TRUE)
            
            temp_n <- sum(as.numeric(temp_bethel))
            updated_cv_constraint <- ifelse(dom==2,
                                            as.numeric(attributes(temp_bethel)$outcv[c(1,4), "PLANNED CV "]),
                                            as.numeric(attributes(temp_bethel)$outcv[, "PLANNED CV "]))
            print(paste0("n = ", temp_n, ", ", updated_cv_constraint) )
          }
          
          ## Save the CV and station allocations that corresponds to n_samples
  
            temp_idx <- ss_sample_allocations$n == n_samples & 
              ss_sample_allocations$species == spp[iispp]
          
          ss_sample_allocations[temp_idx, paste0('CV',1:dom)] <- ifelse(dom==2,
                                                                        as.numeric(attributes(temp_bethel)$outcv[c(1,4), "ACTUAL CV"]),
                                                                        as.numeric(attributes(temp_bethel)$outcv[, "ACTUAL CV"]))
          
          ss_sample_allocations[temp_idx, paste0("Str_", 1:length(temp_bethel))] <- 
            as.integer(temp_bethel)
          
        
      }
      
      ######################
      ##   8) Adjust MS solution ----
      ##   Optimize allocation across a range of sample sizes, given the original
      ##   stratification.
      #####################
      
      ms_sample_allocations <- expand.grid(n = rep(n_samples,length(unique(domain_input))))
      
      ## Subset lower limits of CVs from the ss cvs
      ss_cvs <- subset(ss_sample_allocations, n == n_samples)[,paste0('CV',1:dom)]
      ss_cvs[is.na(ss_cvs)]<-1  
      
      ## CV dataframe input to Bethel algorithm
      error_df <-  data.frame("DOM" = c(1:dom),
                              srs_cv,
                              "domainvalue"  = c(1:dom))
      
        
        names(error_df)[2:(1 + n_spp)] <- paste0("CV", 1:n_spp)
        
      
        ## Stratum statistics input to the Bethel algorithm
        temp_stratif <- solution$aggr_strata
        temp_stratif$N <- temp_stratif$N / n_years
        
        #if (dom==1) {
          
          temp_stratif$DOM1 <- c(rep(1,nrow(temp_stratif)))
          
        # } else if (dom==2) {
        #   
        #   temp_stratif$DOM1 <- c(rep(1,nrow(temp_stratif)/2),rep(2,nrow(temp_stratif)/2))
        #   temp_stratif$DOM2<-temp_stratif$DOM1
        #   
        # }
        
        
        error_df[is.na(error_df)]<-1
        
        ## Run Bethel algorithm and save current n and cv constraints
        temp_bethel <- SamplingStrata::bethel(
          errors = error_df,
          stratif = temp_stratif, 
          realAllocation = T, 
          printa = T)
        
        temp_n <- sum(ceiling(temp_bethel))
        
        # if (dom==2) {
        #   
        #   updated_cv_constraint <-  as.numeric(attributes(temp_bethel)$outcv[c(1:n_spp,((n_spp*3)+1):(n_spp*4)), "PLANNED CV "])
        #                                   
        # } else if (dom==1) {
          
          updated_cv_constraint <-  as.numeric(attributes(temp_bethel)$outcv[c(1:n_spp), "PLANNED CV "])
          
        #}
        
  
        ## Rerun Bethel algorithm, modifying the CVs relative to the distances
        ## between the SRS and SS CVs given n_samples stations. First we calculate 
        ## CVs calculated under SRS for each species with n_samples stations
  
          temp_srs_var <- srs_stats[, paste0("S", 1:n_spp)]^2 * (1 - n_samples / n_cells) / n_samples
          temp_srs_cv <- sqrt(temp_srs_var) / srs_stats[, paste0("M", 1:n_spp)]
        
  
        
        
        
        while (temp_n != n_samples) {
          over_under <- temp_n > n_samples
          
          ## If the current n is < n_samples, decrease the CV by a small amount
          ## relative to the distance between the current CV and the SS CV
          if (over_under == FALSE) {
            updated_cv_constraint <- updated_cv_constraint  - updated_cv_constraint * 0.001
          }
          
          ## If the current n is > n_samples, increase the CV by a small amount
          ## relative to the distance between the current CV and the SRS CV
          if(over_under == TRUE) {
            updated_cv_constraint <- updated_cv_constraint  + updated_cv_constraint * 0.01
          }
          
          ## Update the CV dataframe input with the updated_cv_constraint
  
            error_df[, paste0("CV", 1:n_spp)] <- updated_cv_constraint
          
          
        
          ## Rerun Bethel algorithm
          temp_bethel <- SamplingStrata::bethel(stratif = temp_stratif,
                                                errors = error_df, 
                                                printa = TRUE)
          
          temp_n <- sum(as.numeric(temp_bethel))
          
          ## Save sample size and CV constraint
          if (dom==2) {
            
            updated_cv_constraint <-  as.numeric(attributes(temp_bethel)$outcv[c(1:n_spp,((n_spp*3)+1):(n_spp*4)), "PLANNED CV "])
            
          } else if (dom==1) {
            
            updated_cv_constraint <-  as.numeric(attributes(temp_bethel)$outcv[c(1:n_spp), "PLANNED CV "])
            
          }
          
          
          ## Print out result to console
          print(paste0("n = ", temp_n) )
        }
        
        ## Save optimized CV 
        #temp_idx <- ms_sample_allocations$n == n_samples
  
          ms_sample_allocations[1, paste0("CV", 1:n_spp)] <- 
            as.numeric(attributes(temp_bethel)$outcv[1:n_spp, "ACTUAL CV"])
        
          # if (dom==2) {
          #   ms_sample_allocations[2, paste0("CV", 1:n_spp)] <- 
          #     as.numeric(attributes(temp_bethel)$outcv[c(((n_spp*3)+1):(n_spp*4)), "ACTUAL CV"])
          # 
          # }
          # 
          # if (dom==1) {
            
            ms_sample_allocations[1, paste0("Str_", 1:(length(temp_bethel)))] <- 
              as.integer(temp_bethel)[1:(length(temp_bethel))]
            
          # } else if (dom==2) {
          #   
          #   ms_sample_allocations[1, paste0("Str_", 1:(length(temp_bethel)/2))] <- 
          #     as.integer(temp_bethel)[1:(length(temp_bethel)/2)]
          #   ms_sample_allocations[2, paste0("Str_", 1:(length(temp_bethel)/2))] <- 
          #     as.integer(temp_bethel)[((length(temp_bethel)/2)+1):length(temp_bethel)]
          #   
          # }
    

        #store number samples per strata
      samples_strata<-as.integer(temp_bethel)
  
      #store bethel CV
      cv_bethel_final<-error_df
      
      #cv
      cv_temp <- rbind(CV_random=cv_initial, #max
                            CV_strata=cv_strata_final, #min to achieve strata
                            CV_bethel=cv_bethel_final) #min to achieve naximum sample
      
      #results list to save
      all<-list(result_list=result_list,
                ss_sample_allocations=ss_sample_allocations,
                ms_sample_allocations=ms_sample_allocations,
                samples_strata=data.frame(strata=1:no_strata,n_samples=samples_strata),
                cv=cv_temp)
      
      #strata to plot
      dd<-all$result_list$solution$framenew
      #all$ms_sample_allocations
      effort<-all$ms_sample_allocations[,colnames(all$ms_sample_allocations)[grepl(pattern = 'Str',colnames(all$ms_sample_allocations))]]
      effort1<-reshape2::melt(effort)
      names(effort1)<-c('STRATO','effort')
      effort1$STRATO<-gsub('Str_','',effort1$STRATO)
      dd1<-merge(dd[,c('DOMAINVALUE','STRATO','ID')],grid,by.x='ID',by.y='cell')
      dd2<-merge(dd1,effort1,by='STRATO')
      #dd2$strata<-paste0(dd2$DOMAINVALUE,'_',dd2$STRATO)
      dd2$strata<-paste0(dd2$STRATO)
      dd2$strata<-factor(dd2$strata,levels=c(1:10))
      
      #save list
        save(all,
             file = paste0("./output slope/ms_optim_allocations_ebsnbs_slope_",samp_df[s,'samp_scn'],'_',r,"dens.RData"))
        
        #strata to plot
        dd<-all$result_list$solution$framenew
        #all$ms_sample_allocations
        effort<-all$ms_sample_allocations[,colnames(all$ms_sample_allocations)[grepl(pattern = 'Str',colnames(all$ms_sample_allocations))]]
        effort1<-reshape2::melt(effort)
        names(effort1)<-c('STRATO','effort')
        effort1$STRATO<-gsub('Str_','',effort1$STRATO)
        
        dd1<-merge(dd[,c('DOMAINVALUE','STRATO','ID')],grid,by.x='ID',by.y='cell')
        dd2<-merge(dd1,effort1,by='STRATO')
        dd2$strata<-dd2$STRATO
        
        all_effort<- aggregate(ID ~ X1,all$result_list$sol_by_cell,FUN=length)
        names(all_effort)<-c('strata','effort_area')
        #all_effort$strata<-paste0('1_',all_effort$strata)
        dd2<-merge(dd2,all_effort,by=('strata'))
        dd2$effort_area<-dd2$effort_area/dd2$effort
        dd2$strata<-factor(dd2$strata,levels=c(1:10))
        
        #save plot
        #ragg::agg_png(paste0('./figures slope/',"ms_optim_allocations_ebsnbs_slope_",samp_df[s,'samp_scn'],'.png'),  width = 7, height = 7, units = "in", res = 300)
        #print(
        p1<-
          ggplot()+
          geom_point(data=dd2,aes(x=Lon,y=Lat,color=strata),size=0.1)+
          geom_polygon(data=NBS_sh,aes(x=long,y=lat,group=group),fill=NA,col='black')+
          geom_polygon(data=EBSshelf_sh,aes(x=long,y=lat,group=group),fill=NA,col='black')+
          geom_polygon(data=EBSslope_sh,aes(x=long,y=lat,group=group),fill=NA,col='black')+
          theme_minimal()+
          theme(axis.title = element_blank(),axis.text = element_blank(),panel.grid = element_blank(),
                legend.key.size = unit(2, 'lines'),legend.text = element_text(size=12),legend.title = element_text(size=14))+        
          scale_color_tableau()+
          guides(color = guide_legend(override.aes = list(size = 4)))
          
        #dev.off()
        
        #palette
        pal <- wes_palette("Zissou1", 1000, type = "continuous")      #save plot
        
        #ragg::agg_png(paste0('./figures slope/',"ms_optim_allocations_ebsnbs_slope_",samp_df[s,'samp_scn'],'_effort.png'),  width = 7, height = 7, units = "in", res = 300)
        #print(
        p2<-
          ggplot()+
          geom_point(data=dd2,aes(x=Lon,y=Lat,color=effort_area),size=0.1)+
          geom_polygon(data=NBS_sh,aes(x=long,y=lat,group=group),fill=NA,col='black')+
          geom_polygon(data=EBSshelf_sh,aes(x=long,y=lat,group=group),fill=NA,col='black')+
          geom_polygon(data=EBSslope_sh,aes(x=long,y=lat,group=group),fill=NA,col='black')+
          #ggtitle(samp_df[s,'strat_var'])+
          theme_minimal()+
          theme(axis.title = element_blank(),axis.text = element_blank(),panel.grid = element_blank(),
                legend.key.width = unit(1.5, 'lines'),legend.key.height =  unit(4, 'lines'),legend.text = element_text(size=12),legend.title = element_text(size=14))+      
          #scale_color_continuous(type='viridis',name='stations/area')
          scale_color_gradientn(colours = pal,name='stations/area')
          
          
            
        #)
        #dev.off()
          p <- plot_grid(p1, p2) #labels=c('A', 'B')
          
          if (r=='all') {
            namepng<-paste0('./figures slope/',"ms_optim_allocations_ebsnbs_slope_",samp_df[s,'samp_scn'],'.png')
            title <- ggdraw() + draw_label(paste(samp_df[s,'region'],samp_df[s,'strat_var'],samp_df[s,'type']), fontface='bold')
          } else {
            namepng<-paste0('./figures slope/',"ms_optim_allocations_ebsnbs_slope_",samp_df[s,'samp_scn'],'_',r,'.png')
            title <- ggdraw() + draw_label(paste(samp_df[s,'region'],samp_df[s,'strat_var'],samp_df[s,'type'],'-',r), fontface='bold')
          }
          
          ragg::agg_png(namepng,  width = 12, height = 6, units = "in", res = 300)
          print(
          plot_grid(title, p, ncol=1, rel_heights=c(0.2, 1)) # rel_heights values control title margins
          )
          dev.off()
      }
   }




#########################
# loop over optimized sampling designs
#########################

mins<-c()
maxs<-c()

samp_df1<-samp_df
samp_df1$slp_effort_static<-NA
samp_df1$slp_effort_dynamic_warm<-NA
samp_df1$slp_effort_dynamic_cold<-NA
samp_df1$nbs_effort_static<-NA
samp_df1$nbs_effort_dynamic_warm<-NA
samp_df1$nbs_effort_dynamic_cold<-NA

#loop through sampling designs
for (s in c(1:nrow(samp_df))) { #nrow(samp_df)
  
  #s<-1
  
   if (samp_df[s,'type']=='static') {
    #load multispecies data
    #load(paste0('./output slope/multisp_optimization_static_data_ebsnbs_slope_st.RData')) #df
    regime<-c('all')
  } else {
    #load multispecies data
    #load(paste0('./output slope/multisp_optimization_static_data_ebsnbs_slope_dyn.RData')) #df
    regime<-c('cold','warm')
  }
  
  #   #if ebs
  # if (samp_df[s,'region']=='EBS') {
  #   df1<-
  #     df[which(df$cell<=53464 & df$cell>=15180+1),]
  #   #if ebs and nbs
  # } else if (samp_df[s,'region']=='EBS+NBS') {
  #   df1<-
  #     df[which(df$cell<=53464),]
  #   #if ebs and slope
  # } else if (samp_df[s,'region']=='EBS+SBS') {
  #   df1<-
  #     df[which(df$cell>=15180+1),]
  #   #if all region
  # } else {
  #   df1<-df
  # }
  # 
  # #target variables
  # tar_var<-paste0(rep(df_spp$Y,each=2),c('','_SQ_SUM'))
  # names(df1)[((ncol(df1)-length(tar_var))+1):ncol(df1)]<-tar_var
  # ispp<-n_spp
  # 
  # #removed cells because of depth
  # rem_cells<-df1[which(df1$include==FALSE | df1$Depth > 400),'cell']
  # #ok_cells<-df1[which(df1$include==TRUE | df1$Depth <= 400),'cell']
  # ok_cells <- df1[which(df1$include == TRUE | (df1$Depth <= 400 & df1$Depth > 0)), 'cell']
  # #load data_geostat file
  # #data_geostat<-readRDS(paste0('./data processed/species/',sp,'/','data_geostat_temp.rds')) 
  # 
  # #years
  # n_years<-length(2002:2016)
  
    
  for (r in regime) {
    
    #r<-regime[1]
    
    #save list
    load(
         file = paste0("./output slope/ms_optim_allocations_ebsnbs_slope_",samp_df[s,'samp_scn'],'_',r,"dens.RData")) #all
    
    #strata to plot
    dd<-all$result_list$solution$framenew
    #print(c(range(all$samples_strata$n_samples),sum(all$samples_strata$n_samples)))
    
    # mins<-c(mins,min(all$samples_strata$n_samples))
    # maxs<-c(maxs,max(all$samples_strata$n_samples))
    #all$ms_sample_allocations
    effort<-all$ms_sample_allocations[,colnames(all$ms_sample_allocations)[grepl(pattern = 'Str',colnames(all$ms_sample_allocations))]]
    effort1<-reshape2::melt(effort)
    names(effort1)<-c('STRATO','effort')
    effort1$STRATO<-gsub('Str_','',effort1$STRATO)

    dd1<-merge(dd[,c('DOMAINVALUE','STRATO','ID')],grid,by.x='ID',by.y='cell')
    dd2<-merge(dd1,effort1,by='STRATO')
    dd2$strata<-dd2$STRATO

    all_effort<- aggregate(ID ~ X1,all$result_list$sol_by_cell,FUN=length)
    names(all_effort)<-c('strata','effort_area')
    #all_effort$strata<-paste0('1_',all_effort$strata)
    dd2<-merge(dd2,all_effort,by=('strata'))
    dd2$effort_area<-dd2$effort_area/dd2$effort
    dd2$strata<-factor(dd2$strata,levels=c(1:10))
    
    #maxs and mins
    mins<-c(mins,min(unique(dd2$effort_area)))
    maxs<-c(maxs,max(unique(dd2$effort_area)))
    
    #title
    if (r=='all') {
      title <- paste0(samp_df[s,'region'],'\n',samp_df[s,'strat_var'],' ',samp_df[s,'type'])
    } else {
      title <- paste0(samp_df[s,'region'],'\n',samp_df[s,'strat_var'],' ',samp_df[s,'type'],' - ',r)
    }
    
    #SLOPE
    if (samp_df[s,'region'] %in% c('EBS+SBS','EBS+NBS+SBS')) {
      
      dd3<-subset(dd2,ID>53464)
      aggregate(ID ~ strata + effort, dd3, FUN=length)
      subset(aggregate(ID ~ strata + effort, dd2, FUN=length),strata %in% aggregate(ID ~ strata + effort, dd3, FUN=length)[,'strata'])[,'ID']
      
      samp_slp<-
      data.frame(aggregate(ID ~ strata + effort, dd3, FUN=length),
                 'IDall'=subset(aggregate(ID ~ strata + effort, dd2, FUN=length),
                                strata %in% aggregate(ID ~ strata + effort, dd3, FUN=length)[,'strata'])[,'ID'])
      
      samp_slp$pct<-samp_slp$ID/samp_slp$IDall*100
      samp_slp$n_samp<-round(samp_slp$pct/100*samp_slp$effort,digits = 0)
      samp_slp1<-subset(samp_slp,pct>10)
      
      #number of samples slope
      n_slp<-sum(samp_slp1$n_samp)
      
      if (samp_df[s,'type']=='static') {
  
        samp_df1[s,'slp_effort_static']<-n_slp
  
      } else if (r=='cold') {
        
        samp_df1[s,'slp_effort_dynamic_cold']<-n_slp
        
      } else {
  
        samp_df1[s,'slp_effort_dynamic_warm']<-n_slp
        
      }
      
      
      
      print(title)
      print(as.character(unique(dd3$strata)))
    }
    
    #NBS
    if (samp_df[s,'region'] %in% c('EBS+NBS','EBS+NBS+SBS')) {
      
      dd3<-subset(dd2,ID<15181)
      aggregate(ID ~ strata + effort, dd3, FUN=length)
      subset(aggregate(ID ~ strata + effort, dd2, FUN=length),strata %in% aggregate(ID ~ strata + effort, dd3, FUN=length)[,'strata'])[,'ID']
      
      samp_slp<-
        data.frame(aggregate(ID ~ strata + effort, dd3, FUN=length),
                   'IDall'=subset(aggregate(ID ~ strata + effort, dd2, FUN=length),
                                  strata %in% aggregate(ID ~ strata + effort, dd3, FUN=length)[,'strata'])[,'ID'])
      
      samp_slp$pct<-samp_slp$ID/samp_slp$IDall*100
      samp_slp$n_samp<-round(samp_slp$pct/100*samp_slp$effort,digits = 0)
      samp_slp1<-subset(samp_slp,pct>10)
      
      #number of samples slope
      n_slp<-sum(samp_slp1$n_samp)
      
      if (samp_df[s,'type']=='static') {
        
        samp_df1[s,'nbs_effort_static']<-n_slp
        
      } else if (r=='cold') {
        
        samp_df1[s,'nbs_effort_dynamic_cold']<-n_slp
        
      } else {
        
        samp_df1[s,'nbs_effort_dynamic_warm']<-n_slp
        
      }
      
      
      
      print(title)
      print(as.character(unique(dd3$strata)))
    }
    
  }
}

# samp_df2<-subset(samp_df1,region %in% c("EBS+SLOPE",'EBS+NBS+SLOPE'))
# samp_df21<-reshape2::melt(samp_df2,id.vars=c(names(samp_df2)[1:8]))
# samp_df21<-samp_df21[grepl("slp", samp_df21$variable), ]
# 
# samp_df21$scn<-paste0(samp_df21$region,'\n',samp_df21$strat_var,'\n',samp_df21$type)
# 
# samp_df21$strat_var<-factor(samp_df21$strat_var,levels = c('Depth','varTemp'))
# samp_df21$type<-factor(samp_df21$type,levels = c('static','dynamic'))
# 
# # Convert 'scn' to a factor based on 'region'
# samp_df21$scn <- factor(samp_df21$scn, levels = unique(samp_df21$scn[order(samp_df21$strat_var,samp_df21$type)]))
# 
# ggplot(data=samp_df21)+
#   geom_point(aes(x=scn,y=value,color=variable),size=3,alpha=0.7)+
#   scale_y_continuous('number of sampling stations in SLOPE')+
#   theme_minimal()+
#   theme(axis.title.x = element_blank())+
#   scale_color_manual(values = c('slp_effort_static'='black',
#                                 'slp_effort_dynamic_warm'='red',
#                                 'slp_effort_dynamic_cold'='blue'),
#                      labels=c('static','warm','cold'),name='regime')
# 
# 
# samp_df2<-subset(samp_df1,region %in% c("EBS+NBS",'EBS+NBS+SLOPE'))
# samp_df21<-reshape2::melt(samp_df2,id.vars=c(names(samp_df2)[1:8]))
# samp_df21<-samp_df21[grepl("nbs", samp_df21$variable), ]
# samp_df21$scn<-paste0(samp_df21$region,'\n',samp_df21$strat_var,'\n',samp_df21$type)
# 
# samp_df21$strat_var<-factor(samp_df21$strat_var,levels = c('Depth','varTemp'))
# samp_df21$type<-factor(samp_df21$type,levels = c('static','dynamic'))
# 
# # Convert 'scn' to a factor based on 'region'
# samp_df21$scn <- factor(samp_df21$scn, levels = unique(samp_df21$scn[order(samp_df21$strat_var,samp_df21$type)]))
# 
# ggplot(data=subset(samp_df21,region %in% c("EBS+NBS",'EBS+NBS+SLOPE')))+
#   geom_point(aes(x=scn,y=value,color=variable),size=3,alpha=0.7)+
#   scale_y_continuous('number of sampling stations in NBS')+
#   theme_minimal()+
#   theme(axis.title.x = element_blank())+
#   scale_color_manual(values = c('nbs_effort_static'='black',
#                                 'nbs_effort_dynamic_warm'='red',
#                                 'nbs_effort_dynamic_cold'='blue'),
#                      labels=c('static','warm','cold'),name='regime')


samp_df$region<-gsub('SBS','BSS',samp_df$region)
samp_df$strat_var<-gsub('Depth','depth',samp_df$strat_var)
samp_df$type<-gsub('dynamic','adaptive',samp_df$type)

#common legend
lims<-c(min(mins[13:24]),max(maxs[13:24]))
plot_list<-list()

#loop through sampling designs
for (s in c(1:nrow(samp_df))) { #nrow(samp_df)
  
  #s<-1
  
  if (samp_df[s,'type']=='static') {
    #load multispecies data
    #load(paste0('./output slope/multisp_optimization_static_data_ebsnbs_slope_st.RData')) #df
    regime<-c('all')
  } else {
    #load multispecies data
    #load(paste0('./output slope/multisp_optimization_static_data_ebsnbs_slope_dyn.RData')) #df
    regime<-c('cold','warm')
  }
  
  for (r in regime) {
    
    #load list
    load(
      file = paste0("./output slope/ms_optim_allocations_ebsnbs_slope_",samp_df[s,'samp_scn'],'_',r,".RData")) #all
    
    #strata to plot
    dd<-all$result_list$solution$framenew
    #all$ms_sample_allocations
    effort<-all$ms_sample_allocations[,colnames(all$ms_sample_allocations)[grepl(pattern = 'Str',colnames(all$ms_sample_allocations))]]
    effort1<-reshape2::melt(effort)
    names(effort1)<-c('STRATO','effort')
    effort1$STRATO<-gsub('Str_','',effort1$STRATO)
    
    dd1<-merge(dd[,c('DOMAINVALUE','STRATO','ID')],grid,by.x='ID',by.y='cell')
    dd2<-merge(dd1,effort1,by='STRATO')
    dd2$strata<-dd2$STRATO
    
    all_effort<- aggregate(ID ~ X1,all$result_list$sol_by_cell,FUN=length)
    names(all_effort)<-c('strata','effort_area')
    #all_effort$strata<-paste0('1_',all_effort$strata)
    dd2<-merge(dd2,all_effort,by=('strata'))
    dd2$effort_area<-dd2$effort_area/dd2$effort
    dd2$strata<-factor(dd2$strata,levels=c(1:10))
    
    #subset slope rows
    dd3<-subset(dd2,ID>53464)
    unique(dd3$strata)
    
    #title
    if (r=='all') {
      title <- paste0(samp_df[s,'region'],' and ',samp_df[s,'strat_var'],'\n',samp_df[s,'type'])
    } else {
      title <- paste0(samp_df[s,'region'],' and ',samp_df[s,'strat_var'],'\n',samp_df[s,'type'],'-',r)
    }
    
    #palette
    pal <- wes_palette("Zissou1", 1000, type = "continuous")      #save plot
    
    #ragg::agg_png(paste0('./figures slope/',"ms_optim_allocations_ebsnbs_slope_",samp_df[s,'samp_scn'],'_effort.png'),  width = 7, height = 7, units = "in", res = 300)
    #print(
    p<-
      ggplot()+
      geom_point(data=dd2,aes(x=Lon,y=Lat,color=effort_area),size=0.1)+
      geom_polygon(data=NBS_sh,aes(x=long,y=lat,group=group),fill=NA,col='black')+
      geom_polygon(data=EBSshelf_sh,aes(x=long,y=lat,group=group),fill=NA,col='black')+
      geom_polygon(data=EBSslope_sh,aes(x=long,y=lat,group=group),fill=NA,col='black')+
      ggtitle(title)+
      theme_minimal()+
      theme(axis.title = element_blank(),axis.text = element_blank(),panel.grid = element_blank(),
            legend.key.width = unit(1.5, 'lines'),legend.key.height =  unit(4, 'lines'),
            legend.text = element_text(size=12),legend.title = element_text(size=14),
            plot.title = element_text(hjust = 0.1,vjust=-5,face='bold'))+      
      #scale_color_gradientn(colours = pal,name='stations/area')
      scale_color_gradientn(colours = pal,name='stations/area',limits=lims) #
    
    plot_list[[paste0(s,'_',r)]]<-p+theme(legend.position = 'none')
  }
}

# # Extract the legend
# legend = cowplot::get_plot_component(p, 'guide-box-top', return_all = TRUE)
# cowplot::ggdraw(legend)

#plots
agg_png(paste0('./figures slope/sampling intensity survey designs_dens.png'), width = 9, height = 13, units = "in", res = 300)
plot_grid(plotlist = plot_list[13:24],nrow = 4)
dev.off()
1


#check
samp_df
load(file = paste0("./output slope/ms_optim_allocations_ebsnbs_slope_scn1_alldens.RData"))
all$cv #ebs
load(file = paste0("./output slope/ms_optim_allocations_ebsnbs_slope_scn3_alldens.RData"))
all$cv #with nbs
load(file = paste0("./output slope/ms_optim_allocations_ebsnbs_slope_scn5_all.RData"))
all$cv #with slope
load(file = paste0("./output slope/ms_optim_allocations_ebsnbs_slope_scn7_all.RData"))
all$cv #all

#SPECIES CHECK
#loop through sampling designs
#for (s in c(1:nrow(samp_df))) { #nrow(samp_df)
#for (s in c(1:4,9:12)) { #nrow(samp_df)
for (s in c(5:8,13:24)) { #nrow(samp_df)
    
  #s<- 1
  #s<-12
  #print scenario to check progress
  cat(paste("\n #############  Sampling Scenario", samp_df[s,"samp_scn"], " #############\n"))
  
  #domain
  dom<-samp_df[s,'domain']
  idom<-samp_df[s,'idomain']
  
  ###############
  # load ms data and settings
  ###############
  
  if (samp_df[s,'type']=='static') {
    #load multispecies data
    load(paste0('./output slope/multisp_optimization_static_data_ebsnbs_slope_st.RData')) #df
    regime<-c('all')
  } else {
    #load multispecies data
    load(paste0('./output slope/multisp_optimization_static_data_ebsnbs_slope_dyn.RData')) #df
    regime<-c('cold','warm')
  }
  
  #if ebs
  if (samp_df[s,'region']=='EBS') {
    df1<-
      df[which(df$cell<=53464 & df$cell>=15180+1),]
    #if ebs and nbs
  } else if (samp_df[s,'region']=='EBS+NBS') {
    df1<-
      df[which(df$cell<=53464),]
    #if ebs and slope
  } else if (samp_df[s,'region']=='EBS+SBS') {
    df1<-
      df[which(df$cell>=15180+1),]
    #if all region
  } else {
    df1<-df
  }
 
  spp_cols1<-paste0(spp_name$spp,'_sumDensity_sq')
  spp_cols2<-paste0(spp_name$spp,'_sumDensity')
  spp_cols<-c(spp_cols1,spp_cols2)
  
  df1<-df1[,spp_cols]
  # Ensure only numeric columns are selected
  spps <- (names(df1)[colSums(df1[sapply(df1, is.numeric)]) != 0])
  # Remove everything after the first underscore
  spps <- sort(unique(sub("_.*", "", spps)))
  #print
  cat(paste0('############# ',samp_df[s,'region'], ' - ', length(spps), ' #################\n'))
  cat(paste0('############# ',spps, ' #################\n'))
}
