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

#libraries from cran to call or install/load
pack_cran<-c("splines",'SamplingStrata','wesanderson','dplyr','sp',
             'sf','maptools','rgeos','scales',
             'rnaturalearth','grid','ggplot2','spatstat','ragg')

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
       'Chionoecetes bairdi')

#remove Anoploma and Reinhardtius because habitat preference reasons
spp<-setdiff(spp, c('Anoplopoma fimbria','Reinhardtius hippoglossoides'))

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
        'Tanner crab')

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

#load grid of NBS and EBS
load('./extrapolation grids/northern_bering_sea_grid.rda')
load('./extrapolation grids/eastern_bering_sea_grid.rda')
grid<-as.data.frame(rbind(data.frame(northern_bering_sea_grid,region='NBS'),data.frame(eastern_bering_sea_grid,region='EBS')))
grid$cell<-1:nrow(grid)
#add col and row number
x1<-grid[,c('Lon','Lat','cell')]
names(x1)<-c('x','y','z')
coordinates(x1)=~x + y
#crs(x1)<-c(crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
proj4string(x1)<-CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
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
colnames(x5)<-c('Lat','Lon','cell','optional','col','row')
grid<-x5[,c('Lat','Lon','cell','col','row')]

# Define plot extent (through trial end error) units km (for plotting purposes)
panel_extent <- data.frame(x = c(-1716559.21, -77636.05), #x = c(-1326559.21, -87636.05),
                           y = c(483099.5, 2194909.7)) #y = c(533099.5, 1894909.7))

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
# SCENARIOS
###################################

#sampling scenarios
samp_df<-expand.grid(strat_var=c('Depth_varTemp','varTemp','Depth'),
                     target_var=c('sumDensity'), #,'sqsumDensity'
                     n_samples=c(520), #c(300,500) 520 (EBS+NBS+CRAB);26 (CRAB); 350 (EBS-CRAB); 494 (NBS-CRAB)
                     n_strata=c(15)) #c(5,10,15)

#add scenario number
samp_df$samp_scn<-paste0(paste0('scn',1:nrow(samp_df)))

    
###############
# load ms data and settings
###############

  #load multispecies data
  load(paste0('./output/multisp_optimization_static_data.RData')) #df
  
  tar_var<-paste0(rep(df_spp$Y,each=2),c('','_SQ_SUM'))
  names(df)[((ncol(df)-length(tar_var))+1):ncol(df)]<-tar_var
  ispp<-n_spp

  #removed cells because of depth
  rem_cells<-df[which(df$include==FALSE),'cell']
  ok_cells<-df[which(df$include==1),'cell']
  
  #load data_geostat file
  #data_geostat<-readRDS(paste0('./data processed/species/',sp,'/','data_geostat_temp.rds')) 
  
  #n cells
  n_cells<-length(ok_cells)
  
  #years
  n_years<-length(1982:2022)
  
  #number of domains
  n_dom<-1
  
  #domain_input
  domain_input<-rep(1, n_cells)
  
  #subset cells with appropiate depth
  static_df1<-subset(df,cell %in% ok_cells)
  
  #########################
  # loop over optimized sampling designs
  #########################
  
  #loop through sampling designs
  for (s in 1:nrow(samp_df)) {
  #for (s in 1) {
      
    #s<-1
    
    #print scenario to check progress
    cat(paste(" #############  Sampling Scenario", samp_df[s,"samp_scn"], " #############\n"))
    
    #if scenario includes two stratifying factors
    if (grepl('_',samp_df[s,'strat_var'])) {
      #stratification variables 
      stratum_var_input<-data.frame(X1 = static_df1[,paste0(sub("\\_.*", "", samp_df[s,'strat_var']))],
                                    X2 = static_df1[,paste0(sub(".*_", "", samp_df[s,'strat_var']))]) 
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
    cv_df[["DOM"]] <- 1
    for (iispp in 1:ispp) cv_df[[paste0("CV", iispp)]] <- as.numeric(srs_cv[iispp])
    cv_df[["domainvalue"]] <- 1
    cv_df <- as.data.frame(cv_df)
    
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
    
    #while loop until the desired number of strata  
    flag<-TRUE
    while (flag) {
      
      #print state of spp[1] to get an approximation of the state
      cat(paste(" #############   OPTIMIZING STRATA - CV of ",spp[1],' ', cv_df[,2],"  #############\n"))
      
      #run optimization
      solution <- optimStrata(method = "continuous", #continous variables
                              errors = cv_df,  #precision level - maximum allowable coefficient of variation set by the simple random sampling 
                              framesamp = frame, #df of input variables
                              iter = 30, #300 #maximum number of iterations
                              pops = 10, #100  #dimension of each generations
                              elitism_rate = 0.1, #0.1
                              mut_chance = 1 / (no_strata[1] + 1), #mutation chance
                              nStrata = no_strata, #maximum strata
                              showPlot = TRUE, #FALSE
                              writeFiles = FALSE)
      
      #flag to keep the loop
      flag<-ifelse(nrow(solution$aggr_strata)!=unique(samp_df$n_strata),TRUE,FALSE)
      
       #if condition reduce or increase CV to achieve the objective
      if (nrow(solution$aggr_strata)<unique(samp_df$n_strata)) {
        cv_df[,c(2:(ispp+1))]<-cv_df[,c(2:(ispp+1))]-(cv_df[,c(2:(ispp+1))]*0.001) #reduce 0.1% CV
      } else if (nrow(solution$aggr_strata)>unique(samp_df$n_strata)){
        cv_df[,c(2:(ispp+1))]<-cv_df[,c(2:(ispp+1))]+cv_df[,c(2:(ispp+1))]*0.01} #increase 1%
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
      #iispp<-1
      
      temp_n <- result_list$n
      
      ## Subset density data for species ispp
      ss_df <- subset(x = frame, 
                      select = c("domainvalue", "id", "WEIGHT", "X1", #"X2", 
                                 paste0("Y", iispp), paste0("Y", iispp, "_SQ_SUM")))
      names(ss_df)[grep(x = names(ss_df), pattern = "Y")] <- c("Y1", "Y1_SQ_SUM")
      
      ## Create CV inputs to the Bethel algorithm; initialize at SRS CV
        error_df <- data.frame("DOM" = "DOM1",
                               as.numeric(srs_cv)[iispp],
                               "domainvalue"  = 1)
        names(error_df)[2] <- "CV1"
        
        ## subset stratum stats for the species of interest as inputs to the 
        ## Bethel algorithm
        temp_stratif <- 
            solution$aggr_strata[, c("STRATO", "N", 
                                     paste0("M", iispp), paste0("S", iispp), 
                                     "COST", "CENS", "DOM1", "X1" ,"SOLUZ")]
        temp_stratif$N <- temp_stratif$N / n_years
        temp_stratif$DOM1 <- 1
        names(temp_stratif)[3:4] <- paste0(c("M", "S"), 1)
        
        ## run bethel at the SRS CV
        temp_bethel <- SamplingStrata::bethel(
          errors = error_df,
          stratif = temp_stratif, 
          realAllocation = T, 
          printa = T)
        
        ## Save the current n and cv constraint
        temp_n <- sum(ceiling(temp_bethel))
        updated_cv_constraint <- 
          as.numeric(attributes(temp_bethel)$outcv[, "PLANNED CV "])
        
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
          updated_cv_constraint <- 
            as.numeric(attributes(temp_bethel)$outcv[, "PLANNED CV "])
          print(paste0("n = ", temp_n, ", ", updated_cv_constraint) )
        }
        
        ## Save the CV and station allocations that corresponds to n_samples

          temp_idx <- ss_sample_allocations$n == n_samples & 
            ss_sample_allocations$species == spp[iispp]
        
        ss_sample_allocations[temp_idx, "CV"] <- 
          as.numeric(attributes(temp_bethel)$outcv[, "ACTUAL CV"])
        
        ss_sample_allocations[temp_idx, paste0("Str_", 1:length(temp_bethel))] <- 
          as.integer(temp_bethel)
        
      
    }
    
    ######################
    ##   8) Adjust MS solution ----
    ##   Optimize allocation across a range of sample sizes, given the original
    ##   stratification.
    #####################
    
    ms_sample_allocations <- expand.grid(n = n_samples)
    
    ## Subset lower limits of CVs from the ss cvs
    ss_cvs <- subset(ss_sample_allocations, n == n_samples)$CV
      
    ## CV dataframe input to Bethel algorithm
    error_df <-  data.frame("DOM" = "DOM1",
                            srs_cv,
                            "domainvalue"  = 1)
    
      
      names(error_df)[2:(1 + n_spp)] <- paste0("CV", 1:n_spp)
      
    
      ## Stratum statistics input to the Bethel algorithm
      temp_stratif <- solution$aggr_strata
      temp_stratif$N <- temp_stratif$N / n_years
      temp_stratif$DOM1 <- 1
      
      ## Run Bethel algorithm and save current n and cv constraints
      temp_bethel <- SamplingStrata::bethel(
        errors = error_df,
        stratif = temp_stratif, 
        realAllocation = T, 
        printa = T)
      
      temp_n <- sum(ceiling(temp_bethel))
      updated_cv_constraint <- 
        as.numeric(attributes(temp_bethel)$outcv[, "PLANNED CV "])
      
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
        
        ## Save sample size and CV constraint
        temp_n <- sum(as.numeric(temp_bethel))
        updated_cv_constraint <- 
          as.numeric(attributes(temp_bethel)$outcv[, "PLANNED CV "])
        
        ## Print out result to console
        print(paste0("n = ", temp_n) )
      }
      
      ## Save optimized CV 
      temp_idx <- ms_sample_allocations$n == n_samples

        ms_sample_allocations[temp_idx, paste0("CV", 1:n_spp)] <- 
          as.numeric(attributes(temp_bethel)$outcv[, "ACTUAL CV"])
      
      
      ms_sample_allocations[temp_idx, paste0("Str_", 1:length(temp_bethel))] <- 
        as.integer(temp_bethel)
      
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
    
    #save list
      save(all,
           file = paste0("./output/ms_optim_allocations_",samp_df[s,'samp_scn'],".RData"))
   }

  ###################
  # Plot comparison sampling effort x strata for each species under singlesp or multisp allocation of samples
  ###################
    
  cvs<-data.frame(matrix(NA,nrow = 0,ncol = 4))
  names(cvs)<-c('sp','samp','ss','ms')
  
  for (s in 1:nrow(samp_df)) {
    
    #s<-1
    
    cat(paste('##############################', s, 'of',nrow(samp_df)))
    
    samp<-samp_df[s,'samp_scn']
    
    #load multispecies data
    load(paste0('./output/multisp_optimization_static_data.RData')) #df
    
    #load optimized stratification
    load(file = paste0("./output/ms_optim_allocations_",samp_df[s,'samp_scn'],".RData")) #all
    
    strata<-rbind(all$result_list$solution$indices,
                  data.frame(ID=rem_cells,X1=NA))
    colnames(strata)<-c('cell','Strata')
    
    #n cells by strata to calculate proportion of sampling effort as n samples/total cells
    strata_sum<-aggregate(cell ~ Strata, data = strata, FUN = length)
    names(strata_sum)[2]<-'total_cell'
    
    #merge with ss data
    ss<-all$ss_sample_allocations
    ss1<-data.frame(ss[,c(paste0('Str_',1:unique(samp_df$n_strata)))],row.names = ss$species)
    colnames(ss1)<-1:unique(samp_df$n_strata)
    ss2<-reshape2::melt(as.matrix(ss1))
    names(ss2)<-c('species','Strata','ss_samples')
    strata1<-merge(strata,ss2,by='Strata')
    
    #merge with ms data
    ms<-all$ms_sample_allocations
    ms_strata<-data.frame('Strata'=1:unique(samp_df$n_strata),
                          'ms_samples'=as.numeric(ms[1,16:30]))
    
    
    strata2<-merge(strata1,ms_strata,by='Strata')
    strata2<-merge(strata2,strata_sum,by='Strata')
    #strata2$prop<-strata2$n_samples/strata2$total_cell
    strata2$ratio<-log(strata2$ss_samples/strata2$ms_samples)
    dim(strata2)
    
    df1<-df[,c('Lat','Lon','cell')]
    df1<-merge(df1,strata2,by='cell')
    df1$Strata[is.na(df1$Strata)]<-999
    
    #to store plots
    plot_list_n<-list()
    plot_list_d<-list()
    
    for (isp in spp) {
      
    #isp<-spp[1]
    
    df2<-subset(df1,species==isp)
    
    #ms CV
    ms_cv<-
      ms[,paste0('CV',match(isp,spp))]
    
    #ms SCV
    ms_scv<-(ms_cv*100)^2
    
    #ss CV
    ss_cv<-
      ss$CV[match(isp,spp)]
    
    #ss SCV
    ss_scv<-(ss_cv*100)^2
    
    icvs<-data.frame('sp'=isp,
                     'samp'=samp,
                     'ss'=ss_cv,
                     'ms'=ms_cv)
    
    #append
    cvs<-rbind(cvs,icvs)
    
    #df to spatialpoint df
    coordinates(df2) <- ~ Lon + Lat
    crs(df2)<-c(crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    
    #reproject coordinates for plotting purposes
    df_1<-spTransform(df2,'+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')
    df_2<-data.frame(df_1)
    
    #x and y cells
    xycells<-as.integer(sqrt(dim(df_1)[1]))
    
    # create a template raster
    r1 <- raster(ext=extent(df_1),ncol=xycells, nrow=xycells) #c(15800,15800) 7000
    
    #create raster
    r2<-rasterize(df_1, r1 ,field=c('Strata','ms_samples','ss_samples','ratio'))
    crs(r2) <- CRS('+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')
    r2[r2==999]<-NA
    
    #create polygon to get boundaries of each strata
    r3<-as.data.frame(r2,xy=TRUE)
    r4<-rasterToPolygons(r2$Strata,dissolve=TRUE,)
    
    #color palette
    pal <- wes_palette("Zissou1", length(sort(unique(r3$ss_samples))), type = "continuous")
    color_scale_ss <- setNames(as.character(pal), sort(unique(r3$ss_samples)))
    r3<-r3[complete.cases(r3$Strata),] 
    
    #as factors
    r3$ss_samples<-as.factor(r3$ss_samples)
    #r3$ratio<-as.factor(r3$ratio)
  
    #common name
    com<-spp_name[which(spp_name$spp==isp),'common']
    
    #plot by number of samples
    pn<-
    ggplot()+
      geom_raster(data=r3,aes(x=x,y=y,fill=ss_samples))+
      geom_polygon(data=r4,aes(x=long,y=lat,group=group), colour="black", fill=NA)+
      scale_fill_manual(values = color_scale_ss,name='sampling effort')+
      guides(fill=guide_colorbar(title.position = 'top', title.hjust = 0.5,ticks.colour = NA,frame.colour = 'black'))+
      scale_x_continuous(expand = c(0,0),position = 'bottom',
                         breaks = c(-175,-170,-165,-160,-155),sec.axis = dup_axis())+
      coord_sf(crs = '+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs',
               xlim = panel_extent$x-c(000000,100000),
               ylim = panel_extent$y-c(0,300000),
               label_axes = "-NE-")+
      theme(aspect.ratio = 1,panel.grid.major = element_blank(),
            panel.background = element_rect(fill = NA),panel.ontop = TRUE,
            legend.background =  element_rect(fill = "transparent", colour = "transparent"),legend.key.height= unit(20, 'points'),
            legend.key.width= unit(20, 'points'),axis.title = element_blank(),legend.position = 'none',
            panel.border = element_rect(fill = NA, colour = NA),legend.key = element_rect(color="black"),
            legend.spacing.y = unit(8, 'points'),
            axis.text=element_blank(),axis.ticks = element_blank(),
            plot.margin = margin(0.01,0.01,0.01,0.01), 
            axis.ticks.length = unit(-5,"points"),plot.title = element_text(size=12,hjust = 0.5,vjust=-5, face="bold"))+
      scale_y_continuous(expand = c(0,0),position = 'right',sec.axis = dup_axis())+
      #labs(title=paste0(com,'\n(msSCV=',round(ms_scv,digits = 3),'; ssSCV=',round(ss_scv,digits = 3),')'),fill='')
      labs(title=paste0(com,'\n(msCV=',round(ms_cv,digits = 3),'; ssCV=',round(ss_cv,digits = 3),')'),fill='')
    
    #plot by number of samples
    pd<-
    ggplot()+
      geom_raster(data=r3,aes(x=x,y=y,fill=ratio))+
      geom_polygon(data=r4,aes(x=long,y=lat,group=group), colour="black", fill=NA)+
      scale_fill_gradient2(midpoint = 0, low = "red", mid = "white",
                             high = "blue",name='ss samples - ms samples')+
      guides(fill=guide_colorbar(title.position = 'top', title.hjust = 0.5,ticks.colour = NA,frame.colour = 'black'))+
      scale_x_continuous(expand = c(0,0),position = 'bottom',
                         breaks = c(-175,-170,-165,-160,-155),sec.axis = dup_axis())+
      coord_sf(crs = '+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs',
               xlim = panel_extent$x-c(000000,100000),
               ylim = panel_extent$y-c(0,300000),
               label_axes = "-NE-")+
      theme(aspect.ratio = 1,panel.grid.major = element_blank(),
            panel.background = element_rect(fill = NA),panel.ontop = TRUE,
            legend.background =  element_rect(fill = "transparent", colour = "transparent"),legend.key.height= unit(20, 'points'),
            legend.key.width= unit(20, 'points'),axis.title = element_blank(),legend.position = 'none',
            panel.border = element_rect(fill = NA, colour = NA),legend.key = element_rect(color="black"),
            legend.spacing.y = unit(8, 'points'),
            axis.text=element_blank(),axis.ticks = element_blank(),
            plot.margin = margin(0.01,0.01,0.01,0.01), 
            axis.ticks.length = unit(-5,"points"),plot.title = element_text(size=12,hjust = 0.5,vjust=-5, face="bold"))+
      scale_y_continuous(expand = c(0,0),position = 'right',sec.axis = dup_axis())+
      #labs(title=paste0(com,'\n(msSCV=',round(ms_scv,digits = 3),'; ssSCV=',round(ss_scv,digits = 3),')'),fill='')
      labs(title=paste0(com,'\n(msCV=',round(ms_cv,digits = 3),'; ssCV=',round(ss_cv,digits = 3),')'),fill='')
    
    plot_list_n[[isp]]<-pn
    plot_list_d[[isp]]<-pd
    }
  
    
    #color palette
    pal <- wes_palette("Zissou1", length(sort(unique(r3$ms_samples))), type = "continuous")
    color_scale_ms <- setNames(as.character(pal), sort(unique(r3$ms_samples)))
    
    #as factors
    r3$ms_samples<-as.factor(r3$ms_samples)
    #r3$ratio<-as.factor(r3$ratio)
    
    #plot by number of samples
    pm<-
      ggplot()+
      geom_raster(data=r3,aes(x=x,y=y,fill=ms_samples))+
      geom_polygon(data=r4,aes(x=long,y=lat,group=group), colour="black", fill=NA)+
      scale_fill_manual(values = color_scale_ms,name='sampling effort')+
      guides(fill=guide_colorbar(title.position = 'top', title.hjust = 0.5,ticks.colour = NA,frame.colour = 'black'))+
      scale_x_continuous(expand = c(0,0),position = 'bottom',
                         breaks = c(-175,-170,-165,-160,-155),sec.axis = dup_axis())+
      coord_sf(crs = '+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs',
               xlim = panel_extent$x-c(000000,100000),
               ylim = panel_extent$y-c(0,300000),
               label_axes = "-NE-")+
      theme(aspect.ratio = 1,panel.grid.major = element_blank(),
            panel.background = element_rect(fill = NA),panel.ontop = TRUE,
            legend.background =  element_rect(fill = "transparent", colour = "transparent"),legend.key.height= unit(20, 'points'),
            legend.key.width= unit(20, 'points'),axis.title = element_blank(),legend.position = 'none',
            panel.border = element_rect(fill = NA, colour = NA),legend.key = element_rect(color="black"),
            legend.spacing.y = unit(8, 'points'),
            axis.text=element_blank(),axis.ticks = element_blank(),
            plot.margin = margin(0.01,0.01,0.01,0.01), 
            axis.ticks.length = unit(-5,"points"),plot.title = element_text(size=14,hjust = 0.5,vjust=-5, face="bold"))+
      scale_y_continuous(expand = c(0,0),position = 'right',sec.axis = dup_axis())+
      labs(title=paste0('multispecies\n',gsub('_',' + ',samp_df[s,'strat_var'])),fill='')  
      
  #create legends
  legend_d<-
    ggplot()+
    geom_raster(data=r3,aes(x=x,y=y,fill=as.numeric(ratio)))+
    scale_fill_gradient2(midpoint = mean(range(r3$ratio)), low = "red", mid = "white",
                         high = "blue",breaks=range(as.numeric(r3$ratio)),labels=c("Lower","Higher"),name='log(ss/ms samples)')+
    guides(fill=guide_colorbar(title.position = 'top', title.hjust = 0.5,ticks.colour = NA,frame.colour = 'black'))+
    theme(legend.position = 'bottom')+
    labs(fill='')
  
  legend_n<-
    ggplot()+
    geom_raster(data=r3,aes(x=x,y=y,fill=as.numeric(ss_samples)))+
    scale_fill_gradientn(colours = pal,breaks=range(as.numeric(r3$ss_samples)),labels=c("Low","High"),name='sampling effort (n samples)')+
    guides(fill=guide_colorbar(title.position = 'top', title.hjust = 0.5,ticks.colour = NA,frame.colour = 'black'))+
    theme(legend.position = 'bottom')+
    labs(fill='')
  
  legend1 <- cowplot::get_legend( 
    legend_d + 
      theme(legend.position = "bottom") 
  ) 
  
  legend2 <- cowplot::get_legend( 
    legend_n + 
      theme(legend.position = "bottom") 
  ) 
  
  pgrid1<-cowplot::plot_grid(plotlist = plot_list_d, nrow = 2)
  pgrid2<-cowplot::plot_grid(plotlist = plot_list_n, nrow = 2)
  
  #save plots
  ragg::agg_png(paste0('./figures/sampling designs ss ratio_',samp_df[s,'strat_var'],'.png'), width = 20, height = 7, units = "in", res = 300)
  print(cowplot::plot_grid(pgrid1, legend1, nrow = 2, rel_heights = c(1, .1)))
  dev.off()
  
  ragg::agg_png(paste0('./figures/sampling designs ss n_',samp_df[s,'strat_var'],'.png'), width = 20, height = 7, units = "in", res = 300)
  print(cowplot::plot_grid(pgrid2, legend2, nrow = 2, rel_heights = c(1, .1)))
  dev.off()
  
  ragg::agg_png(paste0('./figures/sampling designs ms_',samp_df[s,'strat_var'],'.png'), width = 5, height = 5, units = "in", res = 300)
  print(pm)
  dev.off()
  
}  
  
  save(cvs,file = './output/ss_ms.RData')
  load(file = './output/ss_ms.RData')
  spp_name$common<-gsub('_EBSNBS','',spp_name$common)
  cvs1<-merge(cvs,spp_name,by.x='sp',by.y='spp')
  cvs1<-cvs1[order(cvs1$common,decreasing = FALSE),]
  cvs1$common<-factor(cvs1$common,levels = rev(sort(unique(cvs1$common))))
  #cvs1$samp<-factor(cvs1$samp,
  #               levels = c('scn3','scn2','scn1'))
  
  #get estimated index SD for each survey across years, sampling scenario and approach
  load('./output/estimated_index_hist.RData') #ind2
  index_sd<-aggregate(index ~ spp + year + scn + approach + sim,ind2,FUN = function(x) c(sd = sd(x)))
  index_sd[which(index_sd$approach=='sys' & index_sd$scn=='scnbase'),]
  
  #true cv
  load('./output/true_ind_hist.RData') #ind2
  
  #true index reshape
  true_ind2<-reshape2::melt(true_ind,id.vars='year')
  names(true_ind2)[2]<-'spp'
  
  #merge sd and true index
  df<-merge(index_sd,true_ind2,by=c('year','spp'))
  df$cvtrue<-df$index/df$value
  names(df)[2:3]<-c('sp','samp')
  df<-merge(df,spp_name,by.x='sp',by.y='spp')
  df1<-subset(df,samp %in% paste0('scn',1:3))
  mean_cvtrue<-aggregate(cvtrue ~ sp + samp ,df,FUN = function(x) c(mean = mean(x)))
  mean_cvtrue
  levels(cvs1$samp)
  
  head(mean_cvtrue)
  names(mean_cvtrue)<-c('sp','samp','cvtrue')
  head(cvs1)
  
  cvs2<-merge(cvs1,mean_cvtrue,by=c('sp','samp'))
  
  ggplot()+
    geom_boxplot(data=df,aes(x=cvtrue,y=common,group=interaction(samp,common)))#,group=samp),color='black',fill='white',stat = "identity", position = position_dodge(width = 0.7),size=2)
    
  cvs3<-reshape2::melt(cvs2)
    
  p1<-
  ggplot()+
    #geom_boxplot(data=df1,aes(x=cvtrue,y=common,group=interaction(samp,common),color=samp),fill='white', position = position_dodge(width = 0.7),outlier.shape = NA)+
    geom_linerange(data=cvs2,aes(xmin=ss,xmax=ms,x=ms,y=common,color=samp),linewidth=1,stat = "identity", position = position_dodge(width = 0.7))+ 
    geom_point(data=subset(cvs3,variable!='cvtrue'),aes(x=value,y=common,group=samp,shape=variable),stat = "identity", position = position_dodge(width = 0.7),size=1.5)+
    
    geom_point(data=cvs2,aes(x=ms,y=common,group=samp),shape=21,stat = "identity", color='black',fill='black',position = position_dodge(width = 0.7),size=1.5,alpha=0.7)+
    #geom_point(data=cvs2,aes(x=cvtrue,y=common,group=samp),shape='*',color='black',stat = "identity", position = position_dodge(width = 0.7),size=7)+
    #geom_point(data=cvs2,aes(x=cvtrue,y=common,group=samp,fill=samp),shape=21,color='black',stat = "identity", position = position_dodge(width = 0.7),size=2)+
    scale_color_manual(values=c('scn1'='#9B59B6','scn2'='#3498DB','scn3'='#1ABC9C'),
                      labels = c('depth','varSBT','depth + varSBT'),
                      limits=c('scn3','scn2','scn1'),
                      name='optimized\nstratification')+
    scale_fill_manual(values=c('scn1'='#9B59B6','scn2'='#3498DB','scn3'='#1ABC9C'),
                       labels = c('opt depth','opt varSBT','opt depth + varSBT'),
                       limits=c('scn3','scn2','scn1'),
                       name='stratification')+
    scale_shape_manual(values=c('ss'=4,'ms'=16),
                      labels = c('single-species','multispecies'),
                      limits=c('ss','ms'),
                      name='objective')+
    # scale_color_manual(values=c('scn1'='#4b7a99','scn2'='#679bc3','scn3'='#8db6c3'),
    #                   labels = c('opt depth','opt varSBT','opt depth + varSBT'),name='stratification')+
    theme_bw()+
    theme(panel.grid.minor = element_line(linetype=2,color='grey90',),#strip.background = element_rect(fill='white'),
          legend.key.size = unit(15, 'points'),legend.direction = 'vertical',legend.text = element_text(size=10),legend.position = 'right',#legend.position=c(.70,.85),
          legend.title = element_text(size=11),legend.spacing.x = unit(0.10, "cm"),legend.box.spacing =  unit(0.10, "cm"), #,strip.text = element_text(size=12)
          strip.background = element_blank(), legend.box.background = element_blank(),#legend.background = element_blank(),legend.box = 'horizontal',#legend.justification = 'right',legend.position='bottom',#
          strip.text = element_blank())+ #axis.text.x = element_text(angle=90,vjust=0.5),
    expand_limits(x = 0)+
    labs(y='',x='expected CV')+
    guides(color=guide_legend(order = 1))+
    scale_x_continuous(limits = c(0,max(cvs1$ms)+max(cvs1$ms)*0.3),expand = c(NA,0)) #expand = c(NA,0.1),limits = c(0,NA)
  
  p1
  
  #save plot
  ragg::agg_png(paste0('./figures/CVss_CVms.png'),  width = 6, height = 7, units = "in", res = 300)
  p1
  dev.off()
  
  
  cvs1$samp<-factor(cvs1$samp,
                    levels = c('scn3','scn2','scn1'))
  
  # #p<- 
  # ggplot()+
  #   #geom_linerange(data=cvs1,aes(xmin=ss,xmax=ms,x=ms,y=common,color=samp),linewidth=1,stat = "identity", position = position_dodge(width = 0.7))+ 
  #   geom_point(data=cvs1,aes(x=log(ms/ss),y=rev(common),group=samp,fill=samp),stat = "identity", position = position_dodge(width = 0.5),size=3,shape=21)+
  #   #geom_point(data=cvs1,aes(x=ms,y=common,group=samp),shape=16,stat = "identity", position = position_dodge(width = 0.7),size=2)+
  #     scale_fill_manual(values=c('scn1'='#4b7a99','scn2'='#679bc3','scn3'='#8db6c3'),
  #                       labels = c('opt depth','opt varSBT','opt depth + varSBT'),
  #                       limits=c('scn3','scn2','scn1'),
  #                       name='stratification')+
  #   # scale_color_manual(values=c('scn1'='#4b7a99','scn2'='#679bc3','scn3'='#8db6c3'),
  #   #                   labels = c('opt depth','opt varSBT','opt depth + varSBT'),name='stratification')+
  #   theme_bw()+
  #   theme(panel.grid.minor = element_line(linetype=2,color='grey90',),#strip.background = element_rect(fill='white'),
  #         legend.key.size = unit(12, 'points'),legend.direction = 'vertical',legend.text = element_text(size=9),legend.position=c(.78,.925),
  #         legend.title = element_text(size=10),legend.spacing.x = unit(0.05, "cm"),legend.box.spacing =  unit(0.01, "cm"), #,strip.text = element_text(size=12)
  #         strip.background = element_blank(), legend.box.background = element_rect(fill = "white", color = "black"),#legend.background = element_blank(),legend.box = 'horizontal',#legend.justification = 'right',legend.position='bottom',#
  #         strip.text = element_blank())+ #axis.text.x = element_text(angle=90,vjust=0.5),
  #   expand_limits(x = 0)+
  #   labs(y='',x='log(CVms/CVss)')+
  #   scale_x_continuous(limits = c(0,max(log(cvs1$ms/cvs1$ss))+max(log(cvs1$ms/cvs1$ss))*0.1),expand = c(NA,0)) #expand = c(NA,0.1),limits = c(0,NA)
  # 
  # #save plot
  # ragg::agg_png(paste0('./figures/CVss_CVms_ratio.png'),  width = 5, height = 7, units = "in", res = 300)
  # p
  # dev.off()

  
  
  ###################
  # Plot spatial random fields
  ###################
  plot_list_rf<-list()
  
  for (isp in spp) {
    
    #isp<-'Gadus macrocephalus'
    cat(paste(" #############  ",isp ," #############\n"))
    
    #common name
    com<-spp_name[which(spp_name$spp==isp),'common']
    
    #fit file
    ff<-list.files(paste0('./shelf EBS NBS VAST/',isp,'/'),'fit',recursive=TRUE)
    
    #load fit file
    load(paste0('./shelf EBS NBS VAST/',isp,'/',ff)) #fit
    
    #get spatial random fields
    spf<-fit$Report$Omega1_gc
    
    #get dataframe
    D_gt <- spf
    D_gt<-data.frame('cell'=c(1:fit$spatial_list$n_g),D_gt,check.names = FALSE)
    
    #merge with grid
    D_gt1<-merge(D_gt,grid,by='cell')
    
    #df to spatialpoint df
    coordinates(D_gt1) <- ~ Lon + Lat
    crs(D_gt1)<-c(crs='+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')
    
    #reproject coordinates for plotting purposes
    #df_1<-spTransform(D_gt1,'+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')
    df_2<-data.frame(D_gt1)
    
    #x and y cells
    xycells<-as.integer(sqrt(dim(D_gt1)[1]))
    
    # create a template raster
    r1 <- raster(ext=extent(df_1),ncol=xycells, nrow=xycells) #c(15800,15800) 7000
    
    #create raster
    r2<-rasterize(D_gt1, r1 ,field=c('1'))
    crs(r2) <- CRS('+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')
    r2[r2==999]<-NA
    
    #create polygon to get boundaries of each strata
    r3<-as.data.frame(r2,xy=TRUE)
    
    #Alaska layer
    ebs_layers <- akgfmaps::get_base_layers(select.region = "ebs", set.crs = "EPSG:3338")
    ak_sppoly<-as(ebs_layers$akland, 'Spatial')
    
    #plot
    prf<-
    ggplot() +
      geom_raster(data=r3,aes(x=x,y=y,fill=layer))+
      #geom_polygon(data=r4,aes(x=long,y=lat,group=group), colour="black", fill=NA)+
      guides(fill=guide_colorbar(title.position = 'top', title.hjust = 0.5,ticks.colour = NA,frame.colour = 'black'))+
      scale_x_continuous(expand = c(0,0),position = 'bottom',
                         breaks = c(-175,-170,-165,-160,-155),sec.axis = dup_axis())+
      coord_sf(crs = '+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs',
               xlim = panel_extent$x-c(000000,100000),
               ylim = panel_extent$y-c(0,300000),
               label_axes = "-NE-")+
      scale_fill_viridis_c(option = 'A',name=('Spatial Random\nField deviations'),
                           guide = guide_colorbar(  frame.colour = "black",ticks.colour = 'black'),na.value=rgb(1, 0, 0, 0))+
      #scale_fill_gradientn(colours = wesanderson::wes_palette("Zissou1", 21, type = "continuous"),name=('Spatial Random\nField deviations'),
      #                      guide = guide_colorbar(  frame.colour = "black",ticks.colour = 'black'),na.value=rgb(1, 0, 0, 0))+
      theme(aspect.ratio = 1,panel.grid.major = element_blank(),
            panel.background = element_rect(fill = NA),panel.ontop = TRUE,
            legend.background =  element_rect(fill = "transparent", colour = "transparent"),legend.key.height= unit(20, 'points'),
            legend.key.width= unit(20, 'points'),axis.title = element_blank(),legend.position = 'none',
            panel.border = element_rect(fill = NA, colour = NA),legend.key = element_rect(color="black"),
            legend.spacing.y = unit(8, 'points'),
            axis.text=element_blank(),axis.ticks = element_blank(),
            plot.margin = margin(0.01,0.01,0.01,0.01), 
            axis.ticks.length = unit(-5,"points"),plot.title = element_text(size=12,hjust = 0.5,vjust=-5, face="bold"))+
      scale_y_continuous(expand = c(0,0),position = 'right',sec.axis = dup_axis())+
      labs(title=paste0(com),fill='')
  
    #store plot
    plot_list_rf[[isp]]<-prf
  } 
  
  #grid of plots
  pgrid1<-cowplot::plot_grid(plotlist = plot_list_rf, nrow = 2)
  
  #plot for common legend
  legend_rf<-
    ggplot()+
    geom_raster(data=r3,aes(x=x,y=y,fill=as.numeric(layer)))+
    scale_fill_viridis_c(breaks=range(as.numeric(r3$layer),na.rm = TRUE),labels=c("Low","High"),option = 'A',name=('Spatial Random\nField deviations'),
                         guide = guide_colorbar(  frame.colour = "black",ticks.colour = 'black'),na.value=rgb(1, 0, 0, 0))+
    guides(fill=guide_colorbar(title.position = 'top', title.hjust = 0.5,ticks.colour = NA,frame.colour = 'black'))+
    theme(legend.position = 'bottom')+
    labs(fill='')
  
  #legend
  legend1 <- cowplot::get_legend( 
    legend_rf + 
      theme(legend.position = "bottom") 
  ) 
  
  #save plot
  ragg::agg_png(paste0('./figures/SpatialRandomFields.png'),  width = 20, height = 7, units = "in", res = 300)
  print(cowplot::plot_grid(pgrid1, legend1, nrow = 2, rel_heights = c(1, .15)))
  dev.off()
  