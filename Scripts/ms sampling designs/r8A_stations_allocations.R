####################################################################
####################################################################
##    
##    simulate stations allocations for each sampling design
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
#x3$x<-as.integer(x3$coords.x1)
#x3$y<-as.integer(x3$coords.x2)
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
# BASELINE STRATA - current sampling design
###################################

#load baseline strata
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

###############
# get simulated allocations - survey simulation
################

#loop over sampling designs
for (samp in unique(samp_df$samp_scn)) { #sampling designs
  
  #samp<-unique(samp_df$samp_scn)[1]
  
  #samplins scenario number
  s<-match(samp,samp_df$samp_scn)
  
  #number of strata
  n_strata<-samp_df$n_strata[s]
  #number of samples
  n_samples<-samp_df$n_samples[s]
  
  if (grepl('base',samp)) { #if it contains base is baseline scenario
    
    strata<-as.data.frame(baseline_strata$cell_strata)[,c('cell','Lat','Lon','Stratum')]
    names(strata)[4]<-c('strata')
    aggregate(strata$cell,by=list(strata$strata),FUN=length)
    D8<-strata
    
    #allocations<-
    if(samp_df[s,'samp_scn']=='scnbase'){
      allocations<-data.frame('strata'=baseline_strata$n_samples$stratum,
                              'n_samples'=baseline_strata$n_samples$scnbase)
    }else{
      allocations<-data.frame('strata'=baseline_strata$n_samples$stratum,
                              'n_samples'=baseline_strata$n_samples$scnbase_bis)
    }
    
  } else {
    
    
    #load results_optimization
    load(file=paste0('./output/ms_optim_allocations_',samp,'.RData')) #list = c('result_list','ss_sample_allocations','ms_sample_allocations','samples_strata','cv_temp')
    load(file=paste0('./output/multisp_optimization_static_data.RData')) #df
    df<-df[,c("Lat",'Lon','cell')]
    
    
    #cell - strata
    strata<-all$result_list$solution$indices #all$result_list$sol_by_cell
    colnames(strata)<-c('cell','strata')
    strata<-strata[order(strata$cell),]
    
    #add a strata value to each cell
    D8<-merge(df,strata,by='cell',all.x=TRUE)
    D8<-D8[,c("cell","Lat","Lon","strata")]
    names(D8)[4]<-'strata'
    D8$strata<-as.numeric(D8$strata)
    D8$strata<-ifelse(is.na(D8$strata),999,D8$strata)
    
    #strata - n_samples
    allocations<-data.frame('strata'=all$samples_strata$strata,
                            'n_samples'=all$samples_strata$n_samples)
  }
  
  #load baseline strata and specify corner stations
  load('./output/baseline_strata.RData')
  baseline_strata$locations[grep('GF|HG|JI|IH|ON|QP|PO',baseline_strata$locations$stationid),]
  baseline_strata$locations$corner<-ifelse(grepl('GF|HG|JI|IH|ON|QP|PO',baseline_strata$locations$stationid),'TRUE','FALSE')
  
  #get strata and cells from the current allocation sampling design (stratum current design - strata optimized stratification)
  current<-merge(baseline_strata$locations,strata,by='cell',all.x=TRUE)
  
  #create raster polygon of stratified design
  #df to spatialpoint df
  D11<-D8
  coordinates(D11) <- ~ Lon + Lat
  crs(D11)<-c(crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  
  #reproject coordinates for plotting purposes
  df_1<-spTransform(D11,'+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')
  df_2<-data.frame(df_1)
  
  #x and y cells
  xycells<-as.integer(sqrt(dim(df_1)[1]))
  
  # create a template raster
  r1 <- raster(ext=extent(df_1),ncol=xycells, nrow=xycells) #c(15800,15800) 7000
  
  #create raster
  r2<-rasterize(df_1, r1 ,field=c('strata'))
  crs(r2) <- CRS('+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')
  r2[r2==999]<-NA
  
  #create polygon to get boundaries of each strata
  r3<-as.data.frame(r2,xy=TRUE)
  r4<-rasterToPolygons(r2$layer,dissolve=TRUE)
  r5<-spTransform(r4,"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

  #replicates n_surveys x n_years
  rep_sur<-n_sur*length(yrs)
  
  #create array to store results
  #to store survey samples
  scn_allocations <- array(data = NA, dim = c(as.numeric(n_samples)*rep_sur,
                                              length(c('Lat','Lon','cell','strata','sur')),
                                              #length(yrs), 
                                              length(c('systematic','random','sb'))),
                           dimnames = list(1:(as.numeric(n_samples)*rep_sur),
                                           c('Lat','Lon','cell','strata','sur'),
                                           #c(yrs),
                                           c('sys','rand','sb')))
  
  #to store points
  dfcurrent<-data.frame(matrix(NA,nrow=0,ncol=5))
  colnames(dfcurrent)<-c('Lon','Lat','cell','strata','sur')
  dfspb<-dfrandom<-dfcurrent
  
  #list to store results
  str_alloc<-list()
  
  #loop over strata
  for (str in allocations$strata) {
    
    #str<-allocations$strata[1]
    
    #print simulation to check progress
    cat(paste(" #############  sampling design",samp ,'- strata',str ," #############\n"))
    
    #number of samples required by str
    n_i<-allocations[which(allocations$strata==str),'n_samples']
    
    #subset cells grid by strata
    D9<-subset(D8,strata==str)
    
    ########################
    ### RANDOM SAMPLING
    ########################
    
    # Initializing parallel backend
    cl <- makeCluster(detectCores()-1)
    registerDoParallel(cl)
    
    
    # Parallelizing the loop
    dfrandom <- foreach(sur = 1:rep_sur, .combine=rbind) %dopar% {
      #cat(paste(" #############  random sampling", '- survey', sur, " #############\n"))
      
      D10 <- D9[sample(nrow(D9), size = n_i, replace = FALSE), ]
      D10 <- D10[, c('Lon', 'Lat', 'cell', 'strata')]
      D10$sur <- sur
      
      return(D10)
    }
    
    # Stopping the parallel backend
    stopCluster(cl)
    
    ########################
    ### SPATIALLY BALANCE SAMPLING
    ########################
    
    #get distance among points
    dis_la <- as.matrix(dist(cbind(D9$Lon, D9$Lat)))
    D9_st_sf <- st_as_sf(D9, coords = c("Lon", "Lat"))
    dis_la_sf <- st_distance(D9_st_sf)
    
    #PWD
    con <- rep(0, nrow(dis_la))
    stand_dist_la_pwd <- Spbsampling::stprod(mat = dis_la, con = con)$mat
    
    # Initializing parallel backend
    cl <- makeCluster(detectCores()-1)
    registerDoParallel(cl)
    
    # Create an empty list to store the results
    dfspb <- foreach(sur = 1:rep_sur, .combine = rbind) %dopar% {
      
      #cat(paste(" #############  spatially-balanced sampling", '- survey', sur, " #############\n"))
      
      s_pwd_la <- Spbsampling::pwd(dis = stand_dist_la_pwd, n = n_i)$s
      D10 <- D9[s_pwd_la[1, ], ]
      
      D10$sur <- sur
      # spatially-balancing strata samples
      spbsamp <- D10[, c('Lon', 'Lat', 'cell', 'strata', 'sur')]
      
      return(spbsamp)
    }
    
    # Stopping the parallel backend
    stopCluster(cl)
    
    #######################
    ### SYSTEMATIC SAMPLING
    #######################
    
    # Initializing parallel backend
    cl <- makeCluster(detectCores()-1)
    registerDoParallel(cl)
    
    # Create an empty list to store the results
    dfcurrent <- foreach(sur = 1:rep_sur, .combine = rbind) %dopar% {
      
      #cat(paste(" #############  systematic sampling", '- survey', sur, " #############\n"))
      
      #if current sampling design
      if (samp_df[s, 'samp_scn'] == 'scnbase') {
        pointsc <- current[which(current$stratum == str), c('longitude', 'latitude', 'cell', 'stratum')]
        names(pointsc) <- c('Lon', 'Lat', 'cell', 'strata')
        
        #if current sampling design w/o corner
      } else if (samp_df[s, 'samp_scn'] == 'scnbase_bis') {
        pointsc <- current[which(current$corner == FALSE & current$stratum == str), c('longitude', 'latitude', 'cell', 'stratum')]
        names(pointsc) <- c('Lon', 'Lat', 'cell', 'strata')
        
        #if optimized sampling design
      } else {
        
        sys <- current[which(current$strata == str), c('longitude', 'latitude', 'cell', 'strata')]
        names(sys) <- c('Lon', 'Lat', 'cell', 'strata')
        
        #if more required samples than available
        if (nrow(sys) < n_i) {
          
          #duplicate df strata
          dff <- D9
          
          #df removing available samples from current design
          dff <- subset(dff, !(cell %in% sys$cell))
          
          #cells to complete the required cells
          ii <- n_i - nrow(sys)
          
          #loop over the required samples using random
          selcell <- sample(dff$cell, ii)
          
          if (nrow(subset(D9, cell %in% c(selcell, sys$cell))) == 0) {
            
            ss <- sample(1:nrow(D9), size = n_i, replace = FALSE)
            pointsc <- D9[ss, c('Lon', 'Lat', 'cell', 'strata')]
            
            #subset points if equal to required number of samples, if not ERROR and rerun iteration
          } else if (nrow(subset(D9, cell %in% c(selcell, sys$cell))) != n_i) {
            flag <- TRUE
            stop("-- different number of points on the current approach", call. = FALSE)
          } else {
            pointsc <- subset(D9, cell %in% c(selcell, sys$cell))[, c('Lon', 'Lat', 'cell', 'strata')]
            names(pointsc) <- c('Lon', 'Lat', 'cell', 'strata')
          }
          
          #else there are enough samples to get from the current sampling design    
        } else {
          ss <- sample(1:nrow(sys), size = n_i, replace = FALSE)
          pointsc <- sys[ss, c('Lon', 'Lat', 'cell', 'strata')]
        }
      }
      
      #systematic strata samples
      syssamp <- pointsc[, c('Lon', 'Lat', 'cell', 'strata')]
      syssamp$sur <- sur
      
      return(syssamp)
    }
    
    #close cluster  
    stopCluster(cl)
    
    #store into a list
    str_alloc[[str]]<-list(dfrandom = dfrandom, dfspb = dfspb, dfcurrent = dfcurrent)
    
  }
  
  #reshape
  cur<-dplyr::bind_rows(lapply(str_alloc, function(x) x[['dfcurrent']]))
  rand<-dplyr::bind_rows(lapply(str_alloc, function(x) x[['dfrandom']]))
  spb<-dplyr::bind_rows(lapply(str_alloc, function(x) x[['dfspb']]))
  
  #append results 
  scn_allocations[,,'sys'] <- unlist(cur)
  scn_allocations[,,'rand'] <- unlist(rand)
  scn_allocations[,,'sb'] <- unlist(spb)
  
  #remove
  rm(dfcurrent,dfrandom,dfspb,str_alloc,cur,rand,spb)
  
  #store station allocations
  save(scn_allocations, file = paste0('./output/survey_allocations_',samp,'.RData')) 
  
}


#READ HOW MANY SYS NEED TO BE REMOVED OR ADDED

  
  
  
  
  
  
  
  
  ###############
  # get simulated allocations - survey simulation
  ################
  n_sur<-100
  st_change<-data.frame(matrix(NA,ncol=5,nrow=0))
  colnames(st_change)<-c('existing','opt','strata','sur','samp')
  
  #loop over sampling designs
  for (samp in unique(samp_df$samp_scn)[1:3]) { #sampling designs
    
    #samp<-unique(samp_df$samp_scn)[1]
    
    #samplins scenario number
    s<-match(samp,samp_df$samp_scn)
    
    #number of strata
    n_strata<-samp_df$n_strata[s]
    #number of samples
    n_samples<-samp_df$n_samples[s]
    
    if (grepl('base',samp)) { #if it contains base is baseline scenario
      
      strata<-as.data.frame(baseline_strata$cell_strata)[,c('cell','Lat','Lon','Stratum')]
      names(strata)[4]<-c('strata')
      aggregate(strata$cell,by=list(strata$strata),FUN=length)
      D8<-strata
      
      #allocations<-
      if(samp_df[s,'samp_scn']=='scnbase'){
        allocations<-data.frame('strata'=baseline_strata$n_samples$stratum,
                                'n_samples'=baseline_strata$n_samples$scnbase)
      }else{
        allocations<-data.frame('strata'=baseline_strata$n_samples$stratum,
                                'n_samples'=baseline_strata$n_samples$scnbase_bis)
      }
      
    } else {
      
      
      #load results_optimization
      load(file=paste0('./output/ms_optim_allocations_',samp,'.RData')) #list = c('result_list','ss_sample_allocations','ms_sample_allocations','samples_strata','cv_temp')
      load(file=paste0('./output/multisp_optimization_static_data.RData')) #df
      df<-df[,c("Lat",'Lon','cell')]
      
      
      #cell - strata
      strata<-all$result_list$solution$indices #all$result_list$sol_by_cell
      colnames(strata)<-c('cell','strata')
      strata<-strata[order(strata$cell),]
      
      #add a strata value to each cell
      D8<-merge(df,strata,by='cell',all.x=TRUE)
      D8<-D8[,c("cell","Lat","Lon","strata")]
      names(D8)[4]<-'strata'
      D8$strata<-as.numeric(D8$strata)
      D8$strata<-ifelse(is.na(D8$strata),999,D8$strata)
      
      #strata - n_samples
      allocations<-data.frame('strata'=all$samples_strata$strata,
                              'n_samples'=all$samples_strata$n_samples)
    }
    
    #load baseline strata and specify corner stations
    load('./output/baseline_strata.RData')
    baseline_strata$locations[grep('GF|HG|JI|IH|ON|QP|PO',baseline_strata$locations$stationid),]
    baseline_strata$locations$corner<-ifelse(grepl('GF|HG|JI|IH|ON|QP|PO',baseline_strata$locations$stationid),'TRUE','FALSE')
    
    #get strata and cells from the current allocation sampling design (stratum current design - strata optimized stratification)
    current<-merge(baseline_strata$locations,strata,by='cell',all.x=TRUE)
    
    #create raster polygon of stratified design
    #df to spatialpoint df
    D11<-D8
    coordinates(D11) <- ~ Lon + Lat
    crs(D11)<-c(crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    
    #reproject coordinates for plotting purposes
    df_1<-spTransform(D11,'+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')
    df_2<-data.frame(df_1)
    
    #x and y cells
    xycells<-as.integer(sqrt(dim(df_1)[1]))
    
    # create a template raster
    r1 <- raster(ext=extent(df_1),ncol=xycells, nrow=xycells) #c(15800,15800) 7000
    
    #create raster
    r2<-rasterize(df_1, r1 ,field=c('strata'))
    crs(r2) <- CRS('+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')
    r2[r2==999]<-NA
    
    #create polygon to get boundaries of each strata
    r3<-as.data.frame(r2,xy=TRUE)
    r4<-rasterToPolygons(r2$layer,dissolve=TRUE)
    r5<-spTransform(r4,"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    
    #replicates n_surveys x n_years
    rep_sur<-n_sur*length(yrs)
    
    #create array to store results
    #to store survey samples
    scn_allocations <- array(data = NA, dim = c(as.numeric(n_samples)*rep_sur,
                                                length(c('Lat','Lon','cell','strata','sur')),
                                                #length(yrs), 
                                                length(c('systematic','random','sb'))),
                             dimnames = list(1:(as.numeric(n_samples)*rep_sur),
                                             c('Lat','Lon','cell','strata','sur'),
                                             #c(yrs),
                                             c('sys','rand','sb')))
    
    #to store points
    dfcurrent<-data.frame(matrix(NA,nrow=0,ncol=5))
    colnames(dfcurrent)<-c('Lon','Lat','cell','strata','sur')
    dfspb<-dfrandom<-dfcurrent
    
    #list to store results
    #str_alloc<-list()
    
    #loop over strata
    for (str in allocations$strata) {
      
      #str<-allocations$strata[1]
      
      #print simulation to check progress
      cat(paste(" #############  sampling design",samp ,'- strata',str ," #############\n"))
      
      #number of samples required by str
      n_i<-allocations[which(allocations$strata==str),'n_samples']
      
      #subset cells grid by strata
      D9<-subset(D8,strata==str)
      
      #######################
      ### SYSTEMATIC SAMPLING
      #######################
      
      
      # Create an empty list to store the results
      
      for (sur in 1:rep_sur) {
        
        cat(paste(" #############  sampling design",samp ,'- strata',str ,' sur',sur," #############\n"))
      
          
          #sys <- current[which(current$strata == str), c('longitude', 'latitude', 'cell', 'strata')]
          sys <- current[which(current$strata == str), c('Lon', 'Lat', 'cell', 'strata')]
          names(sys) <- c('Lon', 'Lat', 'cell', 'strata')
          
          
          st_change1<-data.frame('existing'=nrow(sys),
                                'opt'=n_i,
                                'strata'=str,
                                'sur'=sur,
                                'samp'=samp)
          
          st_change<-rbind(st_change,st_change1)
          
        }
        
      }
      
    }
  
  rep_sur
  st_change
  
  
  
  #READ HOW MANY SYS NEED TO BE REMOVED OR ADDED
  all<-data.frame(matrix(NA,ncol=4,nrow=0))
  colnames(all)<-c('strata','sur','cell')
  
  for (s in 1:3) {
    
    #s<-1
    
    load(file =paste0('./output/survey_allocations_scn',s,'.RData'))
    
    #dimnames(scn_allocations)
    
    alloca<-data.frame(scn_allocations[,,'sys'])
    
    alloca2<-aggregate(cell ~ strata + sur,alloca,FUN=length)
    alloca2$samp<-s
    
    all<-rbind(all,alloca2)
    
    # for (sur in 1:100) {
    #   
    #   #sur<-3
    #   
    #   alloca1<-alloca[which(alloca$sur==sur),'cell'] #c('Lat','Lon')
    #   
    #   alloca2<-aggregate(cell ~ strata + sur,alloca,FUN=length)
    #   alloca2$samp<-s
    #   
    #   all<-rbind(all,alloca2)
    #   
    #   #alloca1 what we sampled
    #   #baseline_strata$locations$cell what we had
    #   
    #   sort(baseline_strata$locations$cell)
    #   sort(alloca1)
    #   length(sort(setdiff(baseline_strata$locations$cell,alloca1))) #these stations were dropped
    #   length(sort(setdiff(alloca1,baseline_strata$locations$cell))) #these stations were added
    #   
    #   
    #   #check how many samples per strata vs available
    #   }
      
    }
  
  
  head(all);head(st_change1)
  st_change$samp<-gsub('scn','',st_change$samp)

  all1<-merge(all,st_change,by=c('strata','sur','samp'))  
  head(all1)  
  
  all1$opt<-all1$cell
  all1$change<-all1$existing/all1$opt
  all1$diff<-all1$existing-all1$opt
  
  nrow(all1[which(all1$change<=1.2 & all1$change>=0.8 ),])/
  nrow(all1)
  
  all2<-all1
  all2$sur<-NULL
  
  all3<-unique(all2)
  all3$samp<-as.factor(all3$samp)
  #scn1- + varSBT'
  #scn2 opt depth','opt varSBT','opt depth
  
  ggplot()+
    geom_bar(data=all3,aes(x=diff))+
    geom_vline(xintercept=0,linetype='dashed')+
    facet_wrap(~samp)
  
  qggplot()+
    geom_bar(data=all3,aes(x=diff,color=samp), width=.5, position = "dodge")+
    geom_vline(xintercept=0,linetype='dashed')
  
  ggplot()+
    geom_boxplot(data=subset(all3,diff <0 ),aes(x=samp,y=diff,color=samp,group=samp))+
    geom_boxplot(data=subset(all3,diff >0 ),aes(x=samp,y=diff,color=samp,group=samp))
    #geom_vline(xintercept=0,linetype='dashed')
  
  
  all3$absdiff<-abs(all3$diff)
  mean(all3$absdiff)

  #% of stratas that needed to add stations from the historical systematic grid
  nrow(subset(all3,diff <0 ))/nrow(all3)
  mean(subset(all3,diff <0 )[,'diff'])
  #% of stratas that needed to drop stations from the historical systematic grid
  nrow(subset(all3,diff >0))/nrow(all3)
  mean(subset(all3,diff >0 )[,'diff'])
  #same
  nrow(subset(all3,diff ==0))/nrow(all3)
  #% of stratas that needed to drop stations from the historical systematic grid
  nrow(subset(all3,diff <=3 & diff >=-3))/nrow(all3)
  #% of stratas that needed to drop stations from the historical systematic grid
  nrow(subset(all3,diff <=3 & diff >=0))/nrow(all3)
  nrow(subset(all3,diff <=0 & diff >=-3))/nrow(all3)
  
  #mean absolute difference 3.9
  #37.7% the existing has more than needed in OPT
  #51.1% the existing has less than needed in OPT
  #11.1% is equal
  
  
  
  
  p<-
  ggplot()+
    #geom_point(data=all1,aes(x=samp,y=change,color=samp))+
    theme_bw()+
    labs(y=paste0('mean of dropped (top) and','\n','added (bottom) stations'))+
    geom_hline(yintercept = 0,linetype='dashed')+
    geom_boxplot(data=subset(all3,diff <0 ),aes(x=samp,y=diff,fill=samp))+
    geom_boxplot(data=subset(all3,diff >0 ),aes(x=samp,y=diff,fill=samp))+
    scale_fill_manual(values=c('1'='#9B59B6','2'='#3498DB','3'='#1ABC9C'),
                      labels = c('opt depth','opt varSBT','opt depth + varSBT'),name='stratification')+
    scale_x_discrete(labels = c('opt depth','opt varSBT','opt depth + varSBT'))+
    theme(axis.title.x = element_blank())+
    theme(panel.grid.minor = element_line(linetype=2, color='grey90'),
          legend.key.width = unit(2.5, "lines"),
          legend.key.size = unit(20, 'points'),
          legend.direction = 'vertical',
          legend.text = element_text(size=12),
          legend.title = element_text(size=12),
          legend.spacing = unit(1, "cm"),
          legend.box.spacing = unit(0.01, "cm"),
          strip.background = element_blank(),
          # legend.background = element_blank(),
          # legend.box = 'horizontal',
          # legend.position = 'bottom',
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          strip.text = element_blank(),
          axis.title.x = element_blank(),
          axis.text = element_text(size = 10))+
    # Add text annotations
    annotate("text", x = 2, y =9, label = "51% strata dropped - mean 4 stations", size = 5, hjust = 0.5) +
    annotate("text", x = 2, y = -12, label = "38% strata added - mean 5 stations", size = 5, hjust = 0.5)
  
  #save index plot
  ragg::agg_png(paste0('./figures/changes_opt_existing.tiff'), width = 7, height = 4, units = "in", res = 300)
  p
  dev.off()
  
  all3$samp<-factor(all3$samp,levels=c('3','2','1'))
  
  
  ggplot()+
    #geom_point(data=all1,aes(x=samp,y=change,color=samp))+
    theme_bw()+
    labs(y='ratio of sampling stations per strata (existing/optimized)')+
    geom_vline(xintercept = 0,linetype='dashed')+
    geom_boxplot(data=subset(all3,diff<0),aes(x=diff,group=1,y=1))+
    geom_boxplot(data=subset(all3,diff>0),aes(x=diff,group=1,y=1))+
    geom_jitter(data=subset(all3),aes(x=diff,color=samp,y=1))+
    #geom_boxplot(data=subset(all3,diff >0 ),aes(y=samp,x=diff,fill=samp))+
    scale_fill_manual(values=c('1'='#9B59B6','2'='#3498DB','3'='#1ABC9C'),
                      name='stratification')+ #labels = c('opt depth + varSBT','opt varSBT','opt depth'),
    # scale_color_manual(values=c('1'='#9B59B6','2'='#3498DB','3'='#1ABC9C'),
    #                    labels = c('opt depth + varSBT','opt varSBT','opt depth'),name='stratification')+
    theme(axis.title.x = element_blank())+
    theme(panel.grid.minor = element_line(linetype=2, color='grey90'),
          legend.key.width = unit(2.5, "lines"),
          legend.key.size = unit(20, 'points'),
          legend.direction = 'vertical',
          legend.text = element_text(size=12),
          legend.title = element_text(size=12),
          legend.spacing = unit(1, "cm"),
          legend.box.spacing = unit(0.01, "cm"),
          strip.background = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          # legend.background = element_blank(),
          # legend.box = 'horizontal',
          # legend.position = 'bottom',
          strip.text = element_blank(),
          axis.title.x = element_blank(),
          axis.text = element_text(size = 10)) 
  

  
  
  all2<-
  aggregate(diff ~ sur + samp,all1,FUN=sum)
    