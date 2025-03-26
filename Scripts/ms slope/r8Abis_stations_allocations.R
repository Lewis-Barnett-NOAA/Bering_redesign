####################################################################
####################################################################
##    
##    simulate stations allocations for each sampling design
##    danielvilasgonzalez@gmail.com/dvilasg@uw.edu
##    
##     spatially balanced / random
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
#out_dir<-'C:/Users/Daniel.Vilas/Work/Adapting Monitoring to a Changing Seascape/'
out_dir<-'/Users/daniel/Work/Adapting Monitoring to a Changing Seascape/'
setwd(out_dir)

#' #selected species
#' spp<-c('Limanda aspera',
#'        'Gadus chalcogrammus',
#'        'Gadus macrocephalus',
#'        'Atheresthes stomias',
#'        'Reinhardtius hippoglossoides',
#'        'Lepidopsetta polyxystra',
#'        'Hippoglossoides elassodon',
#'        'Pleuronectes quadrituberculatus',
#'        'Hippoglossoides robustus',
#'        'Boreogadus saida',
#'        'Eleginus gracilis',
#'        'Anoplopoma fimbria',
#'        'Chionoecetes opilio',
#'        'Paralithodes platypus',
#'        'Paralithodes camtschaticus',
#'        #'Lepidopsetta sp.',
#'        'Chionoecetes bairdi',
#'        'Sebastes alutus',
#'        'Sebastes melanostictus',
#'        'Atheresthes evermanni',
#'        'Sebastes borealis',
#'        'Sebastolobus alascanus',
#'        'Glyptocephalus zachirus',
#'        'Bathyraja aleutica')
#' 
#' #common names
#' spp1<-c('Yellowfin sole',
#'         'Alaska pollock',
#'         'Pacific cod',
#'         'Arrowtooth flounder',
#'         'Greenland turbot',
#'         'Northern rock sole',
#'         'Flathead sole',
#'         'Alaska plaice',
#'         'Bering flounder',
#'         'Arctic cod',
#'         'Saffron cod',
#'         'Sablefish',
#'         'Snow crab',
#'         'Blue king crab',
#'         'Red king crab',
#'         'Tanner crab',
#'         'Pacific ocean perch',
#'         'Rougheye and blackspotted rockfish',
#'         'Kamchatka flounder',
#'         'Shortraker rockfish',
#'         'Shortspine thornyhead',
#'         'Rex sole',
#'         'Aleutian skate')
#' 
#' 
#' #spp df
#' spp_name<-data.frame('spp'=spp,
#'                      'common'=spp1) 
#' 
#' 
#' #df spp, number and target variables
#' df_spp<-data.frame('spp'=spp,
#'                    'n'=c(1:length(spp)),
#'                    'Y'=paste0('Y',c(1:length(spp))))
#' 
#' #number sp
#' n_spp<-length(spp)

#yrs
yrs<-c(2002:2016)

###################################
# LOAD GRID EBS (remember to keep the same order as in fit_model if multiple grids)
###################################

#load grid of NBS and EBS
load('./extrapolation grids/northern_bering_sea_grid.rda')
load('./extrapolation grids/eastern_bering_sea_grid.rda')
load('./extrapolation grids/bering_sea_slope_grid.rda')
names(bering_sea_slope_grid)[4]<-'Stratum'
bering_sea_slope_grid$Stratum<-NA
grid<-as.data.frame(rbind(data.frame(northern_bering_sea_grid,region='NBS'),
                          data.frame(eastern_bering_sea_grid,region='EBS'),
                          data.frame(bering_sea_slope_grid,region='SBS')))
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
#grid_ebs<-grid.ebs_year[which(grid.ebs_year$region != 'EBSslope' & grid.ebs_year$Year %in% yrs),]
grid_ebs<-grid.ebs_year[which(grid.ebs_year$Year %in% yrs),]
dim(grid_ebs)

###################################
# Sampling designs (from script #11) 
###################################

#sampling scenarios
samp_df<-expand.grid(type=c('static','dynamic'),#c('all','cold','warm'),
                     region=c('EBS','EBS+NBS','EBS+SBS','EBS+NBS+SBS'),
                     strat_var=c('varTemp','Depth'), #,'varTemp_forced','Depth_forced' #LonE and combinations
                     target_var=c('sumDensity'), #,'sqsumDensity'
                     n_samples=c(376), #c(300,500) 520 (EBS+NBS+CRAB);26 (CRAB); 350 (EBS-CRAB); 494 (NBS-CRAB)
                     n_strata=c(10),
                     domain=1) #c(5,10,15)

#samples slope to add dummy approach
samp_slope <- subset(samp_df, grepl("SBS", region))
samp_slope$strat_var<-paste0(samp_slope$strat_var,'_dummy')

#add with dummy approach
samp_df<-rbind(samp_df,samp_slope)

#add scenario number
samp_df$samp_scn<-paste0(paste0('scn',1:nrow(samp_df)))

#number of surveys
n_sur<-100

###############
# get simulated allocations - survey simulation
################

#loop over sampling designs
for (s in 1:nrow(samp_df)) { #sampling designs
  
  #s<-1
  
  #samplins scenario number
  #s<-match(samp,samp_df$samp_scn)
  
  #number of strata
  n_strata<-samp_df$n_strata[s]
  #number of samples
  n_samples<-samp_df$n_samples[s]
  
  # if (grepl('base',samp)) { #if it contains base is baseline scenario
  #   
  #   strata<-as.data.frame(baseline_strata$cell_strata)[,c('cell','Lat','Lon','Stratum')]
  #   names(strata)[4]<-c('strata')
  #   aggregate(strata$cell,by=list(strata$strata),FUN=length)
  #   D8<-strata
  #   
  #   #allocations<-
  #   if(samp_df[s,'samp_scn']=='scnbase'){
  #     allocations<-data.frame('strata'=baseline_strata$n_samples$stratum,
  #                             'n_samples'=baseline_strata$n_samples$scnbase)
  #   }else{
  #     allocations<-data.frame('strata'=baseline_strata$n_samples$stratum,
  #                             'n_samples'=baseline_strata$n_samples$scnbase_bis)
  #   }
  #   
  # } else {
    
  if (samp_df[s,'type']=='static') {
    #load multispecies data
    load(paste0('./output slope/multisp_optimization_static_data_ebsnbs_slope_st.RData')) #df
    regime<-c('all')
  } else {
    #load multispecies data
    load(paste0('./output slope/multisp_optimization_static_data_ebsnbs_slope_dyn.RData')) #df
    regime<-c('cold','warm')
  }
  

  for (r in regime) {
    
    #r<-regime[1]
    
    #load results_optimization
    load(file=paste0("./output slope/ms_optim_allocations_ebsnbs_slope_",samp_df[s,'samp_scn'],'_',r,".RData")) #list = c('result_list','ss_sample_allocations','ms_sample_allocations','samples_strata','cv_temp')
    #load(file=paste0('./output slope/multisp_optimization_static_data.RData')) #df
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
  
  # #load baseline strata and specify corner stations
  # load('./output/baseline_strata.RData')
  # baseline_strata$locations[grep('GF|HG|JI|IH|ON|QP|PO',baseline_strata$locations$stationid),]
  # baseline_strata$locations$corner<-ifelse(grepl('GF|HG|JI|IH|ON|QP|PO',baseline_strata$locations$stationid),'TRUE','FALSE')
  # 
  # #get strata and cells from the current allocation sampling design (stratum current design - strata optimized stratification)
  # current<-merge(baseline_strata$locations,strata,by='cell',all.x=TRUE)
  
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
                                              length(c('random','sb'))),
                           dimnames = list(1:(as.numeric(n_samples)*rep_sur),
                                           c('Lat','Lon','cell','strata','sur'),
                                           #c(yrs),
                                           c('rand','sb')))
  
  #to store points
  dfrandom<-data.frame(matrix(NA,nrow=0,ncol=5))
  colnames(dfrandom)<-c('Lon','Lat','cell','strata','sur')
  dfspb<-dfrandom
  
  #list to store results
  str_alloc<-list()
  
  #loop over strata
  for (str in allocations$strata) {
    
    #str<-allocations$strata[1]
    
    #print simulation to check progress
    cat(paste(" #############  sampling design",s ,'- strata',str ," #############\n"))
    
    #number of samples required by str
    n_i<-allocations[which(allocations$strata==str),'n_samples']
    
    #subset cells grid by strata
    D9<-subset(D8,strata==str)
    
    ########################
    ### RANDOM SAMPLING
    ########################
    
    # Initializing parallel backend
    #cl <- makeCluster(detectCores()-3)
    cl <- makeCluster(2)
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
    #cl <- makeCluster(detectCores()-3)
    cl <- makeCluster(2)
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
    
    # # Initializing parallel backend
    # cl <- makeCluster(detectCores()-1)
    # registerDoParallel(cl)
    # 
    # # Create an empty list to store the results
    # dfcurrent <- foreach(sur = 1:rep_sur, .combine = rbind) %dopar% {
    #   
    #   #cat(paste(" #############  systematic sampling", '- survey', sur, " #############\n"))
    #   
    #   #if current sampling design
    #   if (samp_df[s, 'samp_scn'] == 'scnbase') {
    #     pointsc <- current[which(current$stratum == str), c('longitude', 'latitude', 'cell', 'stratum')]
    #     names(pointsc) <- c('Lon', 'Lat', 'cell', 'strata')
    #     
    #     #if current sampling design w/o corner
    #   } else if (samp_df[s, 'samp_scn'] == 'scnbase_bis') {
    #     pointsc <- current[which(current$corner == FALSE & current$stratum == str), c('longitude', 'latitude', 'cell', 'stratum')]
    #     names(pointsc) <- c('Lon', 'Lat', 'cell', 'strata')
    #     
    #     #if optimized sampling design
    #   } else {
    #     
    #     sys <- current[which(current$strata == str), c('longitude', 'latitude', 'cell', 'strata')]
    #     names(sys) <- c('Lon', 'Lat', 'cell', 'strata')
    #     
    #     #if more required samples than available
    #     if (nrow(sys) < n_i) {
    #       
    #       #duplicate df strata
    #       dff <- D9
    #       
    #       #df removing available samples from current design
    #       dff <- subset(dff, !(cell %in% sys$cell))
    #       
    #       #cells to complete the required cells
    #       ii <- n_i - nrow(sys)
    #       
    #       #loop over the required samples using random
    #       selcell <- sample(dff$cell, ii)
    #       
    #       if (nrow(subset(D9, cell %in% c(selcell, sys$cell))) == 0) {
    #         
    #         ss <- sample(1:nrow(D9), size = n_i, replace = FALSE)
    #         pointsc <- D9[ss, c('Lon', 'Lat', 'cell', 'strata')]
    #         
    #         #subset points if equal to required number of samples, if not ERROR and rerun iteration
    #       } else if (nrow(subset(D9, cell %in% c(selcell, sys$cell))) != n_i) {
    #         flag <- TRUE
    #         stop("-- different number of points on the current approach", call. = FALSE)
    #       } else {
    #         pointsc <- subset(D9, cell %in% c(selcell, sys$cell))[, c('Lon', 'Lat', 'cell', 'strata')]
    #         names(pointsc) <- c('Lon', 'Lat', 'cell', 'strata')
    #       }
    #       
    #       #else there are enough samples to get from the current sampling design    
    #     } else {
    #       ss <- sample(1:nrow(sys), size = n_i, replace = FALSE)
    #       pointsc <- sys[ss, c('Lon', 'Lat', 'cell', 'strata')]
    #     }
    #   }
    #   
    #   #systematic strata samples
    #   syssamp <- pointsc[, c('Lon', 'Lat', 'cell', 'strata')]
    #   syssamp$sur <- sur
    #   
    #   return(syssamp)
    # }
    # 
    # #close cluster  
    # stopCluster(cl)
    
    #store into a list
    str_alloc[[str]]<-list(dfrandom = dfrandom, dfspb = dfspb)
    
  }
  
  #reshape
  # cur<-dplyr::bind_rows(lapply(str_alloc, function(x) x[['dfcurrent']]))
  rand<-dplyr::bind_rows(lapply(str_alloc, function(x) x[['dfrandom']]))
  spb<-dplyr::bind_rows(lapply(str_alloc, function(x) x[['dfspb']]))
  
  #append results 
  #scn_allocations[,,'sys'] <- unlist(cur)
  scn_allocations[,,'rand'] <- unlist(rand)
  scn_allocations[,,'sb'] <- unlist(spb)
  
  #remove
  rm(dfrandom,dfspb,str_alloc,rand,spb)
  
  #store station allocations
  save(scn_allocations, file = paste0('./output slope/survey_allocations_',samp_df[s,'samp_scn'],'_',r,'.RData')) 
  }
}
