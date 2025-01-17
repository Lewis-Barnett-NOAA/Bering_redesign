####################################################################
####################################################################
##    
##    simulate data and survey for historical and projected years
##    prepare estimates to compute design-based indices
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

#setseed
set.seed(6)

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
       #'Lepidopsetta sp.',
       'Chionoecetes bairdi',
       'Sebastes alutus',
       'Sebastes melanostictus',
       'Atheresthes evermanni',
       'Sebastes borealis',
       'Sebastolobus alascanus',
       'Glyptocephalus zachirus',
       'Bathyraja aleutica')

#yrs
#yrs<-setdiff(1982:2022,2020)


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
yrs<-c(2002:2016)
grid_ebs<-grid.ebs_year[which(grid.ebs_year$Year %in% yrs),]
dim(grid_ebs)

#FIND SLOPE CELLS DEEPER than 400m
load(file = './data processed/grid_EBS_NBS.RData') #grid.ebs_year$region
grid_slp<-subset(grid.ebs_year,region=='EBSslope' & Year=='1982')
dim(grid_slp)
dim(grid_slp[which(grid_slp$DepthGEBCO<=400),])
ok_slp_cells<-as.numeric(row.names(grid_slp)[which(grid_slp$DepthGEBCO<=400)])
rem_slp_cells<-as.numeric(row.names(grid_slp)[which(grid_slp$DepthGEBCO>400)])

#load grid of NBS and EBS
load('./extrapolation grids/northern_bering_sea_grid.rda')
load('./extrapolation grids/eastern_bering_sea_grid.rda')
load('./extrapolation grids/bering_sea_slope_grid.rda')
colnames(bering_sea_slope_grid)[4]<-'Stratum'
bering_sea_slope_grid$Stratum<-NA
grid<-as.data.frame(rbind(data.frame(northern_bering_sea_grid,region='NBS'),
                          data.frame(eastern_bering_sea_grid,region='EBS'),
                          data.frame(bering_sea_slope_grid,region='SBS')))
grid$cell<-1:nrow(grid)
grid2<-grid

nbs_km2<-sum(as.data.frame(northern_bering_sea_grid)$Area_in_survey_km2)
ebs_km2<-sum(as.data.frame(eastern_bering_sea_grid)$Area_in_survey_km2)
sbs_km2<-sum(grid[which(grid$cell %in% ok_slp_cells & grid$region == 'SBS'),'Area_in_survey_km2']) #adjust area sbs to represent <400km

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

################
# SAMPLES PER REGION
################

n_sur<-100

region_cells<-data.frame(matrix(NA,nrow = 0,ncol = length(c('NBS_sb','EBS_sb','SBS_sb','NBS_rand','EBS_rand','SBS_rand','samp','strat_var','type','region','regime','sur'))))
colnames(region_cells)<-c('NBS_sb','EBS_sb','SBS_sb','NBS_rand','EBS_rand','SBS_rand','samp','strat_var','type','region','regime','sur')

for (s in 1:nrow(samp_df)) { #nrow(samp_df)
  
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
  
  #r<-regime[1]  
  cat(paste0('########## ',s,' - ',r,' ##########\n'))    
  #scn_allocations
  load(file = paste0('./output slope/survey_allocations_',samp_df[s,'samp_scn'],'_',r,'.RData')) 
  
  for (sur in 1:n_sur) {
    
    #sur<-1
    
    #spatially-balanced
    sb_cell<-data.frame(scn_allocations[which(scn_allocations[, "sur", "sb"] == sur), , "sb"])
    #random
    rand_cell <- data.frame(scn_allocations[which(scn_allocations[, "sur", "rand"] == sur), , "rand"])
    
    #counts per survey
    isur<-
      data.frame('NBS_sb'=nrow(sb_cell[which(sb_cell$cell<= 15180),]),
                 'EBS_sb'=nrow(sb_cell[which(sb_cell$cell<= 53464 & sb_cell$cell>= 15181),]),
                 'SBS_sb'=nrow(sb_cell[which(sb_cell$cell>= 53465),]),
                 'NBS_rand'=nrow(rand_cell[which(rand_cell$cell<= 15180),]),
                 'EBS_rand'=nrow(rand_cell[which(rand_cell$cell<= 53464 & rand_cell$cell>= 15181),]),
                 'SBS_rand'=nrow(rand_cell[which(rand_cell$cell>= 53465),]),
                 'samp'=samp_df[s,'samp_scn'],
                 'strat_var'=samp_df[s,'strat_var'],
                 'type'=samp_df[s,'type'],
                 'region'=samp_df[s,'region'],
                 'regime'=r,
                 'sur'=sur)
    
    data.frame(isur)
    
    #append
    region_cells<-rbind(region_cells,isur)
    }
  }
}

######################
# sampling station per region and sampling design
#####################

#for SLOPE
samp_df2<-subset(region_cells,region %in% c("EBS+SBS",'EBS+NBS+SBS'))
samp_df21<-reshape2::melt(samp_df2,id.vars=c(names(samp_df2)[7:12]))
samp_df21<-samp_df21[grepl("SBS", samp_df21$variable), ]

samp_df21$scn<-paste0(samp_df21$region,'\n',samp_df21$strat_var,'\n',samp_df21$type)

samp_df21$strat_var<-factor(samp_df21$strat_var,levels = c('Depth','varTemp',"Depth_dummy",'varTemp_dummy'))
samp_df21$type<-factor(samp_df21$type,levels = c('static','dynamic'))

# Convert 'scn' to a factor based on 'region'
samp_df21$scn <- factor(samp_df21$scn, levels = unique(samp_df21$scn[order(samp_df21$strat_var,samp_df21$type)]))
samp_df21$scn1 <- paste0(samp_df21$region,'\n',samp_df21$strat_var)
unique(samp_df21$scn1)
samp_df21[which(is.na(samp_df21$strat_var)),]

samp_df21$scn1 <- factor(samp_df21$scn1, levels = unique(samp_df21$scn1[order(samp_df21$strat_var,samp_df21$type)]))

#plot 1slope
psbs1<-
ggplot(data=samp_df21[!grepl('dummy',samp_df21$scn1),])+
  #geom_point(aes(x=scn,y=value,color=regime,shape=variable),size=3,alpha=0.7,position=position_dodge(width=0.5))+
  geom_boxplot(aes(x=scn1,y=value,color=regime,linetype=variable),alpha=0.7,position=position_dodge(width=0.9))+
  scale_y_continuous('sampling effort in the SBS\n(n sampling stations)')+
  theme_bw()+
  theme(axis.title.x = element_blank())+
  scale_color_manual(values = c('all'='black',
                                'warm'='red',
                                'cold'='blue'),
                     labels=c('static','warm','cold'),name='regime')+
  scale_linetype_manual(values = c('SBS_sb'='solid',
                                   'SBS_rand'='dashed'),
                        labels=c('spatially-balanced','random'),name='allocation')

#plot 2slope
psbs2<-
ggplot(data=samp_df21[!grepl('dummy',samp_df21$scn1),])+
  #geom_point(aes(x=scn,y=value,color=regime,shape=variable),size=3,alpha=0.7,position=position_dodge(width=0.5))+
  geom_boxplot(aes(x=scn1,y=value/sbs_km2,color=regime,linetype=variable),alpha=0.7,position=position_dodge(width=0.9))+
  scale_y_continuous('sampling effort per area in the SBS\n(n sampling stations / km2)')+ #limits = c(0,0.001)
  theme_bw()+
  theme(axis.title.x = element_blank())+
  scale_color_manual(values = c('all'='black',
                                'warm'='red',
                                'cold'='blue'),
                     labels=c('static','warm','cold'),name='regime')+
  scale_linetype_manual(values = c('SBS_sb'='solid',
                                   'SBS_rand'='dashed'),
                        labels=c('spatially-balanced','random'),name='allocation')


#for NBS
samp_df2<-subset(region_cells,region %in% c("EBS+NBS",'EBS+NBS+SBS'))
samp_df21<-reshape2::melt(samp_df2,id.vars=c(names(samp_df2)[7:12]))
samp_df21<-samp_df21[grepl("NBS", samp_df21$variable), ]

samp_df21$scn<-paste0(samp_df21$region,'\n',samp_df21$strat_var,'\n',samp_df21$type)

samp_df21$strat_var<-factor(samp_df21$strat_var,levels = c('Depth','varTemp',"Depth_dummy",'varTemp_dummy'))
samp_df21$type<-factor(samp_df21$type,levels = c('static','dynamic'))

# Convert 'scn' to a factor based on 'region'
samp_df21$scn <- factor(samp_df21$scn, levels = unique(samp_df21$scn[order(samp_df21$strat_var,samp_df21$type)]))
samp_df21$scn1 <- paste0(samp_df21$region,'\n',samp_df21$strat_var)
unique(samp_df21$scn1)
samp_df21[which(is.na(samp_df21$strat_var)),]

samp_df21$scn1 <- factor(samp_df21$scn1, levels = unique(samp_df21$scn1[order(samp_df21$strat_var,samp_df21$type)]))

#plot 1nbs
pnbs1<-
ggplot(data=samp_df21[!grepl('dummy',samp_df21$scn1),])+
  #geom_point(aes(x=scn,y=value,color=regime,shape=variable),size=3,alpha=0.7,position=position_dodge(width=0.5))+
  geom_boxplot(aes(x=scn1,y=value,color=regime,linetype=variable),alpha=0.7,position=position_dodge(width=0.9))+
  scale_y_continuous('sampling effort in the NBS\n(n sampling stations)')+
  theme_bw()+
  #scale_y_continuous(limits = c(0,180))+

  theme(axis.title.x = element_blank())+
  scale_color_manual(values = c('all'='black',
                                'warm'='red',
                                'cold'='blue'),
                     labels=c('static','warm','cold'),name='regime')+
  scale_linetype_manual(values = c('NBS_sb'='solid',
                                   'NBS_rand'='dashed'),
                        labels=c('spatially-balanced','random'),name='allocation')

#plot 2nbs
pnbs2<-
ggplot(data=samp_df21[!grepl('dummy',samp_df21$scn1),])+
  #geom_point(aes(x=scn,y=value,color=regime,shape=variable),size=3,alpha=0.7,position=position_dodge(width=0.5))+
  geom_boxplot(aes(x=scn1,y=value/nbs_km2,color=regime,linetype=variable),alpha=0.7,position=position_dodge(width=0.9))+
  scale_y_continuous('sampling effort per area in the NBS\n(n sampling stations / km2)',
                     labels = scales::label_number(accuracy = 0.0001))+ #,limits = c(0,0.001)
  theme_bw()+
  theme(axis.title.x = element_blank())+
  scale_color_manual(values = c('all'='black',
                                'warm'='red',
                                'cold'='blue'),
                     labels=c('static','warm','cold'),name='regime')+
  scale_linetype_manual(values = c('NBS_sb'='solid',
                                   'NBS_rand'='dashed'),
                        labels=c('spatially-balanced','random'),name='allocation')


cowplot::plot_grid(psbs1,pnbs1,ncol=1)

# Increase legend size
psbs1 <- psbs1 + 
  theme(
    legend.title = element_text(size = 14),  # Increase legend title size
    legend.text = element_text(size = 12),   # Increase legend text size
    legend.key.size = unit(1.5, "lines")     # Increase legend key size (symbol size)
  )

pnbs1 <- pnbs1 + 
  theme(
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.key.size = unit(1.5, "lines")
  )


library(cowplot)

# Extract the legend from one of the plots
shared_legend <- get_legend(
  psbs1 + theme(legend.position = "right")
)

# Combine plots and the shared legend
final_plot <- plot_grid(
  plot_grid(pnbs1 + theme(legend.position = "none"),
            psbs1 + theme(legend.position = "none"),
            ncol = 1,
            align = 'v'),
  shared_legend,
  ncol = 2,
  rel_widths = c(0.8, 0.2) # Adjust the width ratio for the plots and the legend
)

ragg::agg_png(paste0('./figures slope/sampling_effort.png'), width = 9, height = 7, units = "in", res = 300)
final_plot
dev.off()

# Combine plots and the shared legend
final_plot <- plot_grid(
  plot_grid(pnbs2 + theme(legend.position = "none"),
            psbs2 + theme(legend.position = "none"),
            ncol = 1,
            align = 'v'),
  shared_legend,
  ncol = 2,
  rel_widths = c(0.8, 0.2) # Adjust the width ratio for the plots and the legend
)

ragg::agg_png(paste0('./figures slope/sampling_effort_area.png'), width = 9, height = 7, units = "in", res = 300)
final_plot
dev.off()

################
# HISTORICAL SURVEY
################
 
#create a df of 40years x 100sur/sim (it changes on the projected since it simulations have different indeces)
yrs<-c(2002:2016) 
#yrs<-c(2002,2004,2008,2010,2012,2016)
n_sur<-100
sur_df<-data.frame(num=1:(length(yrs)*n_sur),
           year=rep(yrs,times=n_sur),
           sur=rep(1:n_sur,each=length(yrs)))

#simulated densities
load(file = paste0('./output slope/species/ms_sim_dens_all.RData'))  #sim_dens1
dimnames(sim_dens1)

#ms_sim_survey folder
dir.create('./output slope/ms_sim_survey/')

#create folder
dir.create(paste0('./output slope/ms_sim_survey_hist/'))

#number of simulations
n_sim_hist<-100

#loop over n combinations of simulated
for (sim in 1:n_sim_hist) {
  
  #sim<-1
  
  #cat(paste0('########## ',sim,' ##########\n'))    
  
  
  # Convert 0 to '001'
  sim_fol <- sprintf("%03d", sim)
  
  #create folder
  dir.create(paste0('./output slope/ms_sim_survey_hist/sim',sim_fol))
  
  #array to store
  # index_hist<-array(NA,
  #                   dim=list(length(spp)+length(crabs),3,length(yrs),3,n_sur,nrow(samp_df)),
  #                   dimnames=list(c(spp,crabs),c('STRS_mean','STRS_var','CV_sim'),paste0('y',yrs),c('sys','rand','sb'),1:n_sur,samp_df$samp_scn))
  
  index_hist<-array(NA,
                    dim=list(length(spp),3,length(yrs),2,n_sur,nrow(samp_df),3),
                    dimnames=list(c(spp),
                                  c('STRS_mean','STRS_var','CV_sim'),
                                  paste0('y',yrs),
                                  c('rand','sb'),
                                  1:n_sur,samp_df$samp_scn,
                                  c('all','cold','warm')))
  
  #loop over sampling design
    for (samp in samp_df$samp_scn)  {
      
      #samp<-'scn1'
      #start_time_parallel <- Sys.time()
      
      #number of sampling design
      s<-match(samp,samp_df$samp_scn)
      
      if (samp_df[s,'type']=='static') {
        #load multispecies data
        #load(paste0('./output slope/multisp_optimization_static_data_ebsnbs_slope_st.RData')) #df
        regime<-c('all')
      } else {
        #load multispecies data
        #load(paste0('./output slope/multisp_optimization_static_data_ebsnbs_slope_dyn.RData')) #df
        regime<-c('cold','warm')
      }
      
      if (samp_df[s,'region']=='EBS') {
        grid3<-subset(grid2,region %in% c('EBS'))
      } else if (samp_df[s,'region']=='EBS+NBS') {
        grid3<-subset(grid2,region %in% c('EBS','NBS'))
      } else if (samp_df[s,'region']=='EBS+SBS') {
        grid3<-subset(grid2,region %in% c('EBS','SBS'))
      } else {
        grid3<-grid2
      }
      
      for (r in regime) {
        
        #r<-'all'
        
        #load optimization results
        load(file=paste0("./output slope/ms_optim_allocations_ebsnbs_slope_",samp_df[s,'samp_scn'],'_',r,".RData")) #list = c('result_list','ss_sample_allocations','ms_sample_allocations','samples_strata','cv_temp')
        
        #area
        area_cell<-merge(all$result_list$solution$indices, grid3, by.x='ID',by.y='cell')
        
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
        
      
      #sum(strata_areas$Area_in_survey_km2)
      
      #array to store simulated densities/CPUE
      alloc<-samp_df$n_samples[s]
      
      #load survey allocations by sampling design
        load(file = paste0('./output slope/survey_allocations_',samp_df[s,'samp_scn'],'_',r,'.RData')) 
        #scn_allocations
      #dimnames(scn_allocations)[[3]]<-c('rand','sb')
      
        #array to store results  
        sim_survey <- array(NA,
                            dim = c(alloc, length(spp)+2, n_sur, length(unique(yrs)),length(c('rand','sb'))),
                            dimnames = list(1:alloc, c('cell','strata',spp), 1:n_sur,unique(yrs), c('rand','sb')))
        
        
        
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
          for (apr in c('rand','sb')) {
            
            #apr<-'rand'
    
            #print process        
            cat(paste(" #############  ",samp,'- simdata',sim,'- survey',sur, '- year',y ,'- allocation',apr," #############\n"))
            
            #get densities based on station allocations
            sim_survey<-data.frame(cbind(strata=scn_allocations[scn_allocations[,'sur',apr]==n,c('strata'),apr],
                                         cell=scn_allocations[scn_allocations[,'sur',apr]==n,c('cell'),apr],
                                         dens=sim_dens2[scn_allocations[scn_allocations[,'sur',apr]==n,c('cell'),apr],]),
                                   check.names = FALSE)
            
            sim_survey1<-reshape2::melt(sim_survey,id.vars=c('strata','cell'))
            
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
            index_hist[,'STRS_mean',paste0('y',y),apr,sur,samp,r]<-STRS_mean
            index_hist[,'STRS_var',paste0('y',y),apr,sur,samp,r]<-STRS_var
            index_hist[,'CV_sim',paste0('y',y),apr,sur,samp,r]<-CV
        
          }
        }
      }
    }
  
  save(index_hist, file = paste0('./output slope/ms_sim_survey_hist/sim',sim_fol,'/index_hist.RData'))  

}
