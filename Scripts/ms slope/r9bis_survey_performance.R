####################################################################
####################################################################
##    
##    compare and plot design estimates vs true estimates
##    
##    danielvilasgonzalez@gmail.com/dvilasg@uw.edu
##
####################################################################
####################################################################

######################
# SETTINGS
######################

#clear all objects
rm(list = ls(all.names = TRUE)) 
#free up memrory and report the memory usage
gc() 

#libraries from cran to call or install/load
pack_cran<-c('raster','units','ggplot2','data.table','sf','reshape2','data.table')

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
       'Sebastes melanostictus',
       'Atheresthes evermanni',
       'Sebastes borealis',
       'Sebastolobus alascanus',
       'Glyptocephalus zachirus',
       'Bathyraja aleutica')

#selected species
sel_spp<-c('Gadus chalcogrammus',
           'Gadus macrocephalus',
           'Chionoecetes opilio',
           'Reinhardtius hippoglossoides')

#common species name
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
        'Blackspotted rockfish',
        'Kamchatka flounder',
        'Shortraker rockfish',
        'Shortspine thornyhead',
        'Rex sole',
        'Aleutian skate')

#selected species
sel_spp_com<-c('Alaska pollock',
               'Pacific cod',
               'Snow crab',
               'Greenland turbot')

df_spp<-data.frame('spp'=spp,
                   'common'=spp1) 

df_spp1<-df_spp
df_spp1<-df_spp1[order(df_spp1$common),]
df_spp1$label<-letters[1:nrow(df_spp1)]

#sp convergence for each models


#read coinvergence and st slope
df_conv<-read.csv('./tables/slope_conv.csv')
slp_conv<-df_conv[which(df_conv$slope_st=='convergence'),'spp']
ebsnbs_conv<-df_conv[which(df_conv$EBS_NBS=='convergence'),'spp']


#create folder simulation data
dir.create(paste0('./output slope//species/'))



#years
yrs<-c(2002:2016)
n_yrs<-length(yrs)

###################################
# GRIDS
###################################

#load grid
load('./data processed/grid_EBS_NBS.RData')
yrs<-c(2002:2016)
grid_ebs<-grid.ebs_year[which(grid.ebs_year$Year %in% yrs),]
dim(grid_ebs)

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

#FIND SLOPE CELLS DEEPER than 400m
load(file = './data processed/grid_EBS_NBS.RData') #grid.ebs_year$region
grid_slp<-subset(grid.ebs_year,region=='EBSslope' & Year=='1982')
dim(grid_slp)
dim(grid_slp[which(grid_slp$DepthGEBCO<=400),])
ok_slp_cells<-as.numeric(row.names(grid_slp)[which(grid_slp$DepthGEBCO<=400)])
#rem_slp_cells<-as.numeric(row.names(grid_slp)[which(grid_slp$DepthGEBCO>400)])

###################################
# Sampling designs
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

######################
# HISTORICAL INDEX
######################

#get true abundance index estimates
gapindex<-readRDS('./data raw/afsc_ebs_nbs_gapindex_2024_03_29.rds')
names(gapindex$haul)
names(gapindex)
head(gapindex$cruise)
 
#calculate the effort in each haul
gapindex$haul$EFFORT<-round(gapindex$haul$DISTANCE_FISHED*gapindex$haul$NET_WIDTH/10,digits=1) #ha 

#select columns
hauls_ind<-gapindex$haul[c('HAULJOIN','REGION','STRATUM','EFFORT',
                           'START_LATITUDE','END_LATITUDE','START_LONGITUDE','END_LONGITUDE',
                           'STATIONID', 'BOTTOM_DEPTH','GEAR_TEMPERATURE')]
dim(hauls_ind)
length(unique(hauls_ind$HAULJOIN))

#get species info
spp_ind<-gapindex$species[c('SPECIES_CODE','SPECIES_NAME','COMMON_NAME','YEAR_ADDED')]

#get catches info
catch_ind<-gapindex$catch[c('HAULJOIN','SPECIES_CODE' , 'WEIGHT' ,'NUMBER_FISH')]

#select species
spp_ind1<-subset(spp_ind,SPECIES_NAME %in% spp)

#spp code
sp_code<-spp_ind1['SPECIES_CODE']

#subset the catch of selected species
catch_ind1<-subset(catch_ind,SPECIES_CODE %in% sp_code$SPECIES_CODE)

#merge species catch with species name
catch_ind2<-
  merge(catch_ind1,spp_ind1,by='SPECIES_CODE',all.x=TRUE)

#create a df to have data for each haul and species
length(unique(hauls_ind$HAULJOIN))*length(spp)
hauls_ind1<-do.call("rbind", replicate(length(spp), hauls_ind, simplify = FALSE))
dim(hauls_ind1)

#replicate spp for each station
spp_v<-rep(spp,each=nrow(hauls_ind))

#dataframe
all<-data.frame(hauls_ind1,'SPECIES_NAME'=spp_v)
dim(all)
names(catch_ind2);names(all)
head(catch_ind2)
head(all)
all1<-
  merge(catch_ind2,all,by=c("HAULJOIN","SPECIES_NAME"),all.y=TRUE)
dim(all1)

#arrange species codes and common name NAs
all2<-merge(all1,spp_ind1,by=c('SPECIES_NAME'))
dim(all2)
head(all2)
all2<-all2[,c(1:2,4:5,8:ncol(all2))]
names(all2)
names(all1)[15:17]<-c('SPECIES_CODE','COMMON_NAME','YEAR_ADDED')

#cpue_kgha,cpue_kgkm2,cpue_noha,cpue_nokm2,count,weight_kg columns need to replace NA by 0s
all2[c('WEIGHT','NUMBER_FISH')][is.na(all2[c('WEIGHT','NUMBER_FISH')])] <- 0
head(all2)
summary(all1)
dim(all2)

######################
# HISTORICAL INDEX
######################

#dataframe to store estimated indices
ind2<-data.frame(matrix(NA,nrow = 0,ncol = 7))
names(ind2)<-c('spp','year','approach','sur','scn','index','sim')

#list of files (100 files, one for each simulated data)
files<-list.files('./output slope//ms_sim_survey_hist/',pattern = 'index_hist',recursive = TRUE,full.names = TRUE)

sims<-list.files('./output slope/ms_sim_survey_hist/')
sims<-as.numeric(gsub('sim','',sims))

#loop over simulated data - files  
for (sim in sims) {
  
  #sim<-sims[1]
  
  #print
  cat(paste0('##### ',' sim', sim))
  
  #load file  
  load(files[sim])
  
  ind<-index_hist[,'STRS_mean',,,,,]
  
  #array to dataframe
  ind<-as.data.frame.table(ind)
  
  names(ind)<-c('spp','year','approach','sur','scn','regime','index')
  ind$year<-gsub('y','',ind$year)
  ind$sim<-sim
  
  #append
  ind2<-rbind(ind2,ind)
  
  #remove object
  rm(index_hist)
}

#save simulated index
save(ind2,file = './output slope//estimated_index_hist.RData') #ind2
#load('./output slope/estimated_index_hist.RData')

#aggregate df to get mean, q95 and q5 for each group (sp, year, sampling scenario and approach)
df<-aggregate(index ~ spp + year + scn + approach + regime,ind2,FUN = function(x) c(mean = mean(x), q95 = quantile(x,probs=0.95) , q5 = quantile(x,probs=0.05)) )
colnames(df$index)<-c('mean','q95','q5')

#merge df and sp df
df<-merge(df,df_spp1,by='spp')
df$year<-as.integer(df$year)

##############################
#
# OBTAIN TRUE INDEX FROM OMs FOR NBS+EBS and SLOPE 
#
################################

#23 spp and 4 regions (EBS, NBS+EBS, EBS+SBS and NBS+EBS+SBS)
#loop over spp

### SLOPE

dens_index_hist_OM<-list()

#loop over spp
for (sp in slp_conv) {
  
  #sp<-spp[5] #20
  
  cat(paste(sp,'\n'))
  
  #model
  #mod<-df.conv[which(df.conv$spp==sp),'slope_mod']
  
  #if (mod=='ST') {
    
    mod1<-'fit_st.RData'
    
  #} else if (mod=='non_ST') {
    
  #  mod1<-'fit.RData'
    
  #} else {
    
  #  next
  #}
  
  #get list of fit data
  ff<-list.files(paste0('./slope EBS VAST/',sp),mod1,recursive = TRUE)
  
  #load fit file
  load(paste0('./slope EBS VAST/',sp,'/',ff)) #fit
  #getLoadedDLLs() #if check loaded DLLs
  #check_fit(fit$parameter_estimates)
  
  ##reload model
  fit<-
    reload_model(x = fit)
  
  #store index and dens
  index<-fit$Report$Index_ctl
  dens<-fit$Report$D_gct[,1,]
  
  dens_index_hist_OM[[sp]]<-list('index'=index,'dens'=dens)
  
}

save(dens_index_hist_OM, file = paste0("./output slope//species/dens_index_hist_OM_slope.RData")) 
load(file = paste0("./output slope//species/dens_index_hist_OM_slope.RData")) #dens_index_hist_OM

### NBS+EBS

dens_index_hist_OM<-list()

#loop over spp
for (sp in ebsnbs_conv) {
  
  #sp<-ebsnbs_conv[1] #20
  
  cat(paste(sp,'\n'))
  
  
  #get list of fit data
  ff<-list.files(paste0('./shelf EBS NBS VAST/',sp),'fit',recursive = TRUE)
  
  #load fit file
  load(paste0('./shelf EBS NBS VAST/',sp,'/',ff)) #fit
  #getLoadedDLLs() #if check loaded DLLs
  #check_fit(fit$parameter_estimates)
  
  ##reload model
  fit<-
    reload_model(x = fit)
  
  #store index and dens
  index<-fit$Report$Index_ctl
  dens<-fit$Report$D_gct
  dens_index_hist_OM[[sp]]<-list('index'=index,'dens'=dens)
  
}

save(dens_index_hist_OM, file = paste0("./output slope//species/dens_index_hist_OM_ebsnbs.RData")) 
load(paste0("./output slope//species/dens_index_hist_OM_ebsnbs.RData")) #dens_index_hist_OM

#######join true ind

#load true ebsnbs index
load(file = paste0("./output slope//species/dens_index_hist_OM_ebsnbs.RData")) 
ind_ebsnbs<-dens_index_hist_OM

#load true slope index
load(file = paste0("./output slope//species/dens_index_hist_OM_slope.RData")) 
ind_slope<-dens_index_hist_OM

#df to store results
true_ind<-data.frame(matrix(NA,nrow = length(yrs),ncol = length(spp)))
rownames(true_ind)<-yrs
colnames(true_ind)<-c(spp)

#loop over species stock to extract the true index
for (sp in spp) {
  
  # sp<-spp[18]
  
  if (sp %in% names(ind_ebsnbs)) {
    
    #get index ebs nbs
    ind_ebsnbs1<-drop_units(ind_ebsnbs[[sp]]$index[,as.character(yrs),1])

  } else {
    ind_ebsnbs1<-rep(0,length(yrs))
  }
  
  if (sp %in% names(ind_slope)) {

    #get biomass
    bio<-drop_units(data.frame(sweep(ind_slope[[sp]]$dens[,], 1, bering_sea_slope_grid$Area_in_survey_km2, "*"),check.names = FALSE))
    bio$cell<-c(53465:56505)
    
    #index for the slope <400m
    ind_slope2<-colSums(bio[which(bio$cell %in% ok_slp_cells),as.character(2002:2016)])
    
  } else {
    ind_slope2<-rep(0,length(yrs))
  }

  #sum bio from ebsnbs and slope
  ind<-ind_ebsnbs1+ind_slope2
  
  #append total index
  true_ind[,sp]<-ind
  
}

true_ind[c(9,15),'Sebastes melanostictus']<-NA

#arrange true index data
true_ind$year<-as.character(yrs)
true_ind1<-reshape2::melt(true_ind,id.vars='year')
names(true_ind1)<-c('year','spp','value')
unique(true_ind1$spp)
true_ind1<-true_ind1[which(true_ind1$spp %in% df_spp1$spp),]
true_ind1<-merge(true_ind1,df_spp1,by='spp')
true_ind1$year<-as.integer(true_ind1$year)
true_ind1$dummy<-'true index'

#save true ind
save(true_ind,file = paste0("./output slope//true_ind_hist.RData"))  
load(file = paste0("./output slope//true_ind_hist.RData"))  #true_ind

#fxn to turn axis into scientific
scientific_10 <- function(x) {
  parse(text=gsub("e", " %*% 10^", scales::scientific_format()(x)))
}

#sort approach (station allocation)
df$approach <- factor(df$approach, levels = c("sb", "rand"))

#remove existing vis sampling design and EBSNBS crabs
# df<-subset(df,scn!='scnbase_bis')
# unique(df$common)
# df<-subset(df,common %in% unique(df$common)[!grepl("_EBSNBS", as.character(unique(df$common)))])
# true_ind1<-subset(true_ind1,common %in% unique(df$common)[!grepl("_EBSNBS", as.character(unique(df$common)))])

#to adjust y axis limits
df$value<-df$index[,'q95']/1000
y_scale<-aggregate(value ~ common, df,max)
y_scale$scale<-y_scale$value+y_scale$value*0.2
y_scale$text<-y_scale$value+y_scale$value*0.15
y_scale$apr<-'sys'
y_scale$year<-2010
y_scale$scn<-'scn1'

#Our transformation function
scaleFUN <- function(x) sprintf("%.2f", x)

#check
df[which(df$spp=='Sebastes melanostictus'),]
true_ind1[which(true_ind1$spp=='Sebastes melanostictus'),]

#plot abundance index for each sampling design
p<-
  ggplot() +
  geom_line(data=df, aes(x=year, y=index[,'mean']/1000000000, color=scn, group=interaction(scn, approach, common,regime), linetype=approach), linewidth=1.5, alpha=0.7) +
    geom_point(data=true_ind1, aes(x=year, y=value/1000000000, group=common, shape=dummy), fill='black', color='black', size=1) +
  labs(y=expression('MT ('* 10^6 * ' tons)'), x='') +
  # scale_fill_manual(values=c('scn1'='#9B59B6','scn2'='#3498DB','scn3'='#1ABC9C','scnbase'='#696778'),
  #                   labels = c('existing' ,'opt depth','opt varSBT','opt depth + varSBT'), name='stratification') +
  # scale_color_manual(values=c('scn1'='#9B59B6','scn2'='#3498DB','scn3'='#1ABC9C','scnbase'='#696778'),
  #                    labels = c('existing','opt depth','opt varSBT','opt depth + varSBT'), name='stratification') +
  # scale_alpha_manual(values = c('scn1'=1,'scn2'=1,'scn3'=1,'scnbase'=0.8),
  #                    labels = c('existing' ,'depth','var temp','depth + var temp'), name='stratification') +
  theme_bw() +
  scale_linetype_manual(values = c('sb'='dashed', 'rand'='solid'),
                        labels=c('balanced random','random'),
                        name='station allocation') +
  scale_shape_manual(values=c('true index'=16), name='') +
  #scale_x_continuous(expand=c(0,0),
  #                   breaks = c(2002,2004,2008,2010,2012,2014,2016),
  #                   minor_breaks = setdiff(2002:2016,c(2002,2004,2008,2010,2012,2014,2016))) +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), labels=scaleFUN) +
  theme(panel.grid.minor = element_line(linetype=2, color='grey90'),
        legend.key.width = unit(2.5, "lines"),
        legend.key.size = unit(20, 'points'),
        legend.direction = 'vertical',
        legend.text = element_text(size=12),
        legend.title = element_text(size=12),
        legend.spacing = unit(1, "cm"),
        legend.box.spacing = unit(0.01, "cm"),
        strip.background = element_blank(),
        legend.background = element_blank(),
        legend.box = 'horizontal',
        legend.position = 'bottom',
        strip.text = element_blank(),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 10)) +
  expand_limits(y = 0) +
  geom_text(data=y_scale, aes(label = common, y = text/1000000), x = Inf, vjust = 'inward', hjust = 1.1, size=4, lineheight = 0.8) +
  geom_text(data=df, aes(label = label), x = 1984, y = Inf, vjust = 1.5, size=5) +
  guides(
    fill = guide_legend(nrow=1, order = 1, override.aes = list(size=4)),
    color = guide_legend(nrow=1, order = 1, override.aes = list(size=4, linewidth=1.2)), # Adjust linewidth here
    linetype = guide_legend(nrow=1, order = 2, override.aes = list(linewidth=1.2)), # Adjust linewidth here
    shape = guide_legend(nrow=1, order = 3, override.aes = list(size=4))
  ) +
  geom_blank(data=y_scale, aes(x=year, y=scale/1000000, fill=scn, group=interaction(scn, apr))) +
  facet_wrap(~common, scales='free_y', dir='h', nrow = 5)


#save index plot
ragg::agg_png(paste0('./figures slope/ms_hist_indices.tiff'), width = 14, height = 8, units = "in", res = 300)
p
dev.off()

names(true_ind1)[3]<-'true_ind'
df2<-(merge(df,true_ind1,by=c('spp','common','year','label')))


df2$diff<-(df2$index[,'mean']-(df2$true_ind))/(df2$true_ind)

#to adjust y axis limits
df2$value<-df2$diff
y_scale<-aggregate(value ~ common, df2,max)
y_scale$scale<-y_scale$value+y_scale$value*0.2
y_scale$text<-y_scale$value+y_scale$value*0.17
y_scale$apr<-'rand'
y_scale$year<-2010
y_scale$scn<-'scn1'


#p<-
  ggplot()+
  #geom_ribbon(data=df,aes(x=year,ymax=index[,'q95']/1000000000,ymin=index[,'q5']/1000000000,group=interaction(scn,approach,common),fill=scn),alpha=0.05)+
  #geom_point(data=ex2,aes(x=year,y=biomass_MT/1000000,group=common),fill='black',color='black',size=1.5,shape=4)+
  #geom_point(data=true_ind1,aes(x=year,y=value/1000000000,group=common,shape=dummy),fill='black',color='black',size=1.5)+
  geom_line(data=df2,aes(x=year,y=diff,color=scn,group=interaction(scn,approach,common,regime),linetype=approach),linewidth=1.5,alpha=0.7)+
  labs(y=expression("("*hat('I')*" - I ) / I"),x='')+
  # scale_fill_manual(values=c('scn1'='#9B59B6','scn2'='#3498DB','scn3'='#1ABC9C','scnbase'='#696778'),
  #                   labels = c('existing' ,'opt depth','opt varSBT','opt depth + varSBT'),name='stratification')+
  # scale_color_manual(values=c('scn1'='#9B59B6','scn2'='#3498DB','scn3'='#1ABC9C','scnbase'='#696778'),
  #                    labels = c('existing','opt depth','opt varSBT','opt depth + varSBT'),name='stratification')+
  # scale_alpha_manual(values = c('scn1'=1,'scn2'=1,'scn3'=1,'scnbase'=0.8),
  #                    labels = c('existing' ,'depth','var temp','depth + var temp'),name='stratification')+
  theme_bw()+
  scale_linetype_manual(values = c(
                                   'sb'='dashed',
                                   'rand'='solid'),
                        label=c('balanced random','random'),
                        name='station allocation')+
  #scale_shape_manual(values=c('true index'=16),name='')+
  #coord_trans(y = "exp")+
  scale_x_continuous(expand=c(0,0),
                     breaks = c(1985,1990,1995,2000,2005,2010,2015,2020),
                     minor_breaks = setdiff(1982:2022,c(1982,1985,1990,1995,2000,2005,2010,2015,2020,2022))) +
  #scale_y_continuous(expand = c(0,0), limits = c(0,NA), labels=scaleFUN) +
  theme(panel.grid.minor = element_line(linetype=2, color='grey90'),
        #legend.key.width = unit(2.5, "lines"),
        #legend.key.size = unit(20, 'points'),
        #legend.direction = 'vertical',
        #legend.text = element_text(size=12),
        #legend.title = element_text(size=12),
        #legend.spacing = unit(1, "cm"),
        #legend.box.spacing = unit(0.01, "cm"),
        strip.background = element_blank(),
        legend.background = element_blank(),
        #legend.box = 'horizontal',
        #legend.position = 'bottom',
        strip.text = element_blank(),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 10)) +
  # scale_x_continuous(expand=c(0,0),
  #                    breaks = c(1985,1990,1995,2000,2005,2010,2015,2020),
  #                    minor_breaks = setdiff(1982:2022,c(1982,1985,1990,1995,2000,2005,2010,2015,2020,2022)))+
  #scale_y_continuous(expand = c(0,0), limits = c(0,NA)) +
  # #                    limits =  c(0, max(df$index[,'mean']/1000) + mean(df$index[,'mean'])/10000))+
  # theme(panel.grid.minor = element_line(linetype=2,color='grey90',),legend.key.width = unit(1.5, "lines"),#strip.background = element_rect(fill='white'),
  #       legend.key.size = unit(15, 'points'),legend.direction = 'vertical',legend.text = element_text(size=11), #legend.position=c(.85,.19)
  #       legend.title = element_text(size=12),legend.spacing = unit(2, "cm"),legend.box.spacing =  unit(0.01, "cm"), #,strip.text = element_text(size=12)
  #       strip.background = element_blank(),legend.background = element_blank(),legend.box = 'horizontal',legend.position = 'bottom',#legend.justification = 'right',legend.position='bottom',#legend.position=c(.84,.05),
  #       strip.text = element_blank(),axis.title.x = element_blank(),axis.text = element_text(size = 10))+ #axis.text.x = element_text(angle=90,vjust=0.5),
  expand_limits(y = 0)+
  geom_text(data=y_scale,aes(label = common, y = text),x = Inf, vjust = 'inward', hjust = 1.1,size=4, lineheight = 0.8) + #,fontface='italic'
  geom_text(data=df2,aes(label = label),x = 1984, y = Inf, vjust = 1.5,size=5) + #,fontface='italic'
  # guides(
  #   fill = guide_legend(nrow=1, order = 1, override.aes = list(size=4)),
  #   color = guide_legend(nrow=1, order = 1, override.aes = list(size=4, linewidth=1.2)), # Adjust linewidth here
  #   linetype = guide_legend(nrow=1, order = 2, override.aes = list(linewidth=1.2)), # Adjust linewidth here
  #   shape = guide_legend(nrow=1, order = 3, override.aes = list(size=4))
  # ) +  #facet_wrap(~com_sci,scales='free',dir='v',nrow = 3)
  #pacific cod 
  geom_blank(data=y_scale,aes(x=year,y=scale,fill=scn,group =interaction(scn,apr)))+
  facet_wrap(~common,scales='free_y',dir='h',nrow = 5)

#save index plot
#ragg::agg_png(paste0('./figures/ms_hist_diff_v5.png'), width = 14, height = 8, units = "in", res = 300)
#p
#dev.off()

######################
# HISTORICAL CV
######################

#get CV for each replicate, approach and sampling scenario
#CV are calculated on the script #13 as, 
### CV <- sqrt(STRS_var) / STRS_mean 
### STRS_var<-sum(strs_var, by=year) ### strs_var<-var*area²/samples ### var<-var(CPUE)
### STRS_mean<-sum(index_strata, by=year) ### index_strata<-mean_strata*area ### mean_strata<-mean(CPUE)

#dataframe to store estimated indices
cv2<-data.frame(matrix(NA,nrow = 0,ncol = 8))
names(cv2)<-c('spp','year','approach','sur','scn','regime','cv','sim')

#list of files (100 files, one for each simulated data)
files<-list.files('./output slope//ms_sim_survey_hist/',pattern = 'index_hist',recursive = TRUE,full.names = TRUE)


#loop over simulated data - files  
for (sim in 1:100) {
  
  #sim<-1
  
  #print
  cat(paste0('##### ',' sim', sim))
  
  #load file  
  load(files[sim])
  
  #get estimated CV for groundfish
  cv<-index_hist[,'CV_sim',,,,,]
  
  #array to dataframe
  cv<-as.data.frame.table(cv)

  #combine and add year and simulated dataset
  cv1<-cv 
  names(cv1)<-c('spp','year','approach','sur','scn','regime','cv')
  cv1$year<-gsub('y','',cv1$year)
  cv1$sim<-sim
  
  #append
  cv2<-rbind(cv2,cv1)
  
  #remove object
  rm(index_hist)
}

setDT(cv2)
#cv2[spp==sp & scn==iscn & approach==apr & sim==1 & sur==su]

#save cv sim data  
save(cv2,file = './output slope//estimated_cvsim_hist.RData')
load(file = './output slope//estimated_cvsim_hist.RData')
#mean(cv2[which(cv2$spp == "Boreogadus saida" & cv2$scn == "scnbase"),'cv'])
#mean(cv2[which(cv2$spp == "Boreogadus saida" & cv2$scn != "scnbase"),'cv'])

# means<-aggregate(cv ~ spp + scn + approach + regime,cv2,FUN = function(x) c(mean = mean(x)) )
# #means<-means[which(means$scn!='scnbase_bis'),]
# means[order(means$spp),]
# means$opt<-ifelse(means$scn=='scnbase',FALSE,TRUE)
# means_opt<-aggregate(cv ~ spp + opt,means,FUN = function(x) c(mean = mean(x)) )
# means_opt<-cbind(means_opt[1:20,],'cv_opt'=means_opt[21:40,'cv'])
# means_opt$opt_better<-ifelse(means_opt$cv_opt<means_opt$cv,TRUE,FALSE)
# 
# means_opt1<-merge(means_opt,df_spp1,by='spp')
# #means_opt1<-subset(means_opt1,common %in% unique(means_opt1$common)[!grepl("_EBSNBS", as.character(unique(means_opt1$common)))])
# means_opt1<-means_opt1[,-2]
# means_opt2<-
#   data.frame('common'=gsub('\n',' ',means_opt1$common),
#              'cv_existing'=round(means_opt1$cv,digits = 3),
#              'cv_optimized'=round(means_opt1$cv_opt,digits = 3),
#              'cv_optimized<cv_existing'=means_opt1$opt_better)
# 
# summary(means_opt2)

#if DT
#mean(c(cv2[cv2$spp == "Boreogadus saida" & cv2$scn == "scnbase",])$mean)

#year to number
cv2$year<-as.numeric(as.character(cv2$year))

#aggregate df to get mean, q95 and q5 for each group (year, sampling scenario and approach)
df<-aggregate(cv ~ spp + year + scn + approach + regime,cv2,FUN = function(x) c(mean = mean(x), q95 = quantile(x,probs=0.95) , q5 = quantile(x,probs=0.05)) )
colnames(df$cv)<-c('mean','q95','q5')

#sort factors for plotting purposes
#df$scn<-factor(df$scn,
#               levels = c('scnbase','scnbase_bis','scn3','scn2','scn1'))
#df<-df[which(df$spp %in% df_spp1$spp),]
df<-merge(df,df_spp1,by='spp')
#df$approach<-factor(df$approach,levels=c('sys','sb','rand'))

#define year as numeric
df$year<-as.numeric(df$year)
#df1<-df

#merge results to sampling design table
df1<-merge(df,samp_df,by.x='scn',by.y='samp_scn',all.x=TRUE)

#label scn
df1$scn_label<-paste0(df1$region,'\n',df1$strat_var)

#sort and corrections for plotting purposes
#df1$scn<-factor(df1$scn,levels=c('scnbase','scnbase_bis',paste0('scn',3:1)))

#rename column and remove EBSNBS crabs and existing bis sampling design
df1$value<-df1$cv[,'mean']
# df1<-subset(df1,scn!='scnbase_bis')
# unique(df$common)
# df1<-subset(df1,common %in% unique(df1$common)[!grepl("_EBSNBS", as.character(unique(df1$common)))])
#df1<-subset(df1, grepl("crab", common))

#for geom_blank(0 and adjust scale)
y_scale<-aggregate(value ~ common, subset(df1, spp %in% sel_spp),max)
y_scale$scale<-y_scale$value+y_scale$value*0.2
y_scale$text<-y_scale$value+y_scale$value*0.1
y_scale$apr<-'sb'
y_scale$scn<-'scn1'
y_scale$year<-2022
y_scale$scn_label<-'EBS\nvarTemp'

# #sort factors just in case
df1$scn_label<-factor(df1$scn_label,levels=c("EBS\nDepth","EBS+NBS\nDepth","EBS+SBS\nDepth","EBS+NBS+SBS\nDepth",
                                             "EBS\nvarTemp","EBS+NBS\nvarTemp","EBS+SBS\nvarTemp","EBS+NBS+SBS\nvarTemp",
                                             "EBS+SBS\nDepth_dummy","EBS+NBS+SBS\nDepth_dummy",
                                             "EBS+SBS\nvarTemp_dummy","EBS+NBS+SBS\nvarTemp_dummy"))
                                             
    
#plot estimated CV for each sampling design
p<-
  ggplot()+
    geom_boxplot(data = subset(df1, spp %in% sel_spp),
                 aes(x = scn_label, y = value, fill = scn_label, color = regime,
                     group = interaction(scn_label, approach, spp, regime), linetype = approach),
                 lwd = 0.6, alpha = 0.8, outlier.alpha = 0.3, outlier.size = 1.2)+
    labs(y=expression(widehat(CV)),x='')+
    theme_bw()+ 
    facet_wrap(~common,scales='free_y')+#scales = list(y = list(breaks = pretty(range(df1$value), n = 5))))+ #dir='h',ncol = 2
    scale_linetype_manual(values = c('sb'='solid',
                                     'rand'='dashed'),
                          label=c('balanced random','random'),
                          name='station allocation')+
    scale_color_manual(values = c('all'='black','cold' = 'darkblue', 'warm' = 'darkred')) +
    scale_y_continuous(expand = c(0,0),limits = c(0,NA))+ #expand = c(NA,0.1),limits = c(0,NA)
    theme(panel.grid.minor = element_line(linetype=2, color='grey90'),
          legend.key.width = unit(2.5, "lines"),
          legend.key.size = unit(20, 'points'),
          #legend.direction = 'vertical',
          legend.text = element_text(size=12),
          legend.title = element_text(size=12),
          legend.spacing = unit(1, "cm"),
          legend.box.spacing = unit(0.01, "cm"),
          strip.background = element_blank(),
          legend.background = element_blank(),
          #legend.box = 'horizontal',
          panel.spacing = unit(10, "lines"), # Increase vertical space between facets
          legend.position = 'none',
          strip.text = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
          axis.text = element_text(size = 10)) +
    expand_limits(y = 0)+
    geom_vline(
    xintercept = c(4.5, 8.5, 10.5), # Adjust based on the x-axis positions of groups
    linetype = "dashed", color = "grey70", linewidth = 0.5)+
    geom_text(data=subset(y_scale,common %in% sel_spp_com),aes(label = common, y = text),x = Inf, vjust = 1.7, hjust = 1.1,size=4, lineheight = 0.8) + #,fontface='italic'
    geom_blank(data=subset(y_scale,spp %in% sel_spp_com),aes(x=scn_label,y=scale,fill=scn_label,group =interaction(scn_label,apr)))
    
#save plot
ragg::agg_png(paste0('./figures slope/ms_hist_indices_cv_box.png'), width = 13, height = 8, units = "in", res = 300)
#ragg::agg_png(paste0('./figures/ms_hist_indices_cv_box_EBSNBS_suppl.png'), width = 13, height = 8, units = "in", res = 300)
p
dev.off()

#plot estimated CV for each sampling design without dummy ones (for the manuscript)
p<-
  ggplot()+
  geom_boxplot(data = subset(df1, spp %in% sel_spp & scn_label %in% unique(df1$scn_label)[grep('dummy', unique(df1$scn_label), invert = TRUE)]),
               aes(x = scn_label, y = value, fill = scn_label, color = regime,
                   group = interaction(scn_label, approach, spp, regime), linetype = approach),
               lwd = 0.6, alpha = 0.8, outlier.alpha = 0.3, outlier.size = 1.2)+
  labs(y=expression(widehat(CV)),x='')+
  theme_bw()+ 
  facet_wrap(~common,scales='free_y')+#scales = list(y = list(breaks = pretty(range(df1$value), n = 5))))+ #dir='h',ncol = 2
  scale_linetype_manual(values = c('sb'='solid',
                                   'rand'='dashed'),
                        label=c('balanced random','random'),
                        name='station allocation')+
  scale_color_manual(values = c('all'='black','cold' = 'darkblue', 'warm' = 'darkred')) +
  scale_y_continuous(expand = c(0,0),limits = c(0,NA))+ #expand = c(NA,0.1),limits = c(0,NA)
  theme(panel.grid.minor = element_line(linetype=2, color='grey90'),
        legend.key.width = unit(2.5, "lines"),
        legend.key.size = unit(20, 'points'),
        #legend.direction = 'vertical',
        legend.text = element_text(size=12),
        legend.title = element_text(size=12),
        legend.spacing = unit(1, "cm"),
        legend.box.spacing = unit(0.01, "cm"),
        strip.background = element_blank(),
        legend.background = element_blank(),
        #legend.box = 'horizontal',
        legend.position = 'none',
        strip.text = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.text = element_text(size = 10)) +
  expand_limits(y = 0)+
  geom_vline(
    xintercept = c(4.5), # Adjust based on the x-axis positions of groups
    linetype = "dashed", color = "grey70", linewidth = 0.5)+
  geom_text(data=subset(y_scale,common %in% sel_spp_com),aes(label = common, y = text),x = Inf, vjust = 1.7, hjust = 1.1,size=4, lineheight = 0.8) + #,fontface='italic'
  geom_blank(data=subset(y_scale,spp %in% sel_spp_com),aes(x=scn_label,y=scale,fill=scn_label,group =interaction(scn_label,apr)))
  
#save plot
ragg::agg_png(paste0('./figures slope/ms_hist_indices_cv_box_nodummy.png'), width = 13, height = 8, units = "in", res = 300)
#ragg::agg_png(paste0('./figures/ms_hist_indices_cv_box_EBSNBS_suppl.png'), width = 13, height = 8, units = "in", res = 300)
p
dev.off()

######################
# RRMSE of CV
######################

#get the estimated CV time series for each replicate, sampling scenario and approach
load(file = './output/estimated_cvsim_hist.RData') #cv2 by sim, sur and scn

#year to character
cv2$year<-as.character(cv2$year)
summary(cv2)

#rename
names(cv2)[6]<-'cvsim'
#cv2[spp==sp & scn==iscn & approach==apr & sim==si & sur==su]

#get estimated index SD for each survey across years, sampling scenario and approach
#index_sd by sim, and scn
load('./output/estimated_index_hist.RData') #ind2
summary(ind2)
index_sd<-aggregate(index ~ spp + year + scn + approach + sim,ind2,FUN = function(x) c(sd = sd(x)))
#summary(index_sd)
#summary(index_sd[which(index_sd$approach=='sys' & index_sd$scn=='scnbase_bis'),])
names(index_sd)[6]<-'index_sd'

#data.table to fasten the merging process
setDT(cv2)
View(cv2[spp==sp & scn==iscn & approach==apr & sim==si & sur==su])
setDT(index_sd)

#estimated df by merge cvsim with estindex_sd
dim(cv2)
est_df<-merge(cv2,index_sd,by=c('spp','year','scn','approach','sim'),all.x=TRUE)
dim(est_df)

#get true ind
load('./output/true_ind_hist.RData') #true_ind

#true index reshape
true_ind2<-reshape2::melt(true_ind,id.vars='year')
names(true_ind2)[c(2,3)]<-c('spp','true_ind')
setDT(true_ind2)

#merge estimated df with true index
all_df<-merge(est_df,true_ind2,by=c('spp','year'),all.x=TRUE)
dim(all_df)

#CV true
all_df$cvtrue<-all_df$index_sd/all_df$true_ind

#check
all_df[order(all_df$cvtrue, decreasing = TRUE),]  
all_df[which(all_df$approach=='sys' & all_df$scn=='scnbase'),]


#calculate RRMSE
all_df$sqdiffcv<-(all_df$cvsim-all_df$cvtrue)^2
all_df1<-all_df[, .(mean_sqdiffcv = mean(sqdiffcv,na.rm=FALSE),mean_cvsim=mean(cvsim,na.rm=FALSE),mean_cvtrue=mean(cvtrue,na.rm=FALSE)), by = .(spp,scn,approach,sim,year)]

###################
# Spearman rank correlation CVsim - CVtrue
###################

for (sp in unique(all_df$spp)[10:20]) {
  
  #df to store results
  corr_df<-data.frame(matrix(NA,nrow = 0,ncol = 7))
  names(corr_df)<-c('spp','scn','approach','sim','sur','pvalue','rho')
  
  for (iscn in unique(all_df$scn)[1:4]) {
    for (apr in unique(all_df$approach)) {
      for (si in unique(all_df$sim)) {
        for (su in unique(all_df$sur)) {
          
          #sp<-unique(all_df$spp)[1];iscn<-unique(all_df$scn)[4];apr<-unique(all_df$approach)[1];si<-unique(all_df$sim)[1];su<-unique(all_df$sur)[1]
          
          cat(paste(sp,' - ',iscn,' - ',apr,' - ',si,' - ',su,'\n'))
          #View(all_df[spp==sp & scn==iscn & approach==apr & sim==si & sur==su])
          
          #subset
          iall_df<-all_df1[spp==sp & scn==iscn & approach==apr & sim==si]
          
          # Spearman's rank correlation analysis
          correlation_result <- cor.test(iall_df$mean_cvsim,iall_df$mean_cvtrue, method = "spearman")
          
          # Print the correlation coefficient
          #cat("Spearman's rank correlation coefficient:", correlation_result$estimate, "\n")
          
          # Print the p-value
          #cat("p-value:", correlation_result$p.value, "\n")
          
          #idf
          idf<-data.frame('spp'=paste0(sp),
                          'scn'=paste0(iscn),
                          'approach'=paste0(apr),
                          'sim'=si,
                          'sur'=su,
                          'pvalue'=round(correlation_result$p.value,digits = 3),
                          'rho'=round(correlation_result$estimate[1],digits = 3))
          
          
          #append results
          corr_df<-rbind(corr_df,idf)
          
          
          # Print a summary of the test
          # cat("\nTest summary:\n")
          # print(correlation_result)        
          
          # A value close to 1 indicates a strong positive correlation, suggesting high consistency in the ranking of estimated CVs relative to true CVs across the years.
          # A value close to -1 indicates a strong negative correlation, suggesting an inverse relationship between the ranking of estimated CVs and true CVs across the years.
          # A value close to 0 suggests no significant correlation, indicating inconsistency in the ranking of estimated CVs relative to true CVs across the years.
          
        }
      }
    }
  }
  
  save(corr_df,file = paste0('./output/full_spearman_',sp,'.RData'))
  
}

#corr_df<-corr_df[which(corr_df$spp!='Boreogadus saida'),]
#save(corr_df,file = './output/full_spearman_1-8.RData')

files_list<-list.files('./output/',pattern = 'full_spearman',full.names = TRUE)

data_list <- lapply(files_list, function(file) {
  load(file)
  return(get("corr_df")) # Change "your_data_object_name" to the actual name of your data object in each .RData file
})

combined_data <- do.call(rbind, data_list)

#combined_data[which(combined_data$scn=='scnbase' & combined_data$approach=='sys'),]


library(dplyr)

# Calculate mean for groups on multiple categories
means <- combined_data %>%
  group_by(spp, scn, approach) %>%
  summarise(mean_value = mean(rho))

# Calculate percentage of values above a threshold
#threshold <- 0  # set your threshold here
percent_above_threshold <- combined_data %>%
  group_by(spp, scn, approach) %>%
  summarise(count_above_threshold = sum(pvalue > 0.05),
            count = n(),
            above_threshold = mean(pvalue > 0.05) * 100)
percent_above_threshold$percent<-percent_above_threshold$count_above_threshold/percent_above_threshold$count*100
corr_df[which(corr_df$spp=='Hippoglossoides robustus' & corr_df$approach=='rand'),]


percent_above_threshold0 <- combined_data %>%
  group_by(approach, scn, spp) %>%
  summarise(#count_above_threshold = sum(pvalue > 0.05,na.rm = TRUE),
    #count = n(),
    mean_rho = mean(rho,na.rm = TRUE),
    percent_significant = mean(pvalue < 0.05,na.rm = TRUE) * 100)

write.csv(percent_above_threshold0, file = "./tables/spearman_all.csv", row.names = FALSE)

spp_cr<-c(
  'Chionoecetes opilio',
  'Paralithodes platypus',
  'Paralithodes camtschaticus',
  'Chionoecetes bairdi')

spp2<-setdiff(df_spp1$spp,spp_cr)

percent_above_threshold00<-percent_above_threshold0[which(percent_above_threshold0$spp %in% spp2),]
percent_above_threshold000<-merge(percent_above_threshold00,df_spp1,by='spp')
percent_above_threshold000$scn<-factor(percent_above_threshold000$scn)
levels(percent_above_threshold000$scn)<-rev(c('existing' ,'opt depth','opt varSBT','opt depth + varSBT'))
percent_above_threshold000$approach<-factor(percent_above_threshold000$approach)
levels(percent_above_threshold000$approach)<-rev(c('systematic','balanced random','random'))
percent_above_threshold000$common<-gsub('\n',' ',percent_above_threshold000$common)

write.csv(percent_above_threshold000, file = "./tables/spearman_all1.csv", row.names = FALSE)

df$scn<-factor(df$scn,
               levels = c('scnbase','scnbase_bis','scn3','scn2','scn1'))

percent_above_threshold1 <- combined_data %>%
  group_by(approach) %>%
  summarise(#count_above_threshold = sum(pvalue > 0.05,na.rm = TRUE),
    #count = n(),
    mean_rho = mean(rho,na.rm = TRUE),
    percent_significant = mean(pvalue < 0.05,na.rm = TRUE) * 100)

write.csv(percent_above_threshold1, file = "./tables/spearman_approach.csv", row.names = FALSE)


percent_above_threshold2 <- combined_data %>%
  group_by(scn) %>%
  summarise(#count_above_threshold = sum(pvalue > 0.05,na.rm = TRUE),
    #count = n(),
    mean_rho = mean(rho,na.rm = TRUE),
    percent_significant = mean(pvalue < 0.05,na.rm = TRUE) * 100)

write.csv(percent_above_threshold2, file = "./tables/spearman_scn.csv", row.names = FALSE)


percent_above_threshold3 <- combined_data %>%
  group_by(spp) %>%
  summarise(#count_above_threshold = sum(pvalue > 0.05,na.rm = TRUE),
    #count = n(),
    mean_rho = mean(rho,na.rm = TRUE),
    percent_significant = mean(pvalue < 0.05,na.rm = TRUE) * 100)

write.csv(percent_above_threshold3, file = "./tables/spearman_spp.csv", row.names = FALSE)

percent_above_threshold4 <- combined_data %>%
  group_by(scn,approach) %>%
  summarise(#count_above_threshold = sum(pvalue > 0.05,na.rm = TRUE),
    #count = n(),
    mean_rho = mean(rho,na.rm = TRUE),
    percent_significant = mean(pvalue < 0.05,na.rm = TRUE) * 100)

write.csv(percent_above_threshold4, file = "./tables/spearman_design.csv", row.names = FALSE)

###################
# contiunuation RRMSE
###################

all_df1$sqrtmean_sqdiffcv<-sqrt(all_df1$mean_sqdiffcv)
all_df1$rrmse<-all_df1$sqrtmean_sqdiffcv/all_df1$mean_cvsim

#remove 3sd of rrmse
all_df1<-all_df1[which(all_df1$rrmse <= mean(all_df1$rrmse)+3*sd(all_df1$rrmse)),]
all_df2<-all_df1[which(all_df1$spp %in% df_spp1$spp),]
all_df2[which(all_df2$approach=='sys' & all_df2$scn=='scnbase'),]

#select species and sort 
all_df2<-merge(all_df2,df_spp1,by='spp')
all_df2$scn<-factor(all_df2$scn,levels=c('scnbase','scnbase_bis',paste0('scn',3:1)))
all_df2$approach<-factor(all_df2$approach,levels=c('sys','sb','rand'))

#save rrmse
df3<-all_df2
save(df3,file = './output/rrmse_cv_hist.RData')
load('./output/rrmse_cv_hist.RData')
#remove existing bis design and EBSNBS spp
df3<-subset(df3,scn!='scnbase_bis')
df3<-subset(df3,common %in% unique(df3$common)[!grepl("_EBSNBS", as.character(unique(df3$common)))])

#for label purposes
y_scale<-aggregate(rrmse ~ spp+common+label, df3,max)
y_scale$scale<-y_scale$rrmse+y_scale$rrmse*0.25
y_scale$text<-y_scale$rrmse+y_scale$rrmse*0.20
y_scale$apr<-'sys'
y_scale$scn<-'scn1'
y_scale$year<-2022

#sort by common name
df3$common<-factor(df3$common,levels=c(df_spp1$common))
y_scale$common<-factor(y_scale$common,levels=c(df_spp1$common))

#plot
p<-
  ggplot()+
  geom_boxplot(data=df3,aes(x=scn,y=rrmse,fill=scn,group =interaction(scn,approach,spp),linetype=approach),lwd=0.6,alpha=1,outlier.alpha = 0.3,outlier.size = 1.2,outlier.stroke = 0)+
  #stat_summary(data=df3,aes(x=scn,y=rrmse,fill=scn,group =interaction(scn,approach,spp),linetype=approach),alpha=1,position = position_dodge(),geom = "crossbar", fun = "median", linetype = "solid", width = .7,linewidth=0.3)+
  scale_fill_manual(values=c('scn1'='#9B59B6','scn2'='#3498DB','scn3'='#1ABC9C','scnbase'='#696778'),
                    labels = c('existing' ,'opt depth','opt varSBT','opt depth + varSBT'),name='stratification')+
  #scale_color_manual(values=c('scnbase'='#474554','scnbase_bis'='#878787','scn3'='#8db6c3','scn2'='#679bc3','scn1'='#4b7a99'),
  #                   labels = c('existing','existing w/o corner' ,'opt depth','opt varSBT','opt depth + varSBT'),name='stratification')+
  # scale_alpha_manual(values = c('scn1'=1,'scn2'=1,'scn3'=1,'scnbase'=0.8,'scnbase_bis'=1),
  #                    labels = c('existing','existing w/o corner' ,'depth','var temp','depth + var temp'),name='stratification')+
  theme_bw()+ 
  scale_linetype_manual(values = c('sys'='solid',
                                   'sb'='dashed',
                                   'rand'='dotted'),
                        label=c('systematic','balanced random','random'),
                        name='station allocation')+
  scale_shape_manual(values=c('true index'=21),name='')+
  scale_y_continuous(expand = c(NA,0.1),limits = c(0,NA),breaks = c(0,0.5,1,1.5))+ #expand = c(NA,0.1),limits = c(0,NA)
  #                    limits =  c(0, max(df$index[,'mean']/1000) + mean(df$index[,'mean'])/10000))+
  theme(panel.grid.minor = element_line(linetype=2, color='grey90'),
        legend.key.width = unit(2.5, "lines"),
        legend.key.size = unit(20, 'points'),
        legend.direction = 'vertical',
        legend.text = element_text(size=12),
        legend.title = element_text(size=12),
        legend.spacing = unit(1, "cm"),
        legend.box.spacing = unit(0.01, "cm"),
        strip.background = element_blank(),
        legend.background = element_blank(),
        legend.box = 'horizontal',
        legend.position = 'bottom',
        strip.text = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text = element_text(size = 10)) +
  guides(
    fill = guide_legend(nrow=1, order = 1, override.aes = list(size=4)),
    color = guide_legend(nrow=1, order = 1, override.aes = list(size=4)), # Adjust linewidth here
    linetype = guide_legend(nrow=1, order = 2, override.aes = list()), # Adjust linewidth here
    shape = 'none') +  #facet_wrap(~com_sci,scales='free',dir='v',nrow = 3)  geom_blank(data=y_scale,aes(x=year,y=scale,fill=scn,group =interaction(scn,apr)))
  expand_limits(y = 0)+
  geom_text(data=y_scale,aes(label = common, y = text),x = Inf, vjust = 'inward', hjust = 1.1,size=4, lineheight = 0.8) + #,fontface='italic'
  geom_text(data=df3,aes(label = paste0(label,'        ')),x = 'scnbase', y = Inf, vjust = 1.5,size=5) + #,fontface='italic'
  geom_blank(data=y_scale,aes(x=scn,y=scale,fill=scn,group =interaction(scn,apr)))+
  facet_wrap(~common,scales='free_y',dir='h',nrow = 5)+
  expand_limits(y = 0)+
  labs(y=expression('RRMSE of '*widehat(CV)),x='')#+
#guides(fill=guide_legend(nrow=1,order=1),color=guide_legend(nrow=1,order=1),linetype=guide_legend(nrow=1,order = 2))#+

#geom_blank(data=y_scale,aes(x=scn,y=scale,fill=scn,group =interaction(scn,apr)))

#save plot
ragg::agg_png(paste0('./figures/ms_hist_rrmse_cv_box_v5.png'), width = 13, height = 8, units = "in", res = 300)
p
dev.off()

#plot all spp together
p<-
  ggplot()+
  geom_boxplot(data=df3,aes(x=scn,y=rrmse,fill=scn,group =interaction(scn,approach),linetype=approach),lwd=0.8,alpha=1,outlier.alpha = 0.3,outlier.size = 1.2,outlier.stroke = 0)+ #x=reorder(scn,value)
  #stat_summary(data=df3,aes(x=scn,y=rrmse,fill=scn,group =interaction(scn,approach),linetype=approach),alpha=1,position = position_dodge(),geom = "crossbar", fun = "median", linetype = "solid", width = .7,linewidth=0.3)+
  labs(y='RRMSE of CV',x='')+
  scale_fill_manual(values=c('scn1'='#9B59B6','scn2'='#3498DB','scn3'='#1ABC9C','scnbase'='#696778'),
                    labels = c('existing' ,'opt depth','opt varSBT','opt depth + varSBT'),name='stratification')+
  # scale_color_manual(values=c('scnbase'='#474554','scnbase_bis'='#878787','scn3'='#8db6c3','scn2'='#679bc3','scn1'='#4b7a99'),
  #                    labels = c('existing','existing w/o corner' ,'opt depth','opt varSBT','opt depth + varSBT'),name='stratification')+
  # scale_alpha_manual(values = c('scn1'=1,'scn2'=1,'scn3'=1,'scnbase'=0.8,'scnbase_bis'=1),
  #                    labels = c('existing','existing w/o corner' ,'depth','var temp','depth + var temp'),name='stratification')+
  theme_bw()+ 
  #facet_wrap(~common,scales='free_y',dir='h',nrow = 5)+
  scale_linetype_manual(values = c('sys'='solid',
                                   'sb'='dashed',
                                   'rand'='dotted'),
                        label=c('systematic','balanced random','random'),
                        name='station allocation')+
  scale_shape_manual(values=c('true index'=21),name='')+
  scale_y_continuous(labels=function(x) sprintf('%.2f',x),expand = c(NA,0.1),limits = c(0,1.7))+ #expand = c(NA,0.1),limits = c(0,NA)
  #                    limits =  c(0, max(df$index[,'mean']/1000) + mean(df$index[,'mean'])/10000))+
  theme(panel.grid.minor = element_line(linetype=2,color='grey90'),#strip.background = element_rect(fill='white'),
        legend.position=c(0.722,0.898),legend.key.size = unit(12, 'points'),legend.text = element_text(size=9), #legend.position=c(.85,.19)
        legend.title = element_text(size=10),legend.spacing.y = unit(0.05, "cm"),legend.box.spacing =  unit(0.01, "cm"), #,strip.text = element_text(size=12)
        strip.background = element_blank(),legend.box.background = element_rect(color='black'),legend.direction = 'vertical',legend.box = 'horizontal',legend.background = element_blank(),
        strip.text = element_blank(),axis.text.x = element_blank(),axis.title.x = element_blank())+ #axis.text.x = element_text(angle=90,vjust=0.5),
  expand_limits(y = 0)+
  #geom_text(data=df1,aes(label = common),x = Inf, y = Inf, hjust = 1.1, vjust = 1.5,size=4) + #,fontface='italic'
  #  geom_text(data=df1,aes(label = common),x = Inf, y = -Inf, hjust = 1.1, vjust = -0.8,size=4) + #,fontface='italic'
  guides(fill=guide_legend(ncol=1,order=1,override.aes = list(lwd=0.5)),linetype=guide_legend(ncol=1,order = 2,override.aes = list(lwd=0.5)))#+
#facet_wrap(~com_sci,scales='free',dir='v',nrow = 3)
#geom_blank(data=y_scale,aes(x=scn,y=scale,fill=scn,group =interaction(scn,apr)))

#save plot210notio
ragg::agg_png(paste0('./figures/ms_hist_indices_rrmse_box_allsp_v5.png'), width = 6, height = 5, units = "in", res = 300)
p
dev.off()

# ######################
# # BIAS of CV
# ######################
# 
# 
# df1$bias<-(df1$cvsim-df1$cvtrue)/df1$cvtrue
# 
# #get the estimated CV time series for each replicate, sampling scenario and approach
# load(file = './output/estimated_cvsim_hist.RData') #cv2
# #cv2[approach=='sb' & scn=='scnbase' & spp=='Limanda aspera' & year=='2019' & sim=='1']
# 
# #year to character
# cv2$year<-as.character(cv2$year)
# summary(cv2)
# 
# #rename
# names(cv2)[6]<-'cvsim'
# 
# #get estimated index SD for each survey across years, sampling scenario and approach
# load('./output/estimated_index_hist.RData') #ind2
# summary(ind2)
# index_sd<-aggregate(index ~ spp + year + scn + approach + sim,ind2,FUN = function(x) c(sd = sd(x)))
# summary(index_sd)
# summary(index_sd[which(index_sd$approach=='sys' & index_sd$scn=='scnbase_bis'),])
# 
# #get true ind
# load('./output/true_ind_hist.RData') #ind2
# 
# #true index reshape
# true_ind2<-reshape2::melt(true_ind,id.vars='year')
# names(true_ind2)[2]<-'spp'
# 
# #merge sd and true index
# df<-merge(index_sd,true_ind2,by=c('year','spp'))
# df$cvtrue<-df$index/df$value
# names(df)[6:7]<-c('indsim','indtrue')
# df[which(df$approach=='sys' & df$scn=='scnbase'),]
# 
# #year to character
# df$year<-as.character(df$year)
# 
# #use datatable to fasten the process
# data.table::setDT(df)
# data.table::setDT(cv2)
# #cv2<-data.table(cv2)
# 
# dim(cv2)
# df1<-merge(cv2,df,by=c('spp','scn','approach','sim','year'),all.x=TRUE,allow.cartesian=TRUE)
# #dim(df)
# dim(df1)
# 
# df1$sqdiffcv<-(df1$cvsim-df1$cvtrue)^2
# df1$diffcv<-(df1$cvsim-df1$cvtrue)
# df1[which(df1$approach=='sys' & df1$scn=='scnbase'),]
# 
# df2<-df1[, .(mean_sqdiffcv = mean(sqdiffcv,na.rm=FALSE),cvtrue = cvtrue,mean_diffcv = mean(diffcv,na.rm=FALSE),mean_cvsim=mean(cvsim,na.rm=FALSE),mean_cvtrue=mean(cvtrue,na.rm=FALSE)), by = .(spp,scn,approach,sim,year)]
# df2$sqrtmean_sqdiffcv<-sqrt(df2$mean_sqdiffcv)
# df2$rrmse<-df2$sqrtmean_sqdiffcv/df2$mean_cvsim
# df2$df2$mean_diffcv/df2$cvtrue
# 
# 
# #remove 3sd of rrmse
# df2<-df2[which(df2$rrmse <= mean(df2$rrmse)+3*sd(df2$rrmse)),]
# df3<-df2[which(df2$spp %in% df_spp1$spp),]
# 
# #select species and sort 
# df3<-merge(df3,df_spp1,by='spp')
# df3$scn<-factor(df3$scn,levels=c('scnbase','scnbase_bis',paste0('scn',3:1)))
# df3$approach<-factor(df3$approach,levels=c('sys','sb','rand'))
# 
# save(df3,file = './output/rrmse_cv_hist.RData')
# 
# y_scale<-aggregate(rrmse ~ spp+common+label, df3,max)
# y_scale$scale<-y_scale$rrmse+y_scale$rrmse*0.2
# y_scale$text<-y_scale$rrmse+y_scale$rrmse*0.15
# y_scale$apr<-'sys'
# y_scale$scn<-'scn1'
# y_scale$year<-2022
# 
# 
# 
# 
# 
# #get the estimated CV time series for each replicate, sampling scenario and approach
# #load(file = './output/estimated_cvsim_hist.RData') #cv2
# #true cv
# 
# 
# 
# #load(file = './output/rrmse_cv_hist.RData') #df3
# 
# df3$cvbias<-df3$mean_cvsim-df3$mean_cvtrue
#   df3$rbias<-100*(df3$cvbias/df3$mean_cvtrue)
# 
# y_scale<-aggregate(rbias ~ spp+common+label, df3,max)
# y_scale$scale<-y_scale$rbias+y_scale$rbias*0.2
# y_scale$text<-y_scale$rbias+y_scale$rbias*0.15
# y_scale$apr<-'sys'
# y_scale$scn<-'scn1'
# y_scale$year<-2022
# y_scale$common<-factor(y_scale$common,levels=c(df_spp1$common))
# 
# #plot
# # #p<-
# #   ggplot()+
# #   geom_boxplot(data=df3,aes(x=scn,y=rbias,fill=scn,group =interaction(scn,approach,spp),linetype=approach),alpha=1,outlier.alpha = 0.3,outlier.size = 1.2,outlier.stroke = 0)+ #x=reorder(scn,value)
# #   stat_summary(data=df3,aes(x=scn,y=rbias,fill=scn,group =interaction(scn,approach,spp),linetype=approach),alpha=1,position = position_dodge(),geom = "crossbar", fun = "median", linetype = "solid", width = .7,linewidth=0.3)+
# #   scale_fill_manual(values=c('scnbase'='#474554','scnbase_bis'='#878787','scn3'='#8db6c3','scn2'='#679bc3','scn1'='#4b7a99'),
# #                     labels = c('existing','existing w/o corner' ,'opt depth','opt varSBT','opt depth + varSBT'),name='stratification')+
# #   scale_color_manual(values=c('scnbase'='#474554','scnbase_bis'='#878787','scn3'='#8db6c3','scn2'='#679bc3','scn1'='#4b7a99'),
# #                      labels = c('existing','existing w/o corner' ,'opt depth','opt varSBT','opt depth + varSBT'),name='stratification')+
# #   # scale_alpha_manual(values = c('scn1'=1,'scn2'=1,'scn3'=1,'scnbase'=0.8,'scnbase_bis'=1),
# #   #                    labels = c('existing','existing w/o corner' ,'depth','var temp','depth + var temp'),name='stratification')+
# #   theme_bw()+ 
# #   scale_linetype_manual(values = c('sys'='solid',
# #                                    'sb'='dashed',
# #                                    'rand'='dotted'),
# #                         label=c('systematic','balanced random','random'),
# #                         name='station allocation')+
# #   scale_shape_manual(values=c('true index'=21),name='')+
# #   scale_y_continuous(labels=function(x) sprintf('%.2f',x),expand = c(NA,0.1),limits = c(0,NA))+ #expand = c(NA,0.1),limits = c(0,NA)
# #   #                    limits =  c(0, max(df$index[,'mean']/1000) + mean(df$index[,'mean'])/10000))+
# #   theme(panel.grid.minor = element_line(linetype=2,color='grey90',),#strip.background = element_rect(fill='white'),
# #         legend.key.size = unit(12, 'points'),legend.direction = 'vertical',legend.text = element_text(size=9),axis.text.x = element_blank(), #legend.position=c(.85,.19)
# #         legend.title = element_text(size=10),legend.spacing.x = unit(0.05, "cm"),legend.box.spacing =  unit(0.01, "cm"), #,strip.text = element_text(size=12)
# #         strip.background = element_blank(),legend.background = element_blank(),legend.box = 'vertical',legend.position = 'right',#legend.justification = 'right',legend.position='bottom',#legend.position=c(.84,.05),
# #         strip.text = element_blank(),axis.title.x = element_blank())+ #axis.text.x = element_text(angle=90,vjust=0.5),
# #   expand_limits(y = 0)+
# #   geom_text(data=y_scale,aes(label = common, y = text),x = Inf, vjust = 'inward', hjust = 1.1,size=4, lineheight = 0.8) + #,fontface='italic'
# #   geom_text(data=df3,aes(label = paste0(label,'      ')),x = 'scnbase', y = Inf, vjust = 1.5,size=5) + #,fontface='italic'
# #   geom_blank(data=y_scale,aes(x=scn,y=scale,fill=scn,group =interaction(scn,apr)))+
# #   facet_wrap(~common,scales='free_y',dir='h',nrow = 5)+
# #   expand_limits(y = 0)+
# #   labs(y='% rbias of CV',x='')+
# #   guides(fill=guide_legend(ncol=1,order=1),color=guide_legend(ncol=1,order=1),linetype=guide_legend(ncol=1,order = 2))#+
# 
# 
#   #plot
#   p<-
#     ggplot()+
#     geom_boxplot(data=df3,aes(x=scn,y=rbias,fill=scn,group =interaction(scn,approach,spp),linetype=approach),lwd=0.6,alpha=1,outlier.alpha = 0.3,outlier.size = 1.2,outlier.stroke = 0)+
#     #stat_summary(data=df3,aes(x=scn,y=rrmse,fill=scn,group =interaction(scn,approach,spp),linetype=approach),alpha=1,position = position_dodge(),geom = "crossbar", fun = "median", linetype = "solid", width = .7,linewidth=0.3)+
#     scale_fill_manual(values=c('scn1'='#3498DB','scn2'='#1ABC9C','scn3'='#9B59B6','scnbase'='#474554'),
#                       labels = c('existing' ,'opt depth','opt varSBT','opt depth + varSBT'),name='stratification')+
#     #scale_color_manual(values=c('scnbase'='#474554','scnbase_bis'='#878787','scn3'='#8db6c3','scn2'='#679bc3','scn1'='#4b7a99'),
#     #                   labels = c('existing','existing w/o corner' ,'opt depth','opt varSBT','opt depth + varSBT'),name='stratification')+
#     # scale_alpha_manual(values = c('scn1'=1,'scn2'=1,'scn3'=1,'scnbase'=0.8,'scnbase_bis'=1),
#     #                    labels = c('existing','existing w/o corner' ,'depth','var temp','depth + var temp'),name='stratification')+
#     theme_bw()+ 
#     scale_linetype_manual(values = c('sys'='solid',
#                                      'sb'='dashed',
#                                      'rand'='dotted'),
#                           label=c('systematic','balanced random','random'),
#                           name='station allocation')+
#     scale_shape_manual(values=c('true index'=21),name='')+
#     scale_y_continuous(labels=function(x) sprintf('%.2f',x))+ #expand = c(NA,0.1),limits = c(0,NA)
#     #                    limits =  c(0, max(df$index[,'mean']/1000) + mean(df$index[,'mean'])/10000))+
#     theme(panel.grid.minor = element_line(linetype=2,color='grey90',),#strip.background = element_rect(fill='white'),
#           legend.key.size = unit(12, 'points'),legend.direction = 'vertical',legend.text = element_text(size=9),axis.text.x = element_blank(), #legend.position=c(.85,.19)
#           legend.title = element_text(size=10),legend.spacing.x = unit(0.05, "cm"),legend.box.spacing =  unit(0.01, "cm"), #,strip.text = element_text(size=12)
#           strip.background = element_blank(),legend.background = element_blank(),legend.box = 'vertical',legend.position = 'right',#legend.justification = 'right',legend.position='bottom',#legend.position=c(.84,.05),
#           strip.text = element_blank(),axis.title.x = element_blank())+ #axis.text.x = element_text(angle=90,vjust=0.5),
#     expand_limits(y = 0)+
#     geom_text(data=y_scale,aes(label = common, y = text),x = Inf, vjust = 'inward', hjust = 1.1,size=4, lineheight = 0.8) + #,fontface='italic'
#     geom_text(data=df3,aes(label = paste0(label,'      ')),x = 'scnbase', y = Inf, vjust = 1.5,size=5) + #,fontface='italic'
#     geom_blank(data=y_scale,aes(x=scn,y=scale,fill=scn,group =interaction(scn,apr)))+
#     facet_wrap(~common,scales='free_y',dir='h',nrow = 5)+
#     expand_limits(y = 0)+
#     labs(y='% rbias of CV',x='')+
#     guides(fill=guide_legend(ncol=1,order=1),color=guide_legend(ncol=1,order=1),linetype=guide_legend(ncol=1,order = 2))#+
#   
#   #geom_blank(data=y_scale,aes(x=scn,y=scale,fill=scn,group =interaction(scn,apr)))
#   
#   
#   ragg::agg_png(paste0('./figures/ms_hist_bias_cv_box_v3.png'), width = 13, height = 8, units = "in", res = 300)
#   p
#   dev.off()
#   
#   
#   #p<-
#     ggplot()+
#     geom_boxplot(data=df3,aes(x=scn,y=rbias,fill=scn,group =interaction(scn,approach),linetype=approach),lwd=0.8,alpha=1,outlier.alpha = 0.3,outlier.size = 1.2,outlier.stroke = 0)+ #x=reorder(scn,value)
#     #stat_summary(data=df3,aes(x=scn,y=rrmse,fill=scn,group =interaction(scn,approach),linetype=approach),alpha=1,position = position_dodge(),geom = "crossbar", fun = "median", linetype = "solid", width = .7,linewidth=0.3)+
#     labs(y='% rbias of CV',x='')+
#     scale_fill_manual(values=c('scn1'='#3498DB','scn2'='#1ABC9C','scn3'='#9B59B6','scnbase'='#474554'),
#                       labels = c('existing' ,'opt depth','opt varSBT','opt depth + varSBT'),name='stratification')+
#     # scale_color_manual(values=c('scnbase'='#474554','scnbase_bis'='#878787','scn3'='#8db6c3','scn2'='#679bc3','scn1'='#4b7a99'),
#     #                    labels = c('existing','existing w/o corner' ,'opt depth','opt varSBT','opt depth + varSBT'),name='stratification')+
#     # scale_alpha_manual(values = c('scn1'=1,'scn2'=1,'scn3'=1,'scnbase'=0.8,'scnbase_bis'=1),
#     #                    labels = c('existing','existing w/o corner' ,'depth','var temp','depth + var temp'),name='stratification')+
#     theme_bw()+ 
#     #facet_wrap(~common,scales='free_y',dir='h',nrow = 5)+
#     scale_linetype_manual(values = c('sys'='solid',
#                                      'sb'='dashed',
#                                      'rand'='dotted'),
#                           label=c('systematic','balanced random','random'),
#                           name='station allocation')+
#     scale_shape_manual(values=c('true index'=21),name='')+
#     scale_y_continuous(labels=function(x) sprintf('%.2f',x))+ #expand = c(NA,0.1),limits = c(0,NA)
#     #                    limits =  c(0, max(df$index[,'mean']/1000) + mean(df$index[,'mean'])/10000))+
#     theme(panel.grid.minor = element_line(linetype=2,color='grey90'),#strip.background = element_rect(fill='white'),
#           legend.position=c(0.722,0.898),legend.key.size = unit(12, 'points'),legend.text = element_text(size=9), #legend.position=c(.85,.19)
#           legend.title = element_text(size=10),legend.spacing.y = unit(0.05, "cm"),legend.box.spacing =  unit(0.01, "cm"), #,strip.text = element_text(size=12)
#           strip.background = element_blank(),legend.box.background = element_rect(color='black'),legend.direction = 'vertical',legend.box = 'horizontal',legend.background = element_blank(),
#           strip.text = element_blank(),axis.text.x = element_blank(),axis.title.x = element_blank())+ #axis.text.x = element_text(angle=90,vjust=0.5),
#     expand_limits(y = 0)+
#     #geom_text(data=df1,aes(label = common),x = Inf, y = Inf, hjust = 1.1, vjust = 1.5,size=4) + #,fontface='italic'
#     #  geom_text(data=df1,aes(label = common),x = Inf, y = -Inf, hjust = 1.1, vjust = -0.8,size=4) + #,fontface='italic'
#     guides(fill=guide_legend(ncol=1,order=1,override.aes = list(lwd=0.5)),linetype=guide_legend(ncol=1,order = 2,override.aes = list(lwd=0.5)))#+
#   #facet_wrap(~com_sci,scales='free',dir='v',nrow = 3)
#   #geom_blank(data=y_scale,aes(x=scn,y=scale,fill=scn,group =interaction(scn,apr)))
#   
#   # geom_boxplot(data=df1,aes(x=scn,y=value,fill=scn,group =interaction(scn,approach),linetype=approach),lwd=0.8,alpha=1,outlier.alpha = 0.3,outlier.size = 1.2,outlier.stroke = 0)+ #x=reorder(scn,value)
#   #   #stat_summary(data=df1,aes(x=scn,y=value,fill=scn,group =interaction(scn,approach),linetype=approach),alpha=1,position = position_dodge(),geom = "crossbar", fun = "median", linetype = "solid", width = .7,linewidth=0.4)+
#   #   labs(y='CV',x='')+
#   #   scale_fill_manual(values=c('scnbase'='#474554','scnbase_bis'='#878787','scn3'='#8db6c3','scn2'='#679bc3','scn1'='#4b7a99'),
#   #                     labels = c('existing','existing w/o corner' ,'opt depth','opt varSBT','opt depth + varSBT'),name='stratification')+
#   #   # scale_color_manual(values=c('scnbase'='#474554','scnbase_bis'='#878787','scn3'='#8db6c3','scn2'='#679bc3','scn1'='#4b7a99'),
#   #   #                    labels = c('existing','existing w/o corner' ,'opt depth','opt varSBT','opt depth + varSBT'),name='stratification')+
#   #   # scale_alpha_manual(values = c('scn1'=1,'scn2'=1,'scn3'=1,'scnbase'=0.8,'scnbase_bis'=1),
#   #   #                    labels = c('existing','existing w/o corner' ,'depth','var temp','depth + var temp'),name='stratification')+
#   #   theme_bw()+ 
#   #   #facet_wrap(~common,scales='free_y',dir='h',nrow = 5)+
#   #   scale_linetype_manual(values = c('sys'='solid',
#   #                                    'sb'='dashed',
#   #                                    'rand'='dotted'),
#   #                         label=c('systematic','balanced random','random'),
#   #                         name='station allocation')+
#   #   scale_shape_manual(values=c('true index'=21),name='')+
#   #   scale_y_continuous(labels=function(x) sprintf('%.2f',x),expand = c(NA,0.1),limits = c(0,0.9))+ #expand = c(NA,0.1),limits = c(0,NA)
#   #   #                    limits =  c(0, max(df$index[,'mean']/1000) + mean(df$index[,'mean'])/10000))+
#   #   theme(panel.grid.minor = element_line(linetype=2,color='grey90'),#strip.background = element_rect(fill='white'),
#   #         legend.position=c(0.721,0.881),legend.key.size = unit(12, 'points'),legend.text = element_text(size=9), #legend.position=c(.85,.19)
#   #         legend.title = element_text(size=10),legend.spacing.y = unit(0.05, "cm"),legend.box.spacing =  unit(0.01, "cm"), #,strip.text = element_text(size=12)
#   #         strip.background = element_blank(),legend.box.background = element_rect(color='black'),legend.direction = 'vertical',legend.box = 'horizontal',legend.background = element_blank(),
#   #         strip.text = element_blank(),axis.text.x = element_blank(),axis.title.x = element_blank())+ #axis.text.x = element_text(angle=90,vjust=0.5),
#   #   expand_limits(y = 0)+
#   #   #geom_text(data=df1,aes(label = common),x = Inf, y = Inf, hjust = 1.1, vjust = 1.5,size=4) + #,fontface='italic'
#   #   #  geom_text(data=df1,aes(label = common),x = Inf, y = -Inf, hjust = 1.1, vjust = -0.8,size=4) + #,fontface='italic'
#   #   guides(fill=guide_legend(ncol=1,order=1,override.aes = list(lwd=0.5)),linetype=guide_legend(ncol=1,order = 2,override.aes = list(lwd=0.5)))#+
#   # #facet_wrap(~com_sci,scales='free',dir='v',nrow = 3)
#   # #geom_blank(data=y_scale,aes(x=scn,y=scale,fill=scn,group =interaction(scn,apr)))
#   
#   #save plot210notio
#   ragg::agg_png(paste0('./figures/ms_hist_indices_bias_box_allsp_v3.png'), width = 6, height = 5, units = "in", res = 300)
#   p
#   dev.off()
