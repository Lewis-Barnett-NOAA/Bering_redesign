#Script#9 survey performance for slope Bering Sea
#danielvilasgonzalez@gmail.com/dvilasg@uw.edu
#calculate BIAS + RRMSE + CV of INDEX and CV of INDEX

# SETTINGS #####

#clear all objects
rm(list = ls(all.names = TRUE)) 
#free up memrory and report the memory usage
gc() 

#libraries from cran to call or install/load
pack_cran<-c('raster','units','ggplot2','data.table','sf','reshape2','data.table','dplyr','tidyr')

#install pacman to use p_load function - call library and if not installed, then install
if (!('pacman' %in% installed.packages())) {
  install.packages("pacman")}

#install VAST if it is not
if (!('VAST' %in% installed.packages())) {
  devtools::install_github("james-thorson/VAST@main", INSTALL_opts="--no-staged-install")};library(VAST)

#load/install packages
pacman::p_load(pack_cran,character.only = TRUE)

#setwd
#out_dir<-'/Users/daniel/Work/UW-NOAA/Adapting Monitoring to a Changing Seascape/'
#setwd(out_dir)

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

#create folder simulation data
dir.create(paste0('output/slope//species/'))

#years - only years when BSS and NBS+EBS OM temporally overlap
yrs<-c(2002:2016)
n_yrs<-length(yrs)

# GRIDS #####
#load grid of NBS and EBS
northern_bering_sea_grid <- FishStatsUtils::northern_bering_sea_grid
eastern_bering_sea_grid <- FishStatsUtils::eastern_bering_sea_grid
bering_sea_slope_grid <- FishStatsUtils::bering_sea_slope_grid
colnames(bering_sea_slope_grid)[4]<-'Stratum'
bering_sea_slope_grid$Stratum<-NA
grid<-as.data.frame(rbind(data.frame(northern_bering_sea_grid,region='NBS'),
                          data.frame(eastern_bering_sea_grid,region='EBS'),
                          data.frame(bering_sea_slope_grid,region='SBS')))
grid$cell<-1:nrow(grid)
grid2<-grid

#FIND SLOPE CELLS DEEPER than 400m
load(file = 'data/data_processed/grid_EBS_NBS.RData') #grid.ebs_year$region
grid_slp<-subset(grid.ebs_year,region=='EBSslope' & Year=='1982')
dim(grid_slp)
dim(grid_slp[which(grid_slp$DepthGEBCO<=400),])
ok_slp_cells<-as.numeric(row.names(grid_slp)[which(grid_slp$DepthGEBCO<=400)])
#rem_slp_cells<-as.numeric(row.names(grid_slp)[which(grid_slp$DepthGEBCO>400)])

# Sampling designs #####

# Get SAMPLES DENSITY for the EBS and find for BSS and NBS

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

# ESTIMATED ABUNDANCE INDEX  #####
est_ind_file<-'output/slope//estimated_index_hist_dens.RData'
if (!file.exists(est_ind_file)) {
  # code to run when the file is missing
  message("File not found, running code.")
  
  #dataframe to store estimated indices
  ind2<-data.frame(matrix(NA,nrow = 0,ncol = 7))
  names(ind2)<-c('spp','year','approach','sur','scn','index','sim')
  
  #list of files (100 files, one for each simulated data)
  files<-list.files('output/slope//ms_sim_survey_hist/',pattern = 'index_hist_dens',recursive = TRUE,full.names = TRUE)
  
  sims<-list.files('output/slope/ms_sim_survey_hist/')
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
  save(ind2,file = 'output/slope//estimated_index_hist_dens.RData') #ind2

} else {
  # code to run when the file is missing
  message("File present, loading...")
  
  load(est_ind_file)
}

#aggregate df to get mean, q95 and q5 for each group (sp, year, sampling scenario and approach)
df<-aggregate(index ~ spp + year + scn + approach + regime,ind2,FUN = function(x) c(mean = mean(x), q95 = quantile(x,probs=0.95) , q5 = quantile(x,probs=0.05)) )
colnames(df$index)<-c('mean','q95','q5')

#merge df and sp df
df<-merge(df,df_spp1,by='spp')
df$year<-as.integer(df$year)

# TRUE DENSITIES - ABUNDANCE INDEX ##### 
# from OMs FOR NBS+EBS and SLOPE 

#23 spp and 4 regions (EBS, NBS+EBS, EBS+SBS and NBS+EBS+SBS)
#loop over spp

### BSS  #####
true_index_file <-"output/slope//model_based_EBSNBSBSS.RData"

if (!file.exists(true_index_file)) {
  
  # code to run when the file is missing
  message("File not found, running code.")
  
  dens_index_hist_OM<-list()
  
  #loop over spp
  for (sp in slp_conv) {
    
    #sp<-spp[5] #20
    
    cat(paste(sp,'\n'))
      
    mod1<-'fit_st.RData'
      
    #get list of fit data
    ff<-list.files(paste0('./output/slope/vast/',sp),mod1,recursive = TRUE)
    
    #load fit file
    load(paste0('./output/slope/vast/',sp,'/',ff)) #fit
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
  
  save(dens_index_hist_OM, file = paste0("output/slope//species/dens_index_hist_OM_slope.RData")) 
  load(file = paste0("output/slope//species/dens_index_hist_OM_slope.RData")) #dens_index_hist_OM
  
  ### NBS+EBS  #####
  
  dens_index_hist_OM<-list()
  
  #loop over spp
  for (sp in ebsnbs_conv) {
    
    #sp<-ebsnbs_conv[1] #20
    
    cat(paste(sp,'\n'))
    
    
    #get list of fit data
    ff<-list.files(paste0('./output/vast/',sp),'fit',recursive = TRUE)
    
    #load fit file
    load(paste0('./output/vast/',sp,'/',ff)) #fit
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
  
  save(dens_index_hist_OM, file = paste0("output/slope//species/dens_index_hist_OM_ebsnbs.RData")) 
  load(paste0("output/slope//species/dens_index_hist_OM_ebsnbs.RData")) #dens_index_hist_OM
  
  ####### NBS + EBS + BSS  #####
  
  #load true ebsnbs index
  load(file = paste0("output/slope//species/dens_index_hist_OM_ebsnbs.RData")) 
  ind_ebsnbs<-dens_index_hist_OM
  
  #load true slope index
  load(file = paste0("output/slope//species/dens_index_hist_OM_slope.RData")) 
  ind_slope<-dens_index_hist_OM
  
  #df to store results
  true_ind<-data.frame(matrix(NA,nrow = length(yrs),ncol = length(spp)))
  rownames(true_ind)<-yrs
  colnames(true_ind)<-c(spp)
  
  #array true ind
  true_ind<-
  array(dim=c(length(yrs),5,length(spp)),
        dimnames = list(yrs,c('year','EBS','EBS+NBS','EBS+BSS','EBS+NBS+BSS'),spp))
  
  #loop over species stock to extract the true index
  for (sp in spp) {
    
     #sp<-spp[18]
    
    if (sp %in% names(ind_ebsnbs)) {
      
      #get index ebs nbs
      ind_ebsnbs1<-drop_units(ind_ebsnbs[[sp]]$index[,as.character(yrs),1])
      ind_nbs1<-drop_units(ind_ebsnbs[[sp]]$index[,as.character(yrs),2])
      ind_ebs1<-drop_units(ind_ebsnbs[[sp]]$index[,as.character(yrs),3])
    } else {
      ind_ebsnbs1<-rep(0,length(yrs))
      ind_nbs1<-rep(0,length(yrs))
      ind_ebs1<-rep(0,length(yrs))
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
    ebsnbsbss<-ind_ebsnbs1+ind_slope2
    ebsbss<-ind_ebs1+ind_slope2
      
    #append total index
    true_ind[,'EBS',sp]<-ind_ebs1
    true_ind[,'EBS+NBS',sp]<-ind_ebsnbs1
    true_ind[,'EBS+BSS',sp]<-ebsbss
    true_ind[,'EBS+NBS+BSS',sp]<-ebsnbsbss
    true_ind[,'year',sp]<-c(2002:2016)
  }
  
  true_ind[c(9,15),,'Sebastes melanostictus']<-NA
  
  #save true ind
  save(true_ind,file = paste0("output/slope/model_based_EBSNBSBSS.RData"))  
  
  # reshape into dataframe
  df_list <- lapply(dimnames(true_ind)[[3]], function(sp) {
    df_tmp <- as.data.frame(true_ind[, , sp])
    df_tmp$year <- as.numeric(rownames(df_tmp))
    df_tmp$species <- sp
    df_tmp
  })
  
  df_wide <- do.call(rbind, df_list)
  write.csv(df_wide, file = paste0("output/slope/model_based_EBSNBSBSS.csv"),
            row.names = FALSE)
  
} else {
  # code to run when the file is missing
  message("File present, loading...")
  
  #load true ind
  load(file = true_index_file)  #true_ind
  
}

#arrange true index data
true_ind1<-reshape2::melt(true_ind,id.vars='year')
true_ind1 <- reshape2::melt(true_ind, varnames = c("year", "region", "spp"), value.name = "value")
true_ind1<-subset(true_ind1,region!='year')
true_ind1<-true_ind1[which(true_ind1$spp %in% df_spp1$spp),]
true_ind1<-merge(true_ind1,df_spp1,by='spp')
true_ind1$year<-as.integer(true_ind1$year)
true_ind1$dummy<-'true index'

#fxn to turn axis into scientific
scientific_10 <- function(x) {
  parse(text=gsub("e", " %*% 10^", scales::scientific_format()(x)))
}

#sort approach (station allocation)
df$approach <- factor(df$approach, levels = c("sb", "rand"))

#to adjust y axis limits
df$value<-df$index[,'mean']/1000
y_scale<-aggregate(value ~ common, df,max)
y_scale$scale<-y_scale$value+y_scale$value*0.25
y_scale$text<-y_scale$value+y_scale$value*0.20
y_scale$apr<-'sys'
y_scale$year<-2010
y_scale$scn<-'scn1'

#Our transformation function
scaleFUN <- function(x) sprintf("%.3f", x)

#check
df[which(df$spp=='Sebastes melanostictus'),]
#true_ind1[which(true_ind1$spp=='Sebastes melanostictus'),]

#samp_df
df<-merge(df,samp_df[,c("type","region","strat_var","samp_scn")],by.x='scn',by.y='samp_scn',all.x=TRUE)
#df<-subset(df,scn %in% paste0('scn',1:16))

#save true ind
save(df,file = paste0("output/slope/design_based_EBSNBSBSS.RData"))
write.csv(df, file = "output/slope/design_based_EBSNBSBSS.csv", row.names = FALSE)
#load(file = paste0("output/slope/design_based_EBSNBSBSS.RData"))  

levels(df$region)<-c("EBS","EBS+NBS","EBS+BSS","EBS+NBS+BSS")

y_scale[11,'text']<-77000
y_scale[11,'scale']<-80000

#plot abundance index for each sampling design
p<-
    ggplot() +
    geom_line(data=df, aes(x=year, y=index[,'mean']/1000000000, color=region, group=interaction(scn, approach, common,regime), 
                           linetype=approach), linewidth=0.7, alpha=0.5) +
    geom_point(data=true_ind1, aes(x=year, y=value/1000000000, group=common, shape=region,fill=region), 
               color='black', size=2) +
    labs(y=expression('MT ('* 10^6 * ' tons)'), x='') +
    scale_color_manual(values = c('EBS' = '#d62728',   # Blue
                                    'EBS+NBS' = '#1f77b4',  # Purple
                                    'EBS+BSS' = '#2ca02c',  # Green
                                    'EBS+NBS+BSS' = '#9467bd'),  # Red
                         name = 'design-based')+    # scale_alpha_manual(values = c('scn1'=1,'scn2'=1,'scn3'=1,'scnbase'=0.8),
    #                    labels = c('existing' ,'depth','var temp','depth + var temp'), name='stratification') +
    theme_bw() +
    scale_linetype_manual(values = c('sb'='dashed', 'rand'='solid'),
                          labels=c('balanced random','random'),
                          name='station allocation') +
    scale_shape_manual(values = c('EBS' = 0,   # Blue
                                  'EBS+NBS' =1,  # Purple
                                  'EBS+BSS' = 3,  # Green
                                  'EBS+NBS+BSS' =4)
                       ,name='model-based') +
      scale_fill_manual(values = c('EBS' = '#d62728',   # Blue
                                    'EBS+NBS' = '#1f77b4',  # Purple
                                    'EBS+BSS' = '#2ca02c',  # Green
                                    'EBS+NBS+BSS' = '#9467bd'),  # Red
                         name = 'model-based')+    # scale_alpha_manual(values = c('scn1'=1,'scn2'=1,'scn3'=1,'scnbase'=0.8),
    scale_y_continuous(expand = c(0,0), limits = c(0,NA), labels=scaleFUN) +
    expand_limits(y = 0) +
    geom_text(data=y_scale, aes(label = common, y = text/1000000), x = Inf, vjust = 'inward', hjust = 1.1, size=4, lineheight = 0.8) +
    geom_text(data=df, aes(label = label), x = 1984, y = Inf, vjust = 1.5, size=5) +
      theme(
        legend.box = 'horizontal',  # Stack legends on top of each other
        legend.direction = 'vertical',  # Each legend’s elements are horizontal
        legend.position = c(0.8, 0.075),  # Justify the legend to the right
        legend.justification = 'center',  # Ensure it’s centered vertically
        legend.title = element_text(size = 11, hjust = 0.5),  # Title font size and centering
        legend.text = element_text(size = 10, color = "black"),  # Text size and color
        legend.title.align = 0.5,  # Center the legend titles
        legend.spacing = unit(0.2, "cm"),  # Reduce space between legend elements
        legend.box.spacing = unit(0.2, "cm"),  # Reduce space between stacked legends
        legend.background = element_blank(),  # No background color for the legend
        strip.background = element_blank(),
        strip.text = element_blank(),legend.spacing.y = unit(0.1,'cm'),legend.key.spacing.y = unit(0.1,'cm'),
        #legend.justification = "center",
        axis.title.x = element_blank()
      ) +
      guides(
        color = guide_legend(order = 1, title.position = "top", override.aes = list(linewidth = 1)),
        linetype = guide_legend(order = 3, title.position = "top", override.aes = list(linewidth = 0.6, keywidth = 2, keyheight = 1.5)),
        shape = guide_legend(order = 2, title.position = "top", override.aes = list(size = 3)),
        fill = guide_legend(order = 2, title.position = "top", override.aes = list(size = 3))
      )+
    geom_blank(data=y_scale, aes(x=year, y=scale/1000000, fill=scn, group=interaction(scn, apr))) +
    facet_wrap(~common, scales='free_y', dir='h', nrow = 5)

#save index plot
ragg::agg_png(paste0('figures/slope/ms_ind_EBSNBSBSS.png'), width = 14, height = 8, units = "in", res = 300)
p
dev.off()

names(true_ind1)[4]<-'true_index'
df2<-(merge(df,true_ind1,by=c('spp','common','year','label','region')))
df2$diff<-(df2$index[,'mean']-(df2$true_index))/(df2$true_index)

#to adjust y axis limits
df2$value<-df2$diff
y_scale<-aggregate(value ~ common, df2,max)
y_scale$scale<-y_scale$value+y_scale$value*0.2
y_scale$text<-y_scale$value+y_scale$value*0.17
y_scale$apr<-'rand'
y_scale$year<-2010
y_scale$scn<-'scn1'

#plot
  ggplot()+
  geom_line(data=df2,aes(x=year,y=diff,color=scn,group=interaction(scn,approach,common,regime),linetype=approach),linewidth=1.5,alpha=0.7)+
  labs(y=expression("("*hat('I')*" - I ) / I"),x='')+
  theme_bw()+
  scale_linetype_manual(values = c(
                                   'sb'='dashed',
                                   'rand'='solid'),
                        label=c('balanced random','random'),
                        name='station allocation')+
  scale_x_continuous(expand=c(0,0),
                     breaks = c(1985,1990,1995,2000,2005,2010,2015,2020),
                     minor_breaks = setdiff(1982:2022,c(1982,1985,1990,1995,2000,2005,2010,2015,2020,2022))) +
  #scale_y_continuous(expand = c(0,0), limits = c(0,NA), labels=scaleFUN) +
  theme(panel.grid.minor = element_line(linetype=2, color='grey90'),
        strip.background = element_blank(),
        legend.background = element_blank(),
        strip.text = element_blank(),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 10)) +
  expand_limits(y = 0)+
  geom_text(data=y_scale,aes(label = common, y = text),x = Inf, vjust = 'inward', hjust = 1.1,size=4, lineheight = 0.8) + #,fontface='italic'
  geom_text(data=df2,aes(label = label),x = 1984, y = Inf, vjust = 1.5,size=5) + #,fontface='italic'
  geom_blank(data=y_scale,aes(x=year,y=scale,fill=scn,group =interaction(scn,apr)))+
  facet_wrap(~common,scales='free_y',dir='h',nrow = 5)

  
  #1 TRUE CV  #####
  
  EBS_cells<-15181:53464
  BSS_cells<-53465:56505
  NBS_cells<-1:15180
  
  #get CV for each replicate, approach and sampling scenario
  #CV are calculated on the script #13 as, 
  ### CV <- sqrt(STRS_var) / STRS_mean 
  ### STRS_var<-sum(strs_var, by=year) ### strs_var<-var*area²/samples ### var<-var(CPUE)
  ### STRS_mean<-sum(index_strata, by=year) ### index_strata<-mean_strata*area ### mean_strata<-mean(CPUE)
  #get the true index for each simulated data
  
  #store HIST simulated data
  load(file = paste0('output/slope//species/ms_sim_dens_all.RData'))  #sim_dens1
  
  
  true_index_file <-"output/slope/true_ind_hist.RData"
  
  if (!file.exists(true_index_file)) {
    
    # code to run when the file is missing
    message("File not found, running code.")
  
    #df to store results
    true_ind<-array(dim=c(length(spp),length(yrs),4,100),
                    dimnames = list(spp,yrs,c('EBS','EBS+BSS','EBS+NBS','EBS+NBS+BSS'),1:100))
    
    #loop over species and crab stock to extract the true index
    for (sim in dimnames(sim_dens1)[[4]]) {
    
    #sim<-1
    
    # Print progress
    cat(paste0('##### sim ', sim, '\n'))
    
    #get dens for species and yrs and sim
    dens<-sim_dens1[,spp,as.character(yrs),sim]
    
    for (region in c('EBS','EBS+BSS','EBS+NBS','EBS+NBS+BSS')) {
      
      #region<-'EBS+BSS';y='2016'
      
      if (region=='EBS') {
        cells<-EBS_cells
      } else if (region=='EBS+BSS') {
        cells<-c(EBS_cells,BSS_cells)
      } else if (region=='EBS+NBS') {
        cells<-c(EBS_cells,NBS_cells)
      } else if (region=='EBS+BSS+NBS') {
        cells<-c(EBS_cells,BSS_cells,NBS_cells)
      }      
      
      dens1<-colSums(dens[cells,,])
      true_ind[,,region,sim]<-dens1
    }
    
    #true_ind[,sp]<-drop_units(dens_index_hist_OM[[sp]]$index[,as.character(yrs),1])
    
    }
    
    #save true ind
    save(true_ind,file = paste0("output/slope/true_ind_hist.RData"))  
    
  } else {
    
    # code to run when the file is missing
    message("File present, loading true index file")
    load(file = paste0("output/slope/true_ind_hist.RData"))  
    
  }
  
    
  #as.data.frame
  #add scn based on region
  true_ind1 <- as.data.frame.table(true_ind)
  names(true_ind1)<-c('spp','year','region','sim','true_ind')
  dim(true_ind1)
  
  
  true_cv_file<-'output/slope//estimated_cvtrue_dens.RData'
  
  if (!file.exists(true_cv_file)) {
    
    # code to run when the file is missing
    message("File not found, running code.")
    
    #save simulated index
    load('output/slope/estimated_index_hist_dens.RData') #ind2
    setDT(ind2)  # Convert ind2 to data.table if it's not already
    
    
    # Compute the standard deviation for each group
    sd2 <- ind2[, .(sd = sd(index, na.rm = TRUE)), by = .(spp, year, scn, approach, regime, sim)]
    head(sd2)
    
    #dim(sd)
    dim(sd2)
  
      # Merge 'sd2' with 'samp_df' to add the 'region' column to 'sd2'
    #setDT(sd2)  # Ensure sd2 is a data.table
    setDT(samp_df)  # Ensure samp_df is a data.table
    
    # Perform the merge based on 'scn' column
    sd3 <- merge(sd2, samp_df[, .(samp_scn, region)], by.x = "scn",by.y='samp_scn', all.x = TRUE)
    
    # Replace "SBS" with "BSS" in the 'region' column of sd2
    sd3[, region := gsub("SBS", "BSS", region)]
    
    #set sim integer for merging
    true_ind1$sim<-as.integer(true_ind1$sim)
    sd3$year<-gsub('y','',sd3$year)
    
    # Merge
    true_ind2 <- merge(sd3,true_ind1,  by = c("spp", "year",'region','sim'), all.x = TRUE)
    
    # View the merged data
    dim(true_ind2);dim(sd3)
    
    true_ind2$true_cv<-true_ind2$sd/true_ind2$true_ind
    
    true_ind3<-na.omit(true_ind2)
    true_ind3$true_cv<-true_ind3$true_cv/10
    
    ggplot()+
      geom_boxplot(data=subset(true_ind3,spp %in% sel_spp),
                   aes(x=scn,y=true_cv,color=scn))+
      facet_wrap(~spp)+
      scale_y_continuous(breaks = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,2,3))
    
    save(true_ind3,file = 'output/slope//estimated_cvtrue_dens.RData')
    
  } else {
    # code to run when the file is missing
    message("File present, loading true cv file")
    
    load(file = 'output/slope//estimated_cvtrue_dens.RData')
  
  }  
  
  #unique(true_ind3[, .(scn, region)])
  true_ind31<-true_ind3
  
  setDT(samp_df)
  samp_df$region<-gsub('SBS','BSS',samp_df$region)
  samp_df$strat_var<-gsub('Depth','depth',samp_df$strat_var)
  samp_df_sub <- samp_df[, .(region, samp_scn, strat_var)]  # Subset relevant columns
  
  # Perform the merge (by matching 'scn' with 'samp_scn' and 'region' with 'region')
  true_ind31 <- merge(true_ind31, samp_df_sub, by.x = c('scn','region'), by.y = c('samp_scn','region'), all.x = TRUE)
  true_ind31<-merge(true_ind31,df_spp1,by='spp')
  # Create scn_label
  true_ind31[, scn_label := paste0(region, "\n", strat_var)]
  
  # Create a new combined variable
  true_ind31[, combined_label := factor(
    paste(approach, regime, sep = " - "),
    levels = c('rand - all', 'sb - all', 'rand - cold', 'sb - cold', 'rand - warm', 'sb - warm')
  )]
  
  #filter
  true_ind31 <- true_ind31[!(regime == "cold" & year %in% c(2002:2005,2014:2016))]
  true_ind31 <- true_ind31[!(regime == "warm" & year %in% 2006:2013)]
  
  # Compute mean and standard deviation efficiently
  df_summary <- true_ind31[, .(mean_value = mean(true_cv, na.rm = TRUE),
                               sd_value = sd(true_cv, na.rm = TRUE)),
                           by = .(region, combined_label, scn, approach, spp, 
                                  regime, common, scn_label,strat_var)]
  
  # Replace '+' with '\n' in 'region'
  df_summary[, region1 := gsub("\\+", "\n", region)]
  
  # Rename factor levels for 'strat_var' safely
  df_summary[, strat_var := factor(strat_var, levels = c(levels(strat_var)[1:2], 'depth', 'varSBT'))]

  #filter by spp
  #df_summary<-subset(df_summary,spp %in% sel_spp)
  
  #for geom_blank(0 and adjust scale)
  y_scale<-aggregate((mean_value+sd_value) ~ common, df_summary,max)
  y_scale$scale<-y_scale$`(mean_value + sd_value)`+y_scale$`(mean_value + sd_value)`*0.2
  y_scale$text<-y_scale$`(mean_value + sd_value)`+y_scale$`(mean_value + sd_value)`*0.1
  y_scale$apr<-'sb'
  y_scale$scn<-'scn1'
  y_scale$year<-2022
  y_scale$scn_label<-'EBS\ndepth'
  y_scale$region<-'EBS'
  y_scale$strat_var<-'depth'
  
  # Shared legend items
  labels_shared <- c(
    "random\nstatic",
    "balanced random\nstatic",
    "random\nadaptive cold",
    "balanced random\nadaptive cold",
    "random\nadaptive warm",
    "balanced random\nadaptive warm"
  )
  
  values_colors <- c(
    "rand - all"  = "grey30",
    "sb - all"    = "grey30",
    "rand - cold" = "#1675ac",
    "sb - cold"   = "#1675ac",
    "rand - warm" = "#cc1d1f",
    "sb - warm"   = "#cc1d1f"
  )
  
  values_shapes <- c(
    "rand - all"  = 21,
    "sb - all"    = 24,
    "rand - cold" = 21,
    "sb - cold"   = 24,
    "rand - warm" = 21,
    "sb - warm"   = 24
  )
  
  values_linetype <- c(
    "rand - all"  = "solid",
    "sb - all"    = "dashed",
    "rand - cold" = "solid",
    "sb - cold"   = "dashed",
    "rand - warm" = "solid",
    "sb - warm"   = "dashed"
  )
  
  legend_title <- "sampling allocation and design regime approach"
  
  p<-
  ggplot(na.omit(df_summary)) +
    
    geom_errorbar(
      aes(
        x = interaction(region, strat_var),
        ymin = mean_value - sd_value,
        ymax = mean_value + sd_value,
        color = combined_label,
        group = interaction(scn_label, approach, spp, regime)
      ),
      width = 0.3,
      position = position_dodge(width = 0.9),
      size = 1,
      alpha = 0.8
    ) +
    
    geom_point(
      aes(
        x = interaction(region, strat_var),
        y = mean_value,
        fill = combined_label,
        group = interaction(scn_label, approach, spp, regime),
        shape = combined_label
      ),
      size = 2,
      position = position_dodge(width = 0.9),
      color = "black"
    ) +
    
    labs(y = "true CV", x = "") +
    theme_bw() +
    facet_wrap(~ common, scales = "free_y", nrow = 5, dir = "h") +
    
    scale_fill_manual(
      values = values_colors,
      labels = labels_shared,
      name = legend_title
    ) +
    
    scale_color_manual(
      values = values_colors,
      labels = labels_shared,
      name = legend_title
    ) +
    
    scale_shape_manual(
      values = values_shapes,
      labels = labels_shared,
      name = legend_title
    ) +
    
    scale_linetype_manual(
      values = values_linetype,
      labels = labels_shared,
      name = legend_title
    ) +
    
    scale_y_continuous(
      expand = c(0, 0),
      limits = c(0, NA),
      labels = scales::label_number(accuracy = 0.01)
    ) +
    
    scale_x_discrete(
      labels = function(x) {
        x <- sub("\\..*$", "", x)
        gsub("\\+", "\n", x)
      }
    ) +
    
    theme(
      panel.grid.minor = element_line(linetype = 2, color = "grey90"),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.key.size = unit(30, "points"),
      legend.text = element_text(size = 11),
      legend.spacing = unit(0.3, "cm"),
      legend.title = element_text(size = 12, hjust = 0.5),
      strip.background = element_blank(),
      legend.background = element_blank(),
      panel.spacing.x = unit(0.5, "lines"),
      panel.spacing.y = unit(1, "lines"),
      strip.text = element_blank(),
      axis.title.x = element_blank(),
      axis.text = element_text(size = 10)
    ) +
    
    guides(
      fill = guide_legend(override.aes = list(size = 4), nrow = 1, title.position = "top"),
      color = guide_legend(override.aes = list(size = 4), nrow = 1, title.position = "top"),
      shape = guide_legend(override.aes = list(size = 4), nrow = 1, title.position = "top")
    ) +
    
    
    coord_cartesian(clip = "off") +
    geom_text(
      data = y_scale,
      aes(
        x = Inf,
        y = scale,
        label = common
      ),
      hjust = 1.1,
      vjust = 2,
      size = 4,
      lineheight = 0.8,
      inherit.aes = FALSE
    )+
  
    geom_blank(
      data = y_scale,
      aes(
        x = interaction(region, strat_var),
        y = scale,
        fill = scn_label,
        group = interaction(scn_label, apr)
      )
    )
  
  
  #save plot
  ragg::agg_png(paste0('figures/slope/true_CV_warmcold.png'), width = 18, height = 12, units = "in", res = 300)
  #ragg::agg_png(paste0('./figures/ms_hist_indices_cv_box_EBSNBS_suppl.png'), width = 13, height = 8, units = "in", res = 300)
  p
  dev.off()
  
  df_summary$regime1<-ifelse(df_summary$regime=='all','all','dyn')
  df_summary$combined_label1<-paste0(df_summary$approach," - ",df_summary$regime1)
  
  df_summary_clean <- df_summary %>%
    group_by(region, approach,strat_var, combined_label1, region1, regime1, common) %>%
    summarise(
      mean_value = mean(mean_value, na.rm = TRUE),
      sd_value = mean(sd_value, na.rm = TRUE),  # or another logic
      .groups = "drop"
    )
  
  
  
  df_summary_clean$combined_label1<-factor(df_summary_clean$combined_label1,c("rand - all",'sb - all',"rand - dyn",'sb - dyn'))
  
  labels_shared <- c(
    "random\nstatic",
    "balanced random\nstatic",
    "random\nadaptive",
    "balanced random\nadaptive"
  )
  
  values_colors <- c(
    "rand - all" = "grey30",
    "sb - all"   = "grey30",
    "rand - dyn" = "#bcae19",
    "sb - dyn"   = "#bcae19"
  )
  
  values_shapes <- c(
    "rand - all" = 21,
    "sb - all"   = 24,
    "rand - dyn" = 21,
    "sb - dyn"   = 24
  )
  
  values_linetype <- c(
    "rand - all" = "solid",
    "sb - all"   = "dashed",
    "rand - dyn" = "solid",
    "sb - dyn"   = "dashed"
  )
  
  legend_title <- "sampling allocation and design approach"
  
  
  p1 <- 
  ggplot(na.omit(df_summary_clean)) +
    
    geom_errorbar(
      aes(
        x = interaction(region, strat_var),
        ymin = mean_value - sd_value,
        ymax = mean_value + sd_value,
        color = combined_label1,
        group = interaction(approach, regime1)
      ),
      width = 0.3,
      position = position_dodge(width = 0.9),
      size = 1,
      alpha = 0.8
    ) +
    
    geom_point(
      aes(
        x = interaction(region, strat_var),
        y = mean_value,
        fill = combined_label1,
        shape = combined_label1,
        group = interaction(approach, regime1)
      ),
      size = 2,
      position = position_dodge(width = 0.9),
      color = "black"
    ) +
    
    labs(y = "true CV", x = "") +
    theme_bw() +
    
    facet_wrap(~ common, scales = "free_y", nrow = 5, dir = "h") +
    
    scale_fill_manual(
      values = values_colors,
      labels = labels_shared,
      name = legend_title
    ) +
    
    scale_color_manual(
      values = values_colors,
      labels = labels_shared,
      name = legend_title
    ) +
    
    scale_shape_manual(
      values = values_shapes,
      labels = labels_shared,
      name = legend_title
    ) +
    
    scale_linetype_manual(
      values = values_linetype,
      labels = labels_shared,
      name = legend_title
    ) +
    
    scale_y_continuous(
      expand = c(0, 0),
      limits = c(0, NA),
      labels = scales::label_number(accuracy = 0.01)
    ) +
    
    scale_x_discrete(
      labels = function(x) {
        x <- sub("\\..*$", "", x)
        gsub("\\+", "\n", x)
      }
    ) +
    
    theme(
      panel.grid.minor = element_line(linetype = 2, color = "grey90"),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.key.size = unit(30, "points"),
      legend.text = element_text(size = 11),
      legend.spacing = unit(0.3, "cm"),
      legend.title = element_text(size = 12, hjust = 0.5),
      strip.background = element_blank(),
      legend.background = element_blank(),
      panel.spacing.x = unit(0.5, "lines"),
      panel.spacing.y = unit(1, "lines"),
      strip.text = element_blank(),
      axis.title.x = element_blank(),
      axis.text = element_text(size = 10)
    ) +
    
    guides(
      fill = guide_legend(override.aes = list(size = 4), nrow = 1, title.position = "top"),
      color = guide_legend(override.aes = list(size = 4), nrow = 1, title.position = "top"),
      shape = guide_legend(override.aes = list(size = 4), nrow = 1, title.position = "top")
    ) +
    
    
    coord_cartesian(clip = "off") +
    geom_text(
      data = y_scale,
      aes(
        x = Inf,
        y = scale,
        label = common
      ),
      hjust = 1.1,
      vjust = 2,
      size = 4,
      lineheight = 0.8,
      inherit.aes = FALSE
    ) +
    geom_blank(
      data = y_scale,
      aes(
        x = interaction(region, strat_var),
        y = scale
      )
    )
  
  
  
  ragg::agg_png(paste0('figures/slope/true_CV.png'), width = 18, height = 12, units = "in", res = 300)
  #ragg::agg_png(paste0('./figures/ms_hist_indices_cv_box_EBSNBS_suppl.png'), width = 13, height = 8, units = "in", res = 300)
  p1
  dev.off()
  
  #2 ESTIMATED CV - HISTORICAL CV #####  
  
  ## Define column names
  col_names <- c('spp', 'year', 'approach', 'sur', 'scn', 'regime', 'cv_sim')
  
  est_cv_file<-'output/slope//estimated_cvsim_hist_dens.RData'
  
  if (!file.exists(est_cv_file)) {
    
    # code to run when the file is missing
    message("File not found, running code.")
    
  
    # Get list of files
    files <- list.files('output/slope/ms_sim_survey_hist/', pattern = 'index_hist_dens', recursive = TRUE, full.names = TRUE)
    
    # Process all files using lapply and combine results efficiently
    cv2 <- rbindlist(lapply(seq_along(files), function(sim) {
      
      # Print progress
      cat(paste0('##### sim ', sim, '\n'))
      
      #sim<-1
      
      # Load data
      env <- new.env()
      load(files[sim], envir = env)  # Load into an environment
      index_hist <- env$index_hist   # Extract the expected object
      
      # Ensure index_hist exists
      if (!exists("index_hist", envir = env)) {
        warning(paste("index_hist not found in", files[sim]))
        return(NULL)
      }
      
      # Convert to data.table
      sd <- as.data.frame.table(index_hist[, c('CV_sim'), , , , , ])
      setDT(sd)
      
      # Rename columns
      if (ncol(sd) >= 7) {
        setnames(sd, old = names(sd), new = col_names[1:7])
      } else {
        warning(paste("Unexpected number of columns in", files[sim]))
        return(NULL)
      }
      
      # # Aggregate
      # sd1 <- aggregate(cv_sim ~ spp + year + approach + scn + regime, data = sd, FUN = mean)
      # 
      # # Convert back to data.table before using :=
      # setDT(sd1)
      
      # Add simulation ID
      sd[, sim := sim]
      
      return(sd)
      
    }), fill = TRUE)  # Use fill = TRUE to handle missing columns if needed
    
    #save  
    save(cv2,file = est_cv_file)
  } else {
    # code to run when the file is missing
    message("File present, loading estimated CV file")
    load(file = est_cv_file)
  }
  
  setDT(cv2)
  gc()
  #year
  cv2$year<-gsub('y','',cv2$year)
  
  #year to number
  cv2$year<-as.numeric(as.character(cv2$year))
  true_ind3$year<-as.numeric(true_ind3$year)
  
  setDT(samp_df)
  samp_df$region<-gsub('SBS','BSS',samp_df$region)
  samp_df$strat_var<-gsub('Depth','depth',samp_df$strat_var)
  samp_df_sub <- samp_df[, .(region, samp_scn, strat_var)]  # Subset relevant columns
  
  # Perform the merge (by matching 'scn' with 'samp_scn' and 'region' with 'region')
  cv2 <- merge(cv2, samp_df_sub, by.x = c('scn'), by.y = c('samp_scn'), all.x = TRUE)
  #unique(cv2[, .(scn, region)])
  
  
  # Perform the merge based on the common columns
  cv3 <- merge(cv2, true_ind3, by = c("spp", "year", "scn", "approach", "regime",'sim','region'), all.x = TRUE)
  #unique(cv3[, .(scn, region)])
  
  #save  
  #save(cv3,file = 'output/slope//estimated_cvs_spp_dens.RData')
  #load(file = 'output/slope//estimated_cvs_spp_dens.RData')
  class(cv3)
  
  #df<-df[which(df$spp %in% df_spp1$spp),]
  cv31<-merge(cv3,df_spp1,by='spp')
  
  #df$approach<-factor(df$approach,levels=c('sys','sb','rand'))
  
  # Convert year to numeric
  cv31[, year := as.numeric(year)]
  
  # Create scn_label
  cv31[, scn_label := paste0(region, "\n", strat_var)]
  
  # Rename column and remove EBSNBS crabs and existing bis sampling design
  cv31[, value := cv_sim]
  
  # Create a new combined variable
  cv31[, combined_label := factor(
    paste(approach, regime, sep = " - "),
    levels = c('rand - all', 'sb - all', 'rand - cold', 'sb - cold', 'rand - warm', 'sb - warm')
  )]
  
  #filter
  cv31 <- cv31[!(regime == "cold" & year %in% c(2002:2005,2014:2016))]
  cv31 <- cv31[!(regime == "warm" & year %in% 2006:2013)]
  
  # Compute mean and standard deviation efficiently
  df_summary <- cv31[, .(
    mean_value = mean(value, na.rm = TRUE),
    q10 = quantile(value, probs = 0.10, na.rm = TRUE),
    q90 = quantile(value, probs = 0.90, na.rm = TRUE)
  ), by = .(region, combined_label, scn_label, approach, spp, regime, common, strat_var)]
  
  # Replace '+' with '\n' in 'region'
  df_summary[, region1 := gsub("\\+", "\n", region)]
  
  # Rename factor levels for 'strat_var' safely
  df_summary[, strat_var := factor(strat_var, levels = c(levels(strat_var)[1:2], 'depth', 'varSBT'))]

  #for geom_blank(0 and adjust scale)
  #y_scale<-aggregate(q90 ~ common, subset(df_summary, spp %in% sel_spp),max)
  y_scale<-aggregate(q90 ~ common, df_summary,max)
  y_scale$scale<-y_scale$q90+y_scale$q90*0.1
  y_scale$text<-y_scale$q90+y_scale$q90*0.1
  y_scale$apr<-'sb'
  y_scale$scn<-'scn1'
  y_scale$year<-2022
  y_scale$region<-'EBS'
  y_scale$strat_var<-'depth'
  y_scale$scn_label<-'EBS\ndepth'
  
  
  # Shared legend definitions
  labels_shared <- c(
    'random\nstatic',
    'balanced random\nstatic',
    'random\nadaptive cold',
    'balanced random\nadaptive cold',
    'random\nadaptive warm',
    'balanced random\nadaptive warm'
  )
  
  values_colors <- c(
    'rand - all'  = 'grey30',
    'sb - all'    = 'grey30',
    'rand - cold' = '#1675ac',
    'sb - cold'   = '#1675ac',
    'rand - warm' = "#cc1d1f",
    'sb - warm'   = "#cc1d1f"
  )
  
  values_shapes <- c(
    'rand - all'  = 21,
    'sb - all'    = 24,
    'rand - cold' = 21,
    'sb - cold'   = 24,
    'rand - warm' = 21,
    'sb - warm'   = 24
  )
  
  values_linetype <- c(
    'rand - all'  = 'solid',
    'sb - all'    = 'dashed',
    'rand - cold' = 'solid',
    'sb - cold'   = 'dashed',
    'rand - warm' = 'solid',
    'sb - warm'   = 'dashed'
  )
  
  legend_title <- "sampling allocation and design regime approach"
  
  p <-
    ggplot(subset(df_summary, strat_var == "depth")) +
    
    geom_errorbar(
      aes(
        x = interaction(region, strat_var),
        ymin = q10,
        ymax = q90,
        color = combined_label,
        group = interaction(scn_label, approach, spp, regime)
      ),
      width = 0.3,
      position = position_dodge(width = 0.9),
      size = 1,
      alpha = 0.8
    ) +
    
    geom_point(
      aes(
        x = interaction(region, strat_var),
        y = mean_value,
        fill = combined_label,
        group = interaction(scn_label, approach, spp, regime),
        shape = combined_label
      ),
      size = 2,
      position = position_dodge(width = 0.9),
      color = "black"
    ) +
    
    labs(y = expression(widehat(CV)), x = "") +
    theme_bw() +
    
    facet_wrap(~ common, scales = "free_y", nrow = 5, dir = "h") +
    
    scale_fill_manual(
      values = values_colors,
      labels = labels_shared,
      name = legend_title
    ) +
    
    scale_color_manual(
      values = values_colors,
      labels = labels_shared,
      name = legend_title
    ) +
    
    scale_shape_manual(
      values = values_shapes,
      labels = labels_shared,
      name = legend_title
    ) +
    
    scale_linetype_manual(
      values = values_linetype,
      labels = labels_shared,
      name = legend_title
    ) +
    
    scale_y_continuous(
      expand = c(0, 0),
      limits = c(0, NA),
      labels = scales::label_number(accuracy = 0.01)
    ) +
    
    scale_x_discrete(labels = function(x) {
      x <- sub("\\..*$", "", x)
      gsub("\\+", "\n", x)
    }) +
    
    theme(
      panel.grid.minor = element_line(linetype = 2, color = "grey90"),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.key.size = unit(30, "points"),
      legend.text = element_text(size = 11),
      legend.spacing = unit(0.3, "cm"),
      legend.title = element_text(size = 12, hjust = 0.5),
      strip.background = element_blank(),
      legend.background = element_blank(),
      panel.spacing.x = unit(0.5, "lines"),
      panel.spacing.y = unit(1, "lines"),
      strip.text = element_blank(),
      axis.title.x = element_blank(),
      axis.text = element_text(size = 10)
    ) +
    
    guides(
      fill = guide_legend(override.aes = list(size = 4), nrow = 1, title.position = "top"),
      color = guide_legend(override.aes = list(size = 4), nrow = 1, title.position = "top"),
      shape = guide_legend(override.aes = list(size = 4), nrow = 1, title.position = "top")
    ) +
    
    
    geom_text(
      data = y_scale,
      aes(
        label = common,
        y = text
      ),
      x = Inf,
      hjust = 1.1,
      vjust = 1.7,
      size = 4,
      lineheight = 0.8,
      inherit.aes = FALSE
    ) +
    
    geom_blank(
      data = y_scale,
      aes(
        x = interaction(region, strat_var),
        y = scale,
        fill = scn_label,
        group = interaction(scn_label, apr)
      )
    ) +
    
    coord_cartesian(clip = "off")
  
  #geom_text(data=subset(y_scale,common %in% sel_spp_com),aes(label = common, y = text),x = Inf, vjust = 1.7, hjust = 1.1,size=4, lineheight = 0.8) + #,fontface='italic'
  #geom_blank(data=subset(y_scale,spp %in% sel_spp_com),aes(x=scn_label,y=scale,fill=scn_label,group =interaction(scn_label,apr)))
  
  
  #save plot
  ragg::agg_png(paste0('figures/slope/est_cv_warmcold.png'),  width = 18, height = 12, units = "in", res = 300)
  #ragg::agg_png(paste0('./figures/ms_hist_indices_cv_box_EBSNBS_suppl.png'), width = 13, height = 8, units = "in", res = 300)
  p
  dev.off()
  
  # FIX REGION FACTORS (same fix as previous plot)
  df_summary$region1 <- gsub("\\+", "\n", df_summary$region)
  df_summary$region1 <- factor(df_summary$region1)
  
  df_summary$strat_var <- factor(df_summary$strat_var,
                                 levels = c("depth", "varSBT"))
  
  # REBUILD SUMMARY OBJECTS USING CLEAN REGION
  df_summary$regime1 <- ifelse(df_summary$regime == "all", "all", "dyn")
  df_summary$combined_label1 <- paste0(df_summary$approach,
                                       " - ", df_summary$regime1,
                                       " - ", df_summary$regime)
  
  df_summary<-subset(df_summary,strat_var=='depth')
  
  df_summary_clean <- df_summary %>%
    filter(regime %in% c("cold", "warm") ) %>%
    group_by(region1, region, approach, strat_var, common, regime) %>%
    summarise(
      mean_value = mean(mean_value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_wider(
      names_from = regime,
      values_from = mean_value,
      names_prefix = "mean_"
    ) %>%
    mutate(
      combined_label1 = paste(approach, "- dyn", sep = " ")
    )
  
  df_summary_clean$regime1 <- "dyn"
  
  df_summary_dyn <- df_summary %>%
    group_by(region1, region, approach, strat_var, regime1, common) %>%
    summarise(
      mean_value = mean(mean_value, na.rm = TRUE),
      q10 = mean(q10, na.rm = TRUE),
      q90 = mean(q90, na.rm = TRUE),
      .groups = "drop"
    )
  
  df_summary_dyn <- merge(
    df_summary_dyn,
    df_summary_clean,
    by = c("region", "region1", "approach", "strat_var", "regime1", "common"),
    all.x = TRUE
  )
  
  df_summary_dyn <- df_summary_dyn %>%
    mutate(
      combined_label1 = ifelse(
        is.na(combined_label1),
        paste0(approach, " - ", regime1, " - ", regime1),
        combined_label1
      )
    )
  
  df_summary_dyn$combined_label1 <- factor(
    df_summary_dyn$combined_label1,
    c("rand - all - all", "sb - all - all", "rand - dyn", "sb - dyn")
  )
  
  # FIX x-axis LABELS + SAFELY REMOVE EMPTY LEVELS
  df_summary_dyn$region1 <- droplevels(df_summary_dyn$region1)
  #y_scale
  y_scale_dyn <- df_summary_dyn |>
    group_by(common) |>
    summarise(
      top = max(q90, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      scale = top * 1.20,
      text  = top * 1.10,
      x_dummy = levels(interaction(df_summary_dyn$region1, df_summary_dyn$strat_var))[1]
    )
  
  
  # PLOT WITH FIXED INTERACTION
  p1 <-
    ggplot() +
    
    geom_errorbar(
      data = df_summary_dyn,
      aes(
        x = interaction(region1, strat_var),
        ymin = q10,
        ymax = q90,
        color = combined_label1,
        group = combined_label1
      ),
      width = 0.3,
      position = position_dodge(width = 0.9),
      size = 1,
      alpha = 0.8
    ) +
    
    geom_point(
      data = df_summary_dyn,
      aes(
        x = interaction(region1, strat_var),
        y = mean_value,
        fill = combined_label1,
        group = combined_label1,
        shape = combined_label1
      ),
      size = 2,
      position = position_dodge(width = 0.9),
      color = "black"
    ) +
    
    labs(y = expression(widehat(CV)), x = "") +
    theme_bw() +
    
    facet_wrap(~ common, scales = "free_y", nrow = 5, dir = "h") +
    
    scale_fill_manual(
      values = c(
        "rand - all - all" = "grey30",
        "sb - all - all" = "grey30",
        "rand - dyn" = "#bcae19",
        "sb - dyn" = "#bcae19"
      ),
      labels = c(
        "random\nstatic",
        "balanced random\nstatic",
        "random\nadaptive",
        "balanced random\nadaptive"
      ),
      name = "sampling allocation and design approach"
    ) +
    
    scale_color_manual(
      values = c(
        "rand - all - all" = "grey30",
        "sb - all - all" = "grey30",
        "rand - dyn" = "#bcae19",
        "sb - dyn" = "#bcae19"
      ),
      labels = c(
        "random\nstatic",
        "balanced random\nstatic",
        "random\nadaptive",
        "balanced random\nadaptive"
      ),
      name = "sampling allocation and design approach"
    ) +
    
    scale_shape_manual(
      values = c(
        "rand - all - all" = 21,
        "sb - all - all" = 24,
        "rand - dyn" = 21,
        "sb - dyn" = 24
      ),
      labels = c(
        "random\nstatic",
        "balanced random\nstatic",
        "random\nadaptive",
        "balanced random\nadaptive"
      ),
      name = "sampling allocation and design approach"
    ) +
    
    scale_y_continuous(
      expand = c(0, 0),
      limits = c(0, NA),
      labels = scales::label_number(accuracy = 0.01)
    ) +
    
    scale_x_discrete(labels = function(x) {
      x <- sub("\\..*$", "", x)
      gsub("\\+", "\n", x)
    }) +
    
    theme(
      panel.grid.minor = element_line(linetype = 2, color = "grey90"),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.key.size = unit(30, "points"),
      legend.text = element_text(size = 11),
      legend.spacing = unit(0.3, "cm"),
      legend.title = element_text(size = 12, hjust = 0.5),
      strip.background = element_blank(),
      legend.background = element_blank(),
      panel.spacing.x = unit(0.5, "lines"),
      panel.spacing.y = unit(1, "lines"),
      strip.text = element_blank(),
      axis.title.x = element_blank(),
      axis.text = element_text(size = 10)
    ) +
    
    guides(
      fill = guide_legend(override.aes = list(size = 4), nrow = 1, title.position = "top"),
      color = guide_legend(override.aes = list(size = 4), nrow = 1, title.position = "top"),
      shape = guide_legend(override.aes = list(size = 4), nrow = 1, title.position = "top")
    ) +
    
    geom_text(
      data = y_scale_dyn,
      aes(
        x = Inf,
        y = text,
        label = common
      ),
      hjust = 1.1,
      #vjust = 2,
      size = 4,
      lineheight = 0.8,
      inherit.aes = FALSE
    ) +
    geom_blank(
      data = y_scale_dyn,
      aes(
        x = x_dummy,
        y = scale
      )
    )+
    coord_cartesian(clip = "off")
  
  #save plot
  ragg::agg_png(paste0('figures/slope/est_cv.png'), width = 18, height = 12, units = "in", res = 300)
  #ragg::agg_png(paste0('./figures/ms_hist_indices_cv_box_EBSNBS_suppl.png'), width = 13, height = 8, units = "in", res = 300)
  p1
  dev.off()
  
  
  
#3 CALCULATION RRMSE of INDEX and RBIAS OF INDEX ####
  gc()
  
  #load simulated index
  load('output/slope/estimated_index_hist_dens.RData') #ind2
  
  #load true ind
  load(file = paste0("output/slope/model_based_EBSNBSBSS.RData"))  #true_ind
  
  #arrange true index data
  true_ind1<-reshape2::melt(true_ind,id.vars='year')
  true_ind1 <- melt(true_ind, varnames = c("year", "region", "spp"), value.name = "value")
  
  true_ind1<-subset(true_ind1,region!='year')
  
  true_ind1<-true_ind1[which(true_ind1$spp %in% df_spp1$spp),]
  true_ind1<-merge(true_ind1,df_spp1,by='spp')
  true_ind1$year<-as.integer(true_ind1$year)
  true_ind1$dummy<-'true index'
  #ind2
  #true_ind1
  
  head(ind2)
  head(samp_df)
  head(true_ind1)
  
  
  # Convert to data.table if needed
  setDT(ind2)
  setDT(samp_df)
  setDT(true_ind1)
  
  samp_df1<-samp_df[,c('type','region','strat_var','samp_scn')]
  names(samp_df1)[4]<-'scn'
  
  
  # Join on approach (ind2) == type (samp_df)
  ind2 <- merge(ind2, samp_df1, by= "scn", all.x = TRUE)
  
  
  # Make sure key columns are the same type
  ind2[, year := as.integer(year)]
  # Filter ind2
  ind2 <- ind2[scn %in% unique(samp_df1$scn)]
  ind2[region == "EBS+SBS", region := "EBS+BSS"]
  ind2[region == "EBS+NBS+SBS", region := "EBS+NBS+BSS"]
  
  true_ind1[, year := as.integer(year)]
  true_ind1[region == "EBS+SBS", region := "EBS+BSS"]
  true_ind1[region == "EBS+NBS+SBS", region := "EBS+NBS+BSS"]
  
  head(ind2)
  head(true_ind1)
  names(true_ind1)[4]<-'true_index'
  ind2[is.na(region)] # na for scn that are not considered here because they are forcing as dummy the slope
  
  
  # Perform the merge
  ind3 <- merge(
    ind2, 
    true_ind1[, .(spp, year, region, true_index)], 
    by = c("spp", "year", "region"), 
    all.x = TRUE
  )
  
  est_error_file<-'output/slope//rrmse_rbias_index.RData'
  
  if (!file.exists(est_error_file)) {
    
    # code to run when the file is missing
    message("File not found, running code.")
    
  
    # Create an empty list to store results
    rb_list <- list()
    
    for (s in 1:100) {
      
      #s<-1
      cat(paste0('#### sim ', s,' ###\n'))
      # Subset for one simulation
      ind4 <- ind3[sim == s]
      
      # Apply regime-based filtering (keep 'all' regime untouched)
      ind4 <- na.omit(ind4[
        regime == "all" |
          (regime == "warm" & year %in% c(2002:2005, 2014:2016)) |
          (regime == "cold" & year %in% c(2006:2013))
      ]) #to remove static warm and cold (because of prior errors)
      
      #check
      unique(ind4$region)
      
      # Compute relative bias
      ind4[, rel_bias := 100*((index - true_index) / true_index)]
      ind4[, rel_log_bias := log10(index / true_index) ]
   
      # Compute mean relative bias and RRMSE by spp, year, and scenario
      rb <- ind4[, .(
        rel_bias = mean(rel_bias, na.rm = TRUE),
        rel_log_bias = mean(rel_bias, na.rm = TRUE),
        rrmse = sqrt(mean((index - true_index)^2, na.rm = TRUE)) / mean(true_index, na.rm = TRUE)
      ), by = .(spp, year, scn, approach, regime)]
      
      # Add simulation identifier
      rb[, sim := s]
      
      # Store in list
      rb_list[[s]] <- rb
    }
    
    # Combine results
    rel_bias_rrmse_dt <- rbindlist(rb_list)
  
    save(rel_bias_rrmse_dt,file = est_error_file)
    
  } else {
    
    # code to run when the file is missing
    message("File not found, loading rrmse and rbias of index file")
    
    load(est_error_file)
  }
  
  # Preview results
  head(rel_bias_rrmse_dt)
  
  # Assuming `rel_bias_rrmse_dt` is your summary table (already computed)
  # Step 1: Prepare the plotting data
  
  # Convert to data.table if not already
  setDT(rel_bias_rrmse_dt)
  
  # --- keep only scn1 … scn16 ---
  rel_bias_rrmse_dt <- rel_bias_rrmse_dt[
    scn %in% paste0("scn", 1:16)
  ]
  
  # Keep only necessary simulations (e.g., sim 1) or average across sims if desired
  df_rb_rrmse <- rel_bias_rrmse_dt[, .(
    mean_rel_bias = mean(rel_bias, na.rm = TRUE),
    sd_rel_bias   = sd(rel_bias, na.rm = TRUE),
    mean_rrmse    = mean(rrmse, na.rm = TRUE),
    sd_rrmse      = sd(rrmse, na.rm = TRUE)
  ), by = .(spp, scn, approach)]
  
  # Step 2: Join with scenario-level metadata if needed
  # Assuming samp_df contains mapping of scn to region, strat_var, approach, regime1, etc.
  df_rb_rrmse1<-merge(df_rb_rrmse,samp_df1,by='scn')
  
  # Replace '+' with '\n' in 'region'
  df_rb_rrmse1[, region1 := gsub("\\+", "\n", region)]
  
  # Rename factor levels for 'strat_var' safely
  df_rb_rrmse1[, strat_var := factor(strat_var, levels = c(levels(strat_var)[1:2], 'depth', 'varSBT'))]
  
  df_rb_rrmse1$regime<-ifelse(df_rb_rrmse1$type=='static','all','dyn')
  df_rb_rrmse1$regime1<-ifelse(df_rb_rrmse1$regime=='all','all','dyn')
  df_rb_rrmse1$combined_label1<-paste0(df_rb_rrmse1$approach," - ",df_rb_rrmse1$regime1)
  
  df_rb_rrmse1$combined_label1<-factor(df_rb_rrmse1$combined_label1,c("rand - all",'sb - all',"rand - dyn",'sb - dyn'))
  
  df_rb_rrmse1<-merge(df_rb_rrmse1,df_spp,by='spp')
  df_rb_rrmse1<-subset(df_rb_rrmse1,strat_var=='Depth')
  #3.1 PLOT RRMSE OF INDEX  #####
  
  # for geom_blank and adjust scale
  # y_scale <- aggregate((mean_rrmse + sd_rrmse) ~ common, subset(df_rb_rrmse1, spp %in% sel_spp), max)
  y_scale <- aggregate((mean_rrmse + sd_rrmse) ~ common, df_rb_rrmse1, max)
  y_scale$scale <- y_scale$`(mean_rrmse + sd_rrmse)` * 1.1
  y_scale$text  <- y_scale$`(mean_rrmse + sd_rrmse)` * 1.1
  
  y_scale$apr       <- "sb"
  y_scale$scn       <- "scn1"
  y_scale$year      <- 2022
  y_scale$region    <- "EBS"
  y_scale$strat_var <- "Depth"
  
  # substitute SBS → BSS
  df_rb_rrmse1$region <- gsub("SBS", "BSS", df_rb_rrmse1$region)
  #df_rb_rrmse1 <- droplevels(df_rb_rrmse1)

  p <-
    ggplot() +
    
    geom_errorbar(
      data = df_rb_rrmse1,
      aes(
        x = interaction(region, strat_var),
        ymin = mean_rrmse - sd_rrmse,
        ymax = mean_rrmse + sd_rrmse,
        color = combined_label1,
        group = interaction(approach, regime1)
      ),
      width = 0.3,
      position = position_dodge(width = 0.9),
      size = 1,
      alpha = 0.8
    ) +
    
    geom_point(
      data = df_rb_rrmse1,
      aes(
        x = interaction(region, strat_var),
        y = mean_rrmse,
        fill = combined_label1,
        group = interaction(approach, regime1),
        shape = combined_label1
      ),
      size = 2,
      position = position_dodge(width = 0.9),
      color = "black"
    ) +
    
    labs(y = "RRMSE of abundance estimates", x = "") +
    theme_bw() +
    
    facet_wrap(~ common, scales = "free_y", nrow = 5, dir = "h") +
    
    scale_fill_manual(
      values = c(
        "rand - all" = "grey30",
        "sb - all"   = "grey30",
        "rand - dyn" = "#bcae19",
        "sb - dyn"   = "#bcae19"
      ),
      label = c(
        "random\nstatic",
        "balanced random\nstatic",
        "random\nadaptive",
        "balanced random\nadaptive"
      ),
      name = "sampling allocation and design approach"
    ) +
    
    scale_color_manual(
      values = c(
        "rand - all" = "grey30",
        "sb - all"   = "grey30",
        "rand - dyn" = "#bcae19",
        "sb - dyn"   = "#bcae19"
      ),
      label = c(
        "random\nstatic",
        "balanced random\nstatic",
        "random\nadaptive",
        "balanced random\nadaptive"
      ),
      name = "sampling allocation and design approach"
    ) +
    
    scale_shape_manual(
      values = c(
        "rand - all" = 21,
        "sb - all"   = 24,
        "rand - dyn" = 21,
        "sb - dyn"   = 24
      ),
      label = c(
        "random\nstatic",
        "balanced random\nstatic",
        "random\nadaptive",
        "balanced random\nadaptive"
      ),
      name = "sampling allocation and design approach"
    ) +
    
    scale_y_continuous(labels = scales::label_number(accuracy = 0.01)) +
    
    scale_x_discrete(labels = function(x) {
      x <- sub("\\..*$", "", x)
      gsub("\\+", "\n", x)
    }) +
    theme(
      panel.grid.minor = element_line(linetype = 2, color = "grey90"),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.key.size = unit(30, "points"),
      legend.text = element_text(size = 11),
      legend.spacing = unit(0.3, "cm"),
      legend.title = element_text(size = 12, hjust = 0.5),
      strip.background = element_blank(),
      legend.background = element_blank(),
      panel.spacing.x = unit(0.5, "lines"),
      panel.spacing.y = unit(1, "lines"),
      strip.text = element_blank(),
      axis.title.x = element_blank(),
      axis.text = element_text(size = 10)
    ) +
    
    guides(
      fill = guide_legend(override.aes = list(size = 4), nrow = 1, title.position = "top"),
      color = guide_legend(override.aes = list(size = 4), nrow = 1, title.position = "top"),
      shape = guide_legend(override.aes = list(size = 4), nrow = 1, title.position = "top")
    ) +
    
    geom_text(
      data = y_scale,
      aes(label = common, y = text),
      x = Inf,
      vjust = 1,
      hjust = 1.1,
      size = 4,
      lineheight = 0.8
    ) +
    
    geom_blank(
      data = y_scale,
      aes(
        x = interaction(region, strat_var),
        y = scale,
        fill = scn,
        group = interaction(scn, apr)
      )
    )+
    coord_cartesian(clip = "off")
  
  ragg::agg_png(paste0('figures/slope/RRMSE_index.png'), width = 18, height = 12, units = "in", res = 300)
  #ragg::agg_png(paste0('./figures/ms_hist_indices_cv_box_EBSNBS_suppl.png'), width = 13, height = 8, units = "in", res = 300)
  p
  dev.off()
  
  
  # Keep only necessary simulations (e.g., sim 1) or average across sims if desired
  df_rb_rrmse2 <- rel_bias_rrmse_dt[, .(
    mean_rel_bias = mean(rel_bias, na.rm = TRUE),
    sd_rel_bias   = sd(rel_bias, na.rm = TRUE),
    mean_rrmse    = mean(rrmse, na.rm = TRUE),
    sd_rrmse      = sd(rrmse, na.rm = TRUE)
  ), by = .(spp, scn, approach,regime)]
  
  # Step 2: Join with scenario-level metadata if needed
  # Assuming samp_df contains mapping of scn to region, strat_var, approach, regime1, etc.
  df_rb_rrmse2<-merge(df_rb_rrmse2,samp_df1,by='scn')
  # Replace '+' with '\n' in 'region'
  df_rb_rrmse2[, region1 := gsub("\\+", "\n", region)]
  # Rename factor levels for 'strat_var' safely
  df_rb_rrmse2[, strat_var := factor(strat_var, levels = c(levels(strat_var)[1:2], 'depth', 'varSBT'))]
  #filter by spp
  #df_rb_rrmse2<-subset(df_rb_rrmse2,spp %in% sel_spp)
  
  
  #df_rb_rrmse2$regime<-ifelse(df_rb_rrmse2$type=='static','all','dyn')
  df_rb_rrmse2$regime1<-ifelse(df_rb_rrmse2$regime=='all','all','dyn')
  df_rb_rrmse2$combined_label<-paste0(df_rb_rrmse2$approach," - ",df_rb_rrmse2$regime)
  
  df_rb_rrmse2$combined_label1<-paste0(df_rb_rrmse2$approach," - ",df_rb_rrmse2$regime1)
  df_rb_rrmse2$combined_label1<-factor(df_rb_rrmse2$combined_label1,c("rand - all",'sb - all',"rand - dyn",'sb - dyn'))
  df_rb_rrmse2<-merge(df_rb_rrmse2,df_spp,by='spp')
  
  #sub SBS for BSS
  df_rb_rrmse2$region<-gsub('SBS','BSS',df_rb_rrmse2$region)
  df_rb_rrmse2<-subset(df_rb_rrmse2,strat_var=='depth')
  y_scale[4,2:4]<-0.45
  
  
  # fix factor levels once
  df_rb_rrmse2$combined_label <- factor(
    df_rb_rrmse2$combined_label,
    levels = c("rand - all",
               "sb - all",
               "rand - cold",
               "sb - cold",
               "rand - warm",
               "sb - warm")
  )
  
  # define labels vector once
  labels_vec <- c("random\nstatic",
                  "balanced random\nstatic",
                  "random\nadaptive cold",
                  "balanced random\nadaptive cold",
                  "random\nadaptive warm",
                  "balanced random\nadaptive warm")
  
  y_scale <- aggregate((mean_rrmse + sd_rrmse) ~ common, df_rb_rrmse2, max)
  y_scale$scale <- y_scale$`(mean_rrmse + sd_rrmse)` * 1.1
  y_scale$text  <- y_scale$`(mean_rrmse + sd_rrmse)` * 1.1
  
  y_scale$apr       <- "sb"
  y_scale$scn       <- "scn1"
  y_scale$year      <- 2022
  y_scale$region    <- "EBS"
  y_scale$strat_var <- "depth"
  
  p <- 
    ggplot(na.omit(df_rb_rrmse2)) +
    
    geom_errorbar(
      aes(x = interaction(region, strat_var),
          ymin = mean_rrmse - sd_rrmse,
          ymax = mean_rrmse + sd_rrmse,
          color = combined_label,
          group = interaction(scn, approach, spp, regime)),
      width = 0.3,
      position = position_dodge(width = 0.9),
      size = 1,
      alpha = 0.8
    ) +
    
    geom_point(
      aes(x = interaction(region, strat_var),
          y = mean_rrmse,
          fill = combined_label,
          group = interaction(scn, approach, spp, regime),
          shape = combined_label),
      size = 2,
      position = position_dodge(width = 0.9),
      color = "black"
    ) +
    
    labs(y = "RRMSE of abundance estimates", x = "") +
    theme_bw() +
    
    facet_wrap(~ common, scales = "free_y", nrow = 5, dir = "h") +
    
    scale_y_continuous(labels = scales::label_number(accuracy = 0.01)) +
    
    scale_fill_manual(
      values = c(
        "rand - all"  = "grey30",
        "sb - all"    = "grey30",
        "rand - cold" = "#1675ac",
        "sb - cold"   = "#1675ac",
        "rand - warm" = "#cc1d1f",
        "sb - warm"   = "#cc1d1f"
      ),
      labels = labels_vec,
      name = "sampling allocation and design regime approach"
    ) +
    
    scale_color_manual(
      values = c(
        "rand - all"  = "grey30",
        "sb - all"    = "grey30",
        "rand - cold" = "#1675ac",
        "sb - cold"   = "#1675ac",
        "rand - warm" = "#cc1d1f",
        "sb - warm"   = "#cc1d1f"
      ),
      labels = labels_vec,
      name = "sampling allocation and design regime approach"
    ) +
    
    scale_shape_manual(
      values = c(
        "rand - all"  = 21,
        "sb - all"    = 24,
        "rand - cold" = 21,
        "sb - cold"   = 24,
        "rand - warm" = 21,
        "sb - warm"   = 24
      ),
      labels = labels_vec,
      name = "sampling allocation and design regime approach"
    ) +
    
    theme(
      panel.grid.minor = element_line(linetype = 2, color = "grey90"),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.key.size = unit(30, "points"),
      legend.text = element_text(size = 11),
      legend.spacing = unit(0.3, "cm"),
      legend.title = element_text(size = 12, hjust = 0.5),
      strip.background = element_blank(),
      legend.background = element_blank(),
      panel.spacing.x = unit(0.5, "lines"),
      panel.spacing.y = unit(1, "lines"),
      strip.text = element_blank(),
      axis.title.x = element_blank(),
      axis.text = element_text(size = 10)
    ) +
    
    guides(
      fill = guide_legend(override.aes = list(size = 4), nrow = 1, title.position = "top"),
      color = guide_legend(override.aes = list(size = 4), nrow = 1, title.position = "top"),
      shape = guide_legend(override.aes = list(size = 4), nrow = 1, title.position = "top")
    ) +
    
    
    
    scale_x_discrete(labels = function(x) {
      x <- sub("\\..*$", "", x)
      x <- gsub("\\+", "\n", x)
      x
    }) +
    
    geom_text(
      data = y_scale,
      aes(label = common, y = text),
      x = Inf,
      vjust = 1,
      hjust = 1.1,
      size = 4,
      lineheight = 0.8
    ) +
    
    # FIXED geom_blank. Only x and y aesthetics.
    geom_blank(
      data = y_scale,
      aes(
        x = interaction(region, strat_var),
        y = scale
      )
    )+
    coord_cartesian(clip = "off")
    
  
  ragg::agg_png(paste0('figures/slope/RRMSE_index_warmcold.png'), width = 18, height = 12, units = "in", res = 300)
  #ragg::agg_png(paste0('./figures/ms_hist_indices_cv_box_EBSNBS_suppl.png'), width = 13, height = 8, units = "in", res = 300)
  p
  dev.off()
  
  #3.2 PLOT RBIAS OF INDEX  #####
  
    #for geom_blank(0 and adjust scale)
  # y-scale and artificial rows for geom_blank and facet labels
  y_scale <- aggregate((mean_rel_bias + sd_rel_bias) ~ common, df_rb_rrmse1, max)
  y_scale$scale <- y_scale$`(mean_rel_bias + sd_rel_bias)` * 1.10
  y_scale$text  <- y_scale$`(mean_rel_bias + sd_rel_bias)` * 1.10
  
  # artificial columns so geom_blank has valid x aesthetics
  y_scale$apr       <- "sb"
  y_scale$scn       <- "scn1"
  y_scale$year      <- 2022
  y_scale$region    <- "EBS"
  y_scale$strat_var <- "depth"
  
  # substitute SBS → BSS (matching your RRMSE version)
  df_rb_rrmse1$region <- gsub("SBS", "BSS", df_rb_rrmse1$region)
  
  
  p <-
    ggplot() +
    
    geom_hline(yintercept = 0, alpha = 0.5, linetype = "dotted") +
    
    geom_errorbar(
      data = df_rb_rrmse1,
      aes(
        x = interaction(region, strat_var),
        ymin = mean_rel_bias - sd_rel_bias,
        ymax = mean_rel_bias + sd_rel_bias,
        color = combined_label1,
        group = interaction(approach, regime1)
      ),
      width = 0.3,
      position = position_dodge(width = 0.9),
      size = 1,
      alpha = 0.8
    ) +
    
    geom_point(
      data = df_rb_rrmse1,
      aes(
        x = interaction(region, strat_var),
        y = mean_rel_bias,
        fill = combined_label1,
        group = interaction(approach, regime1),
        shape = combined_label1
      ),
      size = 2,
      position = position_dodge(width = 0.9),
      color = "black"
    ) +
    
    labs(y = "RBIAS of abundance estimates (%)", x = "") +
    theme_bw() +
    
    facet_wrap(~ common, scales = "free_y", nrow = 5, dir = "h") +
    
    scale_fill_manual(
      values = c(
        "rand - all" = "grey30",
        "sb - all"   = "grey30",
        "rand - dyn" = "#bcae19",
        "sb - dyn"   = "#bcae19"
      ),
      label = c(
        "random\nstatic",
        "balanced random\nstatic",
        "random\nadaptive",
        "balanced random\nadaptive"
      ),
      name = "sampling allocation and design approach"
    ) +
    
    scale_color_manual(
      values = c(
        "rand - all" = "grey30",
        "sb - all"   = "grey30",
        "rand - dyn" = "#bcae19",
        "sb - dyn"   = "#bcae19"
      ),
      label = c(
        "random\nstatic",
        "balanced random\nstatic",
        "random\nadaptive",
        "balanced random\nadaptive"
      ),
      name = "sampling allocation and design approach"
    ) +
    
    scale_shape_manual(
      values = c(
        "rand - all" = 21,
        "sb - all"   = 24,
        "rand - dyn" = 21,
        "sb - dyn"   = 24
      ),
      label = c(
        "random\nstatic",
        "balanced random\nstatic",
        "random\nadaptive",
        "balanced random\nadaptive"
      ),
      name = "sampling allocation and design approach"
    ) +
    
    scale_y_continuous(labels = scales::label_number(accuracy = 0.01)) +
    
    scale_x_discrete(labels = function(x) {
      x <- sub("\\..*$", "", x)
      gsub("\\+", "\n", x)
    }) +
    
    theme(
      panel.grid.minor = element_line(linetype = 2, color = "grey90"),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.key.size = unit(30, "points"),
      legend.text = element_text(size = 11),
      legend.spacing = unit(0.3, "cm"),
      legend.title = element_text(size = 12, hjust = 0.5),
      strip.background = element_blank(),
      legend.background = element_blank(),
      panel.spacing.x = unit(0.5, "lines"),
      panel.spacing.y = unit(1, "lines"),
      strip.text = element_blank(),
      axis.title.x = element_blank(),
      axis.text = element_text(size = 10)
    ) +
    
    guides(
      fill = guide_legend(override.aes = list(size = 4), nrow = 1, title.position = "top"),
      color = guide_legend(override.aes = list(size = 4), nrow = 1, title.position = "top"),
      shape = guide_legend(override.aes = list(size = 4), nrow = 1, title.position = "top")
    ) +
    
    
    # facet titles placed far right (same as RRMSE version)
    geom_text(
      data = y_scale,
      aes(label = common, y = text),
      x = Inf,
      vjust = 1,
      hjust = 1.1,
      size = 4,
      lineheight = 0.8
    ) +
    
    geom_blank(
      data = y_scale,
      aes(
        x = interaction(region, strat_var),
        y = scale,
        fill = scn,
        group = interaction(scn, apr)
      )
    )+
    coord_cartesian(clip = "off")
  
  # save
  ragg::agg_png(
    'figures/slope/RBIAS_index.png',
    width = 18, height = 12, units = "in", res = 300
  )
  p
  dev.off()
  
    
    #y_scale[4,2:4]<-6
  y_scale <- aggregate((mean_rel_bias + sd_rel_bias) ~ common, df_rb_rrmse2, max)
  y_scale$scale <- y_scale$`(mean_rel_bias + sd_rel_bias)` * 1.10
  y_scale$text  <- y_scale$`(mean_rel_bias + sd_rel_bias)` * 1.10
  
  # artificial columns so geom_blank has valid x aesthetics
  y_scale$apr       <- "sb"
  y_scale$scn       <- "scn1"
  y_scale$year      <- 2022
  y_scale$region    <- "EBS"
  y_scale$strat_var <- "depth"
  
    p<-
      ggplot(na.omit(df_rb_rrmse2)) +
      geom_hline(yintercept = 0, alpha = 0.5, linetype = 'dotted') +
      geom_errorbar(aes(x = interaction(region,strat_var), ymin = mean_rel_bias - sd_rel_bias, ymax = mean_rel_bias + sd_rel_bias, color = combined_label,
                        group = interaction(scn, approach, spp, regime)),
                    width = 0.3, position = position_dodge(width = 0.9),size=1,alpha=0.8) + 
      geom_point(aes(x = interaction(region,strat_var), y = mean_rel_bias, fill = combined_label, 
                     group = interaction(scn, approach, spp, regime), 
                     shape = combined_label), 
                 size = 2, position = position_dodge(width = 0.9), color = "black") + 
      labs(y = 'RBIAS of abundance estimates (%)', x = '') +
      theme_bw() + 
      
      facet_wrap(~common, scales = 'free_y', nrow = 5, dir = 'h') +
      
      
      scale_y_continuous(labels = scales::label_number(accuracy = 0.01)) +
      
      scale_fill_manual(values = c(
        'rand - all' = 'grey30',
        'sb - all' = 'grey30',
        'rand - cold' = '#1675ac',
        'sb - cold' = '#1675ac',
        'rand - warm' = "#cc1d1f",
        'sb - warm' = "#cc1d1f"), 
        label = c('random\nstatic',
                  'balanced random\nstatic',
                  'random\nadaptive cold',
                  'balanced random\nadaptive cold',
                  'random\nadaptive warm',
                  'balanced random\nadaptive warm'),
        name = "sampling allocation and design regime approach") +
      scale_color_manual(values = c(
        'rand - all' = 'grey30',
        'sb - all' = 'grey30',
        'rand - cold' = '#1675ac',
        'sb - cold' = '#1675ac',
        'rand - warm' = "#cc1d1f",
        'sb - warm' = "#cc1d1f"),
        label = c('random\nstatic',
                  'balanced random\nstatic',
                  'random\nadaptive cold',
                  'balanced random\nadaptive cold',
                  'random\nadaptive warm',
                  'balanced random\nadaptive warm'),
        name = "sampling allocation and design regime approach") +
      
      scale_linetype_manual(values = c('rand - all' = 'solid',
                                       'sb - all' = 'dashed',
                                       'rand - cold' = 'solid',
                                       'sb - cold' = 'dashed',
                                       'rand - warm' = 'solid',
                                       'sb - warm' = 'dashed'),
                            label = c('random\nstatic',
                                      'balanced random\nstatic',
                                      'random\nadaptive cold',
                                      'balanced random\nadaptive cold',
                                      'random\nadaptive warm',
                                      'balanced random\nadaptive warm'),
                            name = "sampling allocation and design regime approach") +
      
      scale_shape_manual(values = c('rand - all' = 21,
                                    'sb - all' = 24,
                                    'rand - cold' = 21,
                                    'sb - cold' = 24,
                                    'rand - warm' = 21,
                                    'sb - warm' = 24),
                         label = c('random\nstatic',
                                   'balanced random\nstatic',
                                   'random\nadaptive cold',
                                   'balanced random\nadaptive cold',
                                   'random\nadaptive warm',
                                   'balanced random\nadaptive warm'),
                         name = "sampling allocation and design regime approach") +
      
      #scale_y_continuous(expand = c(0, 0), limits = c(0, NA),labels = scales::label_number(accuracy = 0.01))+
      #scale_x_discrete(guide = guide_axis_nested(angle=0),labels = function(x) gsub("\\+", "\n", x))+
      theme(
        panel.grid.minor = element_line(linetype = 2, color = "grey90"),
        legend.position = "bottom",
        legend.key.width = unit(1, "lines"),
        legend.key.size = unit(30, "points"),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 12,hjust = 0.5),
        legend.spacing.y = unit(0.3, "cm"),
        legend.spacing = unit(0.3, "cm"),
        legend.box.spacing = unit(0.3, "cm"),
        strip.background = element_blank(),
        legend.background = element_blank(),
        panel.spacing.x = unit(0.5, "lines"),
        panel.spacing.y = unit(1, "lines"),
        strip.text = element_blank(),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 10)
      ) +
      
      
      guides(
        fill = guide_legend(
          override.aes = list(size = 4),
          nrow = 1,
          title.position = "top"
        ),
        color = guide_legend(
          override.aes = list(size = 4),
          nrow = 1,
          title.position = "top"
        ),
        shape = guide_legend(
          override.aes = list(size = 4),
          nrow = 1,
          title.position = "top"
        ),
      )+
      #scale_x_discrete(
      #           labels = function(x) gsub("\\+", "\n", x)
      #)  +
      scale_x_discrete(labels = function(x) {
        # Keep everything before the first dot
        x <- sub("\\..*$", "", x)
        # Replace + with line break
        x <- gsub("\\+", "\n", x)
        return(x)
      })+
      # facet titles placed far right (same as RRMSE version)
      geom_text(
        data = y_scale,
        aes(label = common, y = text),
        x = Inf,
        vjust = 1,
        hjust = 1.1,
        size = 4,
        lineheight = 0.8
      ) +
      
      geom_blank(
        data = y_scale,
        aes(
          x = interaction(region, strat_var),
          y = scale,
          fill = scn,
          group = interaction(scn, apr)
        )
      )+
      coord_cartesian(clip = "off")
    
    
    ragg::agg_png(paste0('figures/slope/RBIAS_index_warmcold.png'), width = 18, height = 12, units = "in", res = 300)
    #ragg::agg_png(paste0('./figures/ms_hist_indices_cv_box_EBSNBS_suppl.png'), width = 13, height = 8, units = "in", res = 300)
    p
    dev.off()
    

#4 CALCULATION RRMSE AND RBIAS OF CV  #####
#LOAD

load(file = 'output/slope//estimated_cvs_spp_dens.RData')#cv3
head(cv3)
# Step 1: Filter based on regime + year
cv3_filtered <- cv3[
  !(regime == "static") & (
    regime == "all" |
      (regime == "warm" & year %in% c(2002:2005, 2014:2016)) |
      (regime == "cold" & year %in% c(2006:2013))
  )
]

# Step 2: Calculate relative bias and squared error
cv3_filtered[, rel_bias := 100*((cv_sim - true_cv) / true_cv)]
cv3_filtered[, sqrtdiff := (cv_sim - true_cv)^2]

# Step 3: Aggregate to get mean RB and RRMSE per group
cv_summary <- cv3_filtered[, .(
  mean_rel_bias = mean(rel_bias, na.rm = TRUE),
  mean_sqrtdiff = mean(sqrtdiff, na.rm = TRUE),
  mean_cv_sim = mean(cv_sim, na.rm = TRUE),
  mean_cv_true = mean(true_cv, na.rm = TRUE)
), by = .(spp, year, scn, approach, regime, sim, region)]

# Step 4: Calculate RRMSE
cv_summary[, rrmse := sqrt(mean_sqrtdiff) / mean_cv_true]

# Optional: remove NAs
cv5 <- na.omit(cv_summary)

df1<-as.data.frame(cv5)
# Create a new combined variable in your data
df1$combined_label <- paste(df1$approach, df1$regime, sep = " - ")
# #sort factors just in case
df1$combined_label<-factor(df1$combined_label,levels=c('rand - all' ,
                                                       'sb - all' ,
                                                       'rand - cold' ,
                                                       'sb - cold' ,
                                                       'rand - warm' ,
                                                       'sb - warm' ) )

  df_sub<-df1
 setDT(df_sub)
 setDT(samp_df)
 samp_df$region<-gsub('SBS','BSS',samp_df$region)
 samp_df$strat_var<-gsub('Depth','depth',samp_df$strat_var)
 samp_df_sub <- samp_df[, .(region, samp_scn, strat_var)]  # Subset relevant columns

# Perform the merge (by matching 'scn' with 'samp_scn' and 'region' with 'region')
df_sub <- merge(df_sub, samp_df_sub, by.x = c('scn','region'), by.y = c('samp_scn','region'), all.x = TRUE)

#filter
df_sub <- df_sub[!(regime == "cold" & year %in% c(2002:2005,2014:2016))]
df_sub <- df_sub[!(regime == "warm" & year %in% 2006:2013)]
# Keep only necessary simulations (e.g., sim 1) or average across sims if desired
df_rb_rrmse <- df_sub[, .(
  mean_rel_bias = mean(mean_rel_bias, na.rm = TRUE),
  sd_rel_bias   = sd(mean_rel_bias, na.rm = TRUE),
  mean_rrmse    = mean(rrmse, na.rm = TRUE),
  sd_rrmse      = sd(rrmse, na.rm = TRUE)
), by = .(spp, scn, approach)]

# Step 2: Join with scenario-level metadata if needed
# Assuming samp_df contains mapping of scn to region, strat_var, approach, regime1, etc.
df_rb_rrmse1<-merge(df_rb_rrmse,samp_df1,by='scn')

# Replace '+' with '\n' in 'region'
df_rb_rrmse1[, region1 := gsub("\\+", "\n", region)]

# Rename factor levels for 'strat_var' safely
df_rb_rrmse1[, strat_var := factor(strat_var, levels = c(levels(strat_var)[1:2], 'depth', 'varSBT'))]

#filter by spp
#df_rb_rrmse1<-subset(df_rb_rrmse1,spp %in% sel_spp)

# library(ggh4x
#         )

df_rb_rrmse1$regime<-ifelse(df_rb_rrmse1$type=='static','all','dyn')
df_rb_rrmse1$regime1<-ifelse(df_rb_rrmse1$regime=='all','all','dyn')
df_rb_rrmse1$combined_label1<-paste0(df_rb_rrmse1$approach," - ",df_rb_rrmse1$regime1)

df_rb_rrmse1$combined_label1<-factor(df_rb_rrmse1$combined_label1,c("rand - all",'sb - all',"rand - dyn",'sb - dyn'))

df_rb_rrmse1<-merge(df_rb_rrmse1,df_spp,by='spp')

#sub SBS for BSS
df_rb_rrmse1$region<-gsub('SBS','BSS',df_rb_rrmse1$region)

#4.1 PLOT RRMSE OF CV ####

#for geom_blank(0 and adjust scale)
#y_scale<-aggregate((mean_rrmse+sd_rrmse) ~ common, subset(df_rb_rrmse1, spp %in% sel_spp),max)
y_scale <- df_rb_rrmse1 |>
  group_by(common) |>
  summarise(
    top = max(mean_rrmse + sd_rrmse, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    scale = top * 1.10,
    text  = top * 1.05,
    x_dummy = levels(interaction(df_rb_rrmse1$region, df_rb_rrmse1$strat_var))[1]
  )


#
p<-
  ggplot() +
  #geom_hline(yintercept = 0, alpha = 0.5, linetype = 'dotted') +
  
  geom_errorbar(data = df_rb_rrmse1, 
                aes(x = interaction(region, strat_var), 
                    ymin = mean_rrmse - sd_rrmse, 
                    ymax = mean_rrmse + sd_rrmse, 
                    color = combined_label1,
                    group = interaction(approach, regime1)),
                width = 0.3, position = position_dodge(width = 0.9), 
                size = 1, alpha = 0.8) + 
  
  geom_point(data = df_rb_rrmse1,
             aes(x = interaction(region, strat_var), 
                 y = mean_rrmse, 
                 fill = combined_label1, 
                 group = interaction(approach, regime1), 
                 shape = combined_label1), 
             size = 2, position = position_dodge(width = 0.9), 
             color = "black") + 
  
    labs(y = expression("RRMSE of " * widehat(CV)),y='')+
    theme_bw() + 
  
  facet_wrap(~common, scales = 'free_y', nrow = 5, dir = 'h') +
  
  scale_fill_manual(values = c(
    'rand - all' = 'grey30',
    'sb - all' = 'grey30',
    'rand - dyn' = '#bcae19',
    'sb - dyn' = '#bcae19'),
    label = c('random\nstatic',
              'balanced random\nstatic',
              'random\nadaptive',
              'balanced random\nadaptive'),
    name = "sampling allocation and design approach") +
  
  scale_color_manual(values = c(
    'rand - all' = 'grey30',
    'sb - all' = 'grey30',
    'rand - dyn' = '#bcae19',
    'sb - dyn' = '#bcae19'),
    label = c('random\nstatic',
              'balanced random\nstatic',
              'random\nadaptive',
              'balanced random\nadaptive'),
    name = "sampling allocation and design approach") +
  
  scale_shape_manual(values = c(
    'rand - all' = 21,
    'sb - all' = 24,
    'rand - dyn' = 21,
    'sb - dyn' = 24),
    label = c('random\nstatic',
              'balanced random\nstatic',
              'random\nadaptive',
              'balanced random\nadaptive'),
    name = "sampling allocation and design approach") +
  
  scale_y_continuous(labels = scales::label_number(accuracy = 0.01)) +
  theme(
    panel.grid.minor = element_line(linetype = 2, color = "grey90"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.key.size = unit(30, "points"),
    legend.text = element_text(size = 11),
    legend.spacing = unit(0.3, "cm"),
    legend.title = element_text(size = 12, hjust = 0.5),
    strip.background = element_blank(),
    legend.background = element_blank(),
    panel.spacing.x = unit(0.5, "lines"),
    panel.spacing.y = unit(1, "lines"),
    strip.text = element_blank(),
    axis.title.x = element_blank(),
    axis.text = element_text(size = 10)
  ) +
  
  guides(
    fill = guide_legend(override.aes = list(size = 4), nrow = 1, title.position = "top"),
    color = guide_legend(override.aes = list(size = 4), nrow = 1, title.position = "top"),
    shape = guide_legend(override.aes = list(size = 4), nrow = 1, title.position = "top")
  ) +
  
  #scale_x_discrete(
  #           labels = function(x) gsub("\\+", "\n", x)
  #)  +
  scale_x_discrete(labels = function(x) {
    # Keep everything before the first dot
    x <- sub("\\..*$", "", x)
    # Replace + with line break
    x <- gsub("\\+", "\n", x)
    return(x)
  })+

  geom_text(
    data = y_scale,
    aes(label = common, y = text),
    x = Inf,
    hjust = 1.1,
    #vjust = 1,
    size = 4,
    lineheight = 0.8,
    inherit.aes = FALSE
  ) +
  geom_blank(
    data = y_scale,
    aes(x = x_dummy, y = scale)
  )+
  coord_cartesian(clip = "off")

  

ragg::agg_png(paste0('figures/slope/RRMSE_cv.png'), width = 18, height = 12, units = "in", res = 300)
#ragg::agg_png(paste0('./figures/ms_hist_indices_cv_box_EBSNBS_suppl.png'), width = 13, height = 8, units = "in", res = 300)
p
dev.off()


# Keep only necessary simulations (e.g., sim 1) or average across sims if desired
df_rb_rrmse2 <- df_sub[, .(
  mean_rel_bias = mean(mean_rel_bias, na.rm = TRUE),
  sd_rel_bias   = sd(mean_rel_bias, na.rm = TRUE),
  mean_rrmse    = mean(rrmse, na.rm = TRUE),
  sd_rrmse      = sd(rrmse, na.rm = TRUE)
), by = .(spp, scn, approach,regime)]

# Step 2: Join with scenario-level metadata if needed
# Assuming samp_df contains mapping of scn to region, strat_var, approach, regime1, etc.
df_rb_rrmse2<-merge(df_rb_rrmse2,samp_df1,by='scn')
# Replace '+' with '\n' in 'region'
df_rb_rrmse2[, region1 := gsub("\\+", "\n", region)]
# Rename factor levels for 'strat_var' safely
df_rb_rrmse2[, strat_var := factor(strat_var, levels = c(levels(strat_var)[1:2], 'depth', 'varSBT'))]
setattr(df_rb_rrmse2$strat_var, "levels", c('depth', 'varSBT'))  # Update levels properly
#df_rb_rrmse2$regime<-ifelse(df_rb_rrmse2$type=='static','all','dyn')
df_rb_rrmse2$regime1<-ifelse(df_rb_rrmse2$regime=='all','all','dyn')
df_rb_rrmse2$combined_label<-paste0(df_rb_rrmse2$approach," - ",df_rb_rrmse2$regime)

df_rb_rrmse2$combined_label1<-paste0(df_rb_rrmse2$approach," - ",df_rb_rrmse2$regime1)
df_rb_rrmse2$combined_label1<-factor(df_rb_rrmse2$combined_label1,c("rand - all",'sb - all',"rand - dyn",'sb - dyn'))
df_rb_rrmse2<-merge(df_rb_rrmse2,df_spp,by='spp')

df_rb_rrmse2$region<-gsub('SBS','BSS',df_rb_rrmse2$region)

#for geom_blank(0 and adjust scale)
y_scale <- df_rb_rrmse1 |>
  group_by(common) |>
  summarise(
    top = max(mean_rrmse + sd_rrmse, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    scale = top * 1.10,
    text  = top * 1.05,
    x_dummy = levels(interaction(df_rb_rrmse1$region, df_rb_rrmse1$strat_var))[1]
  )

#y_scale$scn_label<-'EBS\ndepth'
#y_scale[4,2:4]<-3

# Define the order of the legend items
desired_order <- c(
  'rand - cold',
  'sb - cold',
  'rand - warm',
  'sb - warm'
)


# Convert combined_label to a factor with this order
df_rb_rrmse2$combined_label <- factor(df_rb_rrmse2$combined_label, levels = desired_order)



p<-
  ggplot(na.omit(df_rb_rrmse2)) +
  
  geom_errorbar(aes(x = interaction(region,strat_var), ymin = mean_rrmse - sd_rrmse, ymax = mean_rrmse + sd_rrmse, color = combined_label,
                    group = interaction(scn, approach, spp, regime)),
                width = 0.3, position = position_dodge(width = 0.9),size=1,alpha=0.8) + 
  geom_point(aes(x = interaction(region,strat_var), y = mean_rrmse, fill = combined_label, 
                 group = interaction(scn, approach, spp, regime), 
                 shape = combined_label), 
             size = 2, position = position_dodge(width = 0.9), color = "black") + 
  labs(y = expression("RRMSE of " * widehat(CV)),y='')+
  theme_bw() + 
  
  facet_wrap(~common, scales = 'free_y', nrow = 5, dir = 'h') +
  
  
  scale_y_continuous(labels = scales::label_number(accuracy = 0.01)) +
  
  scale_fill_manual(values = c(
    'rand - all' = 'grey30',
    'sb - all' = 'grey30',
    'rand - cold' = '#1675ac',
    'sb - cold' = '#1675ac',
    'rand - warm' = "#cc1d1f",
    'sb - warm' = "#cc1d1f"), 
    label = c('random\nstatic',
              'balanced random\nstatic',
              'random\nadaptive cold',
              'balanced random\nadaptive cold',
              'random\nadaptive warm',
              'balanced random\nadaptive warm'),
    name = "sampling allocation and design regime approach") +
  scale_color_manual(values = c(
    'rand - all' = 'grey30',
    'sb - all' = 'grey30',
    'rand - cold' = '#1675ac',
    'sb - cold' = '#1675ac',
    'rand - warm' = "#cc1d1f",
    'sb - warm' = "#cc1d1f"),
    label = c('random\nstatic',
              'balanced random\nstatic',
              'random\nadaptive cold',
              'balanced random\nadaptive cold',
              'random\nadaptive warm',
              'balanced random\nadaptive warm'),
    name = "sampling allocation and design regime approach") +
  
  scale_linetype_manual(values = c('rand - all' = 'solid',
                                   'sb - all' = 'dashed',
                                   'rand - cold' = 'solid',
                                   'sb - cold' = 'dashed',
                                   'rand - warm' = 'solid',
                                   'sb - warm' = 'dashed'),
                        label = c('random\nstatic',
                                  'balanced random\nstatic',
                                  'random\nadaptive cold',
                                  'balanced random\nadaptive cold',
                                  'random\nadaptive warm',
                                  'balanced random\nadaptive warm'),
                        name = "sampling allocation and design regime approach") +
  
  scale_shape_manual(values = c('rand - all' = 21,
                                'sb - all' = 24,
                                'rand - cold' = 21,
                                'sb - cold' = 24,
                                'rand - warm' = 21,
                                'sb - warm' = 24),
                     label = c('random\nstatic',
                               'balanced random\nstatic',
                               'random\nadaptive cold',
                               'balanced random\nadaptive cold',
                               'random\nadaptive warm',
                               'balanced random\nadaptive warm'),
                     name = "sampling allocation and design regime approach") +
  
  #scale_y_continuous(expand = c(0, 0), limits = c(0, NA),labels = scales::label_number(accuracy = 0.01))+
  theme(
    panel.grid.minor = element_line(linetype = 2, color = "grey90"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.key.size = unit(30, "points"),
    legend.text = element_text(size = 11),
    legend.spacing = unit(0.3, "cm"),
    legend.title = element_text(size = 12, hjust = 0.5),
    strip.background = element_blank(),
    legend.background = element_blank(),
    panel.spacing.x = unit(0.5, "lines"),
    panel.spacing.y = unit(1, "lines"),
    strip.text = element_blank(),
    axis.title.x = element_blank(),
    axis.text = element_text(size = 10)
  ) +
  
  guides(
    fill = guide_legend(override.aes = list(size = 4), nrow = 1, title.position = "top"),
    color = guide_legend(override.aes = list(size = 4), nrow = 1, title.position = "top"),
    shape = guide_legend(override.aes = list(size = 4), nrow = 1, title.position = "top")
  ) +
  
    #scale_x_discrete(
    #           labels = function(x) gsub("\\+", "\n", x)
    #)  +
    scale_x_discrete(labels = function(x) {
      # Keep everything before the first dot
      x <- sub("\\..*$", "", x)
      # Replace + with line break
      x <- gsub("\\+", "\n", x)
      return(x)
    })+
  geom_text(
    data = y_scale,
    aes(label = common, y = text),
    x = Inf,
    hjust = 1.1,
    #vjust = 1,
    size = 4,
    lineheight = 0.8,
    inherit.aes = FALSE
  ) +
  geom_blank(
    data = y_scale,
    aes(x = x_dummy, y = scale)
  )+
  coord_cartesian(clip = "off")


  ragg::agg_png(paste0('figures/slope/RRMSE_cv_warmcold.png'), width = 18, height = 12, units = "in", res = 300)
  #ragg::agg_png(paste0('./figures/ms_hist_indices_cv_box_EBSNBS_suppl.png'), width = 13, height = 8, units = "in", res = 300)
  p
  dev.off()
  
  #4.2 PLOT RBIAS OF CV ####
  
  #for geom_blank(0 and adjust scale)

  y_scale <- df_rb_rrmse1 |>
    group_by(common) |>
    summarise(
      top = max(mean_rel_bias + sd_rel_bias, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      scale = top * 1.10,
      text  = top * 1.05,
      x_dummy = levels(interaction(df_rb_rrmse1$region, df_rb_rrmse1$strat_var))[1]
    )
  
  
  
  
  p<-
    ggplot() +
    #geom_hline(yintercept = 0, alpha = 0.5, linetype = 'dotted') +
    
    geom_errorbar(data = df_rb_rrmse1, 
                  aes(x = interaction(region, strat_var), 
                      ymin = mean_rel_bias - sd_rel_bias, 
                      ymax = mean_rel_bias + sd_rel_bias, 
                      color = combined_label1,
                      group = interaction(approach, regime1)),
                  width = 0.3, position = position_dodge(width = 0.9), 
                  size = 1, alpha = 0.8) + 
    
    geom_point(data = df_rb_rrmse1,
               aes(x = interaction(region, strat_var), 
                   y = mean_rel_bias, 
                   fill = combined_label1, 
                   group = interaction(approach, regime1), 
                   shape = combined_label1), 
               size = 2, position = position_dodge(width = 0.9), 
               color = "black") + 
    
    labs(y = expression("RBIAS of " * widehat(CV) * " (%)"),y='')+
    
    theme_bw() + 
    
    facet_wrap(~common, scales = 'free_y', nrow = 5, dir = 'h') +
    
    scale_fill_manual(values = c(
      'rand - all' = 'grey30',
      'sb - all' = 'grey30',
      'rand - dyn' = '#bcae19',
      'sb - dyn' = '#bcae19'),
      label = c('random\nstatic',
                'balanced random\nstatic',
                'random\nadaptive',
                'balanced random\nadaptive'),
      name = "sampling allocation and design approach") +
    
    scale_color_manual(values = c(
      'rand - all' = 'grey30',
      'sb - all' = 'grey30',
      'rand - dyn' = '#bcae19',
      'sb - dyn' = '#bcae19'),
      label = c('random\nstatic',
                'balanced random\nstatic',
                'random\nadaptive',
                'balanced random\nadaptive'),
      name = "sampling allocation and design approach") +
    
    scale_shape_manual(values = c(
      'rand - all' = 21,
      'sb - all' = 24,
      'rand - dyn' = 21,
      'sb - dyn' = 24),
      label = c('random\nstatic',
                'balanced random\nstatic',
                'random\nadaptive',
                'balanced random\nadaptive'),
      name = "sampling allocation and design approach") +
    
    scale_y_continuous(labels = scales::label_number(accuracy = 0.01)) +
    #scale_x_discrete(guide = guide_axis_nested(angle = 0),
      #               labels = function(x) gsub("\\+", "\n", x)) +
    
    theme(
      panel.grid.minor = element_line(linetype = 2, color = "grey90"),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.key.size = unit(30, "points"),
      legend.text = element_text(size = 11),
      legend.spacing = unit(0.3, "cm"),
      legend.title = element_text(size = 12, hjust = 0.5),
      strip.background = element_blank(),
      legend.background = element_blank(),
      panel.spacing.x = unit(0.5, "lines"),
      panel.spacing.y = unit(1, "lines"),
      strip.text = element_blank(),
      axis.title.x = element_blank(),
      axis.text = element_text(size = 10)
    ) +
    
    guides(
      fill = guide_legend(override.aes = list(size = 4), nrow = 1, title.position = "top"),
      color = guide_legend(override.aes = list(size = 4), nrow = 1, title.position = "top"),
      shape = guide_legend(override.aes = list(size = 4), nrow = 1, title.position = "top")
    ) +
    
  #scale_x_discrete(
  #           labels = function(x) gsub("\\+", "\n", x)
  #)  +
  scale_x_discrete(labels = function(x) {
    # Keep everything before the first dot
    x <- sub("\\..*$", "", x)
    # Replace + with line break
    x <- gsub("\\+", "\n", x)
    return(x)
  })+
    
    geom_text(
      data = y_scale,
      aes(label = common, y = text),
      x = Inf,
      hjust = 1.1,
      #vjust = 1,
      size = 4,
      lineheight = 0.8,
      inherit.aes = FALSE
    ) +
    geom_blank(
      data = y_scale,
      aes(x = x_dummy, y = scale)
    )+
    coord_cartesian(clip = "off")
  
  
  ragg::agg_png(paste0('figures/slope/RBIAS_cv.png'), width = 18, height = 12, units = "in", res = 300)
  #ragg::agg_png(paste0('./figures/ms_hist_indices_cv_box_EBSNBS_suppl.png'), width = 13, height = 8, units = "in", res = 300)
  p
  dev.off()
  
  
  # Keep only necessary simulations (e.g., sim 1) or average across sims if desired
  df_rb_rrmse2 <- df_sub[, .(
    mean_rel_bias = mean(mean_rel_bias, na.rm = TRUE),
    sd_rel_bias   = sd(mean_rel_bias, na.rm = TRUE),
    mean_rrmse    = mean(rrmse, na.rm = TRUE),
    sd_rrmse      = sd(rrmse, na.rm = TRUE)
  ), by = .(spp, scn, approach,regime)]
  
  # Step 2: Join with scenario-level metadata if needed
  # Assuming samp_df contains mapping of scn to region, strat_var, approach, regime1, etc.
  df_rb_rrmse2<-merge(df_rb_rrmse2,samp_df1,by='scn')
  # Replace '+' with '\n' in 'region'
  df_rb_rrmse2[, region1 := gsub("\\+", "\n", region)]
  # Rename factor levels for 'strat_var' safely
  df_rb_rrmse2[, strat_var := factor(strat_var, levels = c(levels(strat_var)[1:2], 'depth', 'varSBT'))]
  setattr(df_rb_rrmse2$strat_var, "levels", c('depth', 'varSBT'))  # Update levels properly
  #filter by spp
  #df_rb_rrmse2$regime<-ifelse(df_rb_rrmse2$type=='static','all','dyn')
  df_rb_rrmse2$regime1<-ifelse(df_rb_rrmse2$regime=='all','all','dyn')
  df_rb_rrmse2$combined_label<-paste0(df_rb_rrmse2$approach," - ",df_rb_rrmse2$regime)
  
  df_rb_rrmse2$combined_label1<-paste0(df_rb_rrmse2$approach," - ",df_rb_rrmse2$regime1)
  df_rb_rrmse2$combined_label1<-factor(df_rb_rrmse2$combined_label1,c("rand - all",'sb - all',"rand - dyn",'sb - dyn'))
  df_rb_rrmse2<-merge(df_rb_rrmse2,df_spp,by='spp')
  
  df_rb_rrmse2$region<-gsub('SBS','BSS',df_rb_rrmse2$region)
  
  
  
  # Convert combined_label to a factor with this order
  df_rb_rrmse2$combined_label <- factor(df_rb_rrmse2$combined_label, levels = desired_order)
  
  
  p<-
    ggplot(na.omit(df_rb_rrmse2)) +
    
    geom_errorbar(aes(x = interaction(region,strat_var), ymin = mean_rel_bias - sd_rel_bias, ymax = mean_rel_bias + sd_rel_bias, color = combined_label,
                      group = interaction(scn, approach, spp, regime)),
                  width = 0.3, position = position_dodge(width = 0.9),size=1,alpha=0.8) + 
    geom_point(aes(x = interaction(region,strat_var), y = mean_rel_bias, fill = combined_label, 
                   group = interaction(scn, approach, spp, regime), 
                   shape = combined_label), 
               size = 2, position = position_dodge(width = 0.9), color = "black") + 
    labs(y = expression("RBIAS of " * widehat(CV) * " (%)"),y='')+
    theme_bw() + 
    
    facet_wrap(~common, scales = 'free_y', nrow = 5, dir = 'h') +
    
    
    scale_y_continuous(labels = scales::label_number(accuracy = 0.01)) +
    
    scale_fill_manual(values = c(
      'rand - all' = 'grey30',
      'sb - all' = 'grey30',
      'rand - cold' = '#1675ac',
      'sb - cold' = '#1675ac',
      'rand - warm' = "#cc1d1f",
      'sb - warm' = "#cc1d1f"), 
      label = c('random\nstatic',
                'balanced random\nstatic',
                'random\nadaptive cold',
                'balanced random\nadaptive cold',
                'random\nadaptive warm',
                'balanced random\nadaptive warm'),
      name = "sampling allocation and design regime approach") +
    scale_color_manual(values = c(
      'rand - all' = 'grey30',
      'sb - all' = 'grey30',
      'rand - cold' = '#1675ac',
      'sb - cold' = '#1675ac',
      'rand - warm' = "#cc1d1f",
      'sb - warm' = "#cc1d1f"),
      label = c('random\nstatic',
                'balanced random\nstatic',
                'random\nadaptive cold',
                'balanced random\nadaptive cold',
                'random\nadaptive warm',
                'balanced random\nadaptive warm'),
      name = "sampling allocation and design regime approach") +
    
    scale_linetype_manual(values = c('rand - all' = 'solid',
                                     'sb - all' = 'dashed',
                                     'rand - cold' = 'solid',
                                     'sb - cold' = 'dashed',
                                     'rand - warm' = 'solid',
                                     'sb - warm' = 'dashed'),
                          label = c('random\nstatic',
                                    'balanced random\nstatic',
                                    'random\nadaptive cold',
                                    'balanced random\nadaptive cold',
                                    'random\nadaptive warm',
                                    'balanced random\nadaptive warm'),
                          name = "sampling allocation and design regime approach") +
    
    scale_shape_manual(values = c('rand - all' = 21,
                                  'sb - all' = 24,
                                  'rand - cold' = 21,
                                  'sb - cold' = 24,
                                  'rand - warm' = 21,
                                  'sb - warm' = 24),
                       label = c('random\nstatic',
                                 'balanced random\nstatic',
                                 'random\nadaptive cold',
                                 'balanced random\nadaptive cold',
                                 'random\nadaptive warm',
                                 'balanced random\nadaptive warm'),
                       name = "sampling allocation and design regime approach") +
    
    #scale_y_continuous(expand = c(0, 0), limits = c(0, NA),labels = scales::label_number(accuracy = 0.01))+
    #scale_x_discrete(guide = guide_axis_nested(angle=0),labels = function(x) gsub("\\+", "\n", x))+
    theme(
      panel.grid.minor = element_line(linetype = 2, color = "grey90"),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.key.size = unit(30, "points"),
      legend.text = element_text(size = 11),
      legend.spacing = unit(0.3, "cm"),
      legend.title = element_text(size = 12, hjust = 0.5),
      strip.background = element_blank(),
      legend.background = element_blank(),
      panel.spacing.x = unit(0.5, "lines"),
      panel.spacing.y = unit(1, "lines"),
      strip.text = element_blank(),
      axis.title.x = element_blank(),
      axis.text = element_text(size = 10)
    ) +
    
    guides(
      fill = guide_legend(override.aes = list(size = 4), nrow = 1, title.position = "top"),
      color = guide_legend(override.aes = list(size = 4), nrow = 1, title.position = "top"),
      shape = guide_legend(override.aes = list(size = 4), nrow = 1, title.position = "top")
    ) +
    
    #scale_x_discrete(
    #           labels = function(x) gsub("\\+", "\n", x)
    #)  +
    scale_x_discrete(labels = function(x) {
      # Keep everything before the first dot
      x <- sub("\\..*$", "", x)
      # Replace + with line break
      x <- gsub("\\+", "\n", x)
      return(x)
    })+
      
      geom_text(
        data = y_scale,
        aes(label = common, y = text),
        x = Inf,
        hjust = 1.1,
        #vjust = 1,
        size = 4,
        lineheight = 0.8,
        inherit.aes = FALSE
      ) +
      geom_blank(
        data = y_scale,
        aes(x = x_dummy, y = scale)
      )+
      coord_cartesian(clip = "off")
  
  ragg::agg_png(paste0('figures/slope/RBIAS_cv_warmcold.png'), width = 18, height = 12, units = "in", res = 300)
  #ragg::agg_png(paste0('./figures/ms_hist_indices_cv_box_EBSNBS_suppl.png'), width = 13, height = 8, units = "in", res = 300)
  p
  dev.off()
  
  


#5 CV ratio ####
#LOAD
#load(file = 'output/slope//estimated_cvs.RData') #cv3
  load(file = 'output/slope//estimated_cvs_spp_dens.RData') #cv3
  
#merge samp data
setDT(samp_df)
samp_df$region<-gsub('SBS','BSS',samp_df$region)
samp_df<-samp_df[, .(region,strat_var,samp_scn)]

# Perform the merge (by matching 'scn' with 'samp_scn' and 'region' with 'region')
cv3 <- merge(cv3, samp_df, by.x = c('scn','region','strat_var'), by.y = c('samp_scn','region','strat_var'), all.x = TRUE)
  
# Aggregate data using data.table
cv33 <- cv3[, .(cv_ebs = mean(cv_sim)), 
                 by = .(spp, year, scn, approach, regime, sim,region,strat_var)]
x<-na.omit(cv33)
unique(x[, .( region)])

#filter cvEBS
cv3ebs <- cv3[region == "EBS" ]
cv3ebs<-na.omit(cv3ebs)
unique(cv3ebs[, .(scn)])

# # Select specific columns, 
cv3ebs <- cv3ebs[, .(region,strat_var,approach,spp,regime,cv_sim,sim,year)]

# Aggregate data using data.table
cv3ebs <- cv3ebs[, .(cv_ebs = mean(cv_sim)), 
           by = .(spp, year, strat_var, approach, regime, sim)]

# Merge by the specified columns
cv4ebs <- merge(cv3, cv3ebs, 
                    by = c("spp", "year", "approach", "regime", "sim",'strat_var'), 
                    all.x = TRUE)  # Use all=TRUE for a full join (keeps unmatched rows)

#log ratio
cv4ebs$ratio<-log(cv4ebs$cv_sim/cv4ebs$cv_ebs)

#save
save(cv4ebs,file = 'output/slope//estimated_cvratio_dens.RData') #cv4ebs
load(file = 'output/slope//estimated_cvratio_dens.RData')

# Perform the merge (by matching 'scn' with 'samp_scn' and 'region' with 'region')
cv5ebs <-cv4ebs
#cv5ebs <- merge(cv4ebs, samp_df_sub, by.x = c('scn','region','strat_var'), by.y = c('samp_scn','region','strat_var'), all.x = TRUE)

#filter
cv5ebs <- cv5ebs[!(regime == "cold" & year %in% c(2002:2005,2014:2016))]
cv5ebs <- cv5ebs[!(regime == "warm" & year %in% 2006:2013)]

# Compute mean and standard deviation while keeping 'common' and 'strat_var'
df_summary <- cv5ebs[, .(
  mean_ratio = mean(ratio, na.rm = TRUE),
  q90 = quantile(ratio,probs=0.90, na.rm = TRUE),
  q10 = quantile(ratio,probs=0.10, na.rm = TRUE)
), by = .(region, approach, spp, scn, regime, strat_var)]
#unique(df_summary[, .(scn, region)])

df_summary$region1<-gsub('+','\n',df_summary$region)
levels(df_summary$strat_var)[1:2]<-c('varSBT','depth')
df_summary$strat_var<-factor(df_summary$strat_var,levels = c('depth','varSBT'))

#label scn
df_summary$scn_label<-paste0(df_summary$region,'\n',df_summary$strat_var)

df_summary$common <- df_spp$common[match(df_summary$spp, df_spp$spp)]
#df_summary$mean_sd<-df_summary$mean_ratio+df_summary$sd_ratio

# Create a new combined variable in your data
df_summary$combined_label <- paste(df_summary$approach, df_summary$regime, sep = " - ")
# #sort factors just in case
df_summary$combined_label<-factor(df_summary$combined_label,levels=c('rand - all' ,
                                                       'sb - all' ,
                                                       'rand - cold' ,
                                                       'sb - cold' ,
                                                       'rand - warm' ,
                                                       'sb - warm' ) )

#for geom_blank(0 and adjust scale)
#y_scale <- aggregate(q90 ~ common, df_summary, max)
 y_scale <- df_summary |>
    group_by(common) |>
    summarise(
      top = max(q90, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      scale = top * 1.10,
      text  = top * 1.05,
      x_dummy = levels(interaction(df_summary$region, df_summary$strat_var))[1]
    )

# Convert combined_label to a factor with this order
df_summary$combined_label <- factor(df_summary$combined_label, levels = desired_order)


p<-
  ggplot(na.omit(subset(df_summary,  region !='EBS'))) +
    
    geom_hline(yintercept = 0,linetype='dashed')+
  geom_errorbar(aes(x = interaction(region,strat_var), ymin = q10, ymax = q90, color = combined_label, 
                    group = interaction(approach, spp, regime)),
                width = 0.3, position = position_dodge(width = 0.8),size=1,alpha=0.8) + 
  geom_point(aes(x = interaction(region,strat_var), y = mean_ratio, fill = combined_label, 
                 group = interaction(approach, spp, regime), 
                 shape = combined_label), 
             size = 2, position = position_dodge(width = 0.8), color = "black") + 
  labs(y = expression(log(widehat(CV)/widehat(CV)[EBS])), x = '') +   
  theme_bw() + 
  facet_wrap(~common , nrow = 5, dir = 'h') +
  
  scale_fill_manual(values = c(
    'rand - all' = 'grey30',
    'sb - all' = 'grey30',
    'rand - cold' = '#1675ac',
    'sb - cold' = '#1675ac',
    'rand - warm' = "#cc1d1f",
    'sb - warm' = "#cc1d1f"), 
    label = c('random\nstatic',
              'balanced random\nstatic',
              'random\nadaptive cold',
              'balanced random\nadaptive cold',
              'random\nadaptive warm',
              'balanced random\nadaptive warm'),
    name = "sampling allocation and design regime approach") +
    scale_color_manual(values = c(
      'rand - all' = 'grey30',
      'sb - all' = 'grey30',
      'rand - cold' = '#1675ac',
      'sb - cold' = '#1675ac',
      'rand - warm' = "#cc1d1f",
      'sb - warm' = "#cc1d1f"), 
      label = c('random\nstatic',
                'balanced random\nstatic',
                'random\nadaptive cold',
                'balanced random\nadaptive cold',
                'random\nadaptive warm',
                'balanced random\nadaptive warm'),
      name = "sampling allocation and design regime approach") +
  
  scale_linetype_manual(values = c('rand - all' = 'solid',
                                   'sb - all' = 'dashed',
                                   'rand - cold' = 'solid',
                                   'sb - cold' = 'dashed',
                                   'rand - warm' = 'solid',
                                   'sb - warm' = 'dashed'),
                        label = c('random\nstatic',
                                  'balanced random\nstatic',
                                  'random\nadaptive cold',
                                  'balanced random\nadaptive cold',
                                  'random\nadaptive warm',
                                  'balanced random\nadaptive warm'),
                        name = "sampling allocation and design regime approach") +
  
  scale_shape_manual(values = c('rand - all' = 21,
                                'sb - all' = 24,
                                'rand - cold' = 21,
                                'sb - cold' = 24,
                                'rand - warm' = 21,
                                'sb - warm' = 24),
                     label = c('random\nstatic',
                               'balanced random\nstatic',
                               'random\nadaptive cold',
                               'balanced random\nadaptive cold',
                               'random\nadaptive warm',
                               'balanced random\nadaptive warm'),
                     name = "sampling allocation and design regime approach") +
  
    #scale_y_continuous(limits = c(-0.25,0.45))+
  #scale_y_continuous(expand = expansion(mult = c(0.03, 0)),limits = c(0,NA),labels = scales::label_number(accuracy = 0.01))+
  theme(
    panel.grid.minor = element_line(linetype = 2, color = "grey90"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.key.size = unit(30, "points"),
    legend.text = element_text(size = 11),
    legend.spacing = unit(0.3, "cm"),
    legend.title = element_text(size = 12, hjust = 0.5),
    strip.background = element_blank(),
    legend.background = element_blank(),
    panel.spacing.x = unit(0.5, "lines"),
    panel.spacing.y = unit(1, "lines"),
    strip.text = element_blank(),
    axis.title.x = element_blank(),
    axis.text = element_text(size = 10)
  ) +
  
  guides(
    fill = guide_legend(override.aes = list(size = 4), nrow = 1, title.position = "top"),
    color = guide_legend(override.aes = list(size = 4), nrow = 1, title.position = "top"),
    shape = guide_legend(override.aes = list(size = 4), nrow = 1, title.position = "top")
  ) +
  
  #scale_x_discrete(
  #           labels = function(x) gsub("\\+", "\n", x)
  #)  +
  scale_x_discrete(labels = function(x) {
    # Keep everything before the first dot
    x <- sub("\\..*$", "", x)
    # Replace + with line break
    x <- gsub("\\+", "\n", x)
    return(x)
  })+
  geom_text(
    data = y_scale,
    aes(label = common, y = text),
    x = Inf,
    hjust = 1.1,
    #vjust = 1,
    size = 4,
    lineheight = 0.8,
    inherit.aes = FALSE
  ) +
  geom_blank(
    data = y_scale,
    aes(x = x_dummy, y = scale)
  )+
  coord_cartesian(clip = "off")

#save plot
ragg::agg_png(paste0('figures/slope/logcvratio_warmcold.png'), width = 18, height = 12, units = "in", res = 300)
#ragg::agg_png(paste0('./figures/ms_hist_indices_cv_box_EBSNBS_suppl.png'), width = 13, height = 8, units = "in", res = 300)
p
dev.off()



df_summary$regime1<-ifelse(df_summary$regime=='all','all','dyn')
df_summary$combined_label1<-paste0(df_summary$approach," - ",df_summary$regime1)

df_summary_clean <- df_summary %>%
  group_by(region, approach,strat_var, combined_label1, region1, regime1, common) %>%
  summarise(
    mean_ratio = mean(mean_ratio, na.rm = TRUE),
    q10 = mean(q10, na.rm = TRUE),  # or another logic
    q90 = mean(q90, na.rm = TRUE),  # or another logic
    
    .groups = "drop"
  )



df_summary_clean$combined_label1<-factor(df_summary_clean$combined_label1,c("rand - all",'sb - all',"rand - dyn",'sb - dyn'))
df_summary_clean$combined_label1 <- factor(df_summary_clean$combined_label1, c("rand - all",'sb - all',"rand - dyn",'sb - dyn'))

p1 <-
  ggplot(na.omit(subset(df_summary_clean, region != 'EBS'))) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  geom_errorbar(
    aes(
      x = interaction(region, strat_var),
      ymin = q10,
      ymax = q90,
      color = combined_label1,
      group = combined_label1
    ),
    width = 0.3,
    position = position_dodge(width = 0.8),
    size = 1,
    alpha = 0.8
  ) +
  geom_point(
    aes(
      x = interaction(region, strat_var),
      y = mean_ratio,
      fill = combined_label1,
      group = combined_label1,
      shape = combined_label1
    ),
    size = 2.5,
    position = position_dodge(width = 0.8),
    color = "black"
  ) +
  labs(y = expression(log(widehat(CV) / widehat(CV)[EBS])), x = "") +
  theme_bw() +
  facet_wrap(~common, nrow = 5, dir = "h") +
  
  scale_fill_manual(
    values = c(
      "rand - all" = "grey30",
      "sb - all" = "grey30",
      "rand - dyn" = "#bcae19",
      "sb - dyn" = "#bcae19"
    ),
    label = c(
      "random\nstatic",
      "balanced random\nstatic",
      "random\nadaptive",
      "balanced random\nadaptive"
    ),
    name = "sampling allocation and design approach"
  ) +
  scale_color_manual(
    values = c(
      "rand - all" = "grey30",
      "sb - all" = "grey30",
      "rand - dyn" = "#bcae19",
      "sb - dyn" = "#bcae19"
    ),
    label = c(
      "random\nstatic",
      "balanced random\nstatic",
      "random\nadaptive",
      "balanced random\nadaptive"
    ),
    name = "sampling allocation and design approach"
  ) +
  scale_shape_manual(
    values = c(
      "rand - all" = 21,
      "sb - all" = 24,
      "rand - dyn" = 21,
      "sb - dyn" = 24
    ),
    label = c(
      "random\nstatic",
      "balanced random\nstatic",
      "random\nadaptive",
      "balanced random\nadaptive"
    ),
    name = "sampling allocation and design approach"
  ) +
  
  theme(
    panel.grid.minor = element_line(linetype = 2, color = "grey90"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.key.size = unit(30, "points"),
    legend.text = element_text(size = 11),
    legend.spacing = unit(0.3, "cm"),
    legend.title = element_text(size = 12, hjust = 0.5),
    strip.background = element_blank(),
    legend.background = element_blank(),
    panel.spacing.x = unit(0.5, "lines"),
    panel.spacing.y = unit(1, "lines"),
    strip.text = element_blank(),
    axis.title.x = element_blank(),
    axis.text = element_text(size = 10)
  ) +
  
  guides(
    fill = guide_legend(override.aes = list(size = 4), nrow = 1, title.position = "top"),
    color = guide_legend(override.aes = list(size = 4), nrow = 1, title.position = "top"),
    shape = guide_legend(override.aes = list(size = 4), nrow = 1, title.position = "top")
  ) +
  
  scale_x_discrete(labels = function(x) {
    x <- sub("\\..*$", "", x)
    x <- gsub("\\+", "\n", x)
    x
  }) +
  
  geom_text(
    data = y_scale,
    aes(label = common, y = text),
    x = Inf,
    hjust = 1.1,
    size = 4,
    lineheight = 0.8,
    inherit.aes = FALSE
  ) +
  geom_blank(
    data = y_scale,
    aes(x = x_dummy, y = scale)
  ) +
  coord_cartesian(clip = "off")

ragg::agg_png(paste0("figures/slope/logcvratio.png"), width = 18, height = 12, units = "in", res = 300)
p1
dev.off()
