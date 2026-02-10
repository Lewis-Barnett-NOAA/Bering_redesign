#Script#9 survey performance for slope Bering Sea
#danielvilasgonzalez@gmail.com/dvilasg@uw.edu
#calculate BIAS + RRMSE + CV of INDEX and CV of INDEX

#each estimate:
#predicted abundance 
# abundance true (100replicas+23spp+14y+12scn+2allocation)
# estimates abundance x100sim surveys
# cv of abundance true
# cv of abundance estimate
# rrmse of abundance
# rbias of abundance
# cv of abundance true
# cv of abundance estimate
# cv of abundance true
# cv of abundance estimate
# rrmse of abundance
# rbias of abundance

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
out_dir<-'/Users/daniel/Work/UW-NOAA/Adapting Monitoring to a Changing Seascape/'
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
sel_spp<-c('Gadus chalcogrammus',
           'Gadus macrocephalus',
           'Chionoecetes opilio',
           'Reinhardtius hippoglossoides')

#selected species
sel_spp_com<-c('Alaska pollock',
               'Pacific cod',
               'Snow crab',
               'Greenland turbot')

df_spp<-data.frame('species'=spp,
                   'common'=spp1) 

df_spp1<-df_spp
df_spp1<-df_spp1[order(df_spp1$common),]
df_spp1$label<-letters[1:nrow(df_spp1)]

#create folder simulation data
dir.create(paste0('./output slope//species/'))

#years - only years when BSS and NBS+EBS OM temporally overlap
yrs<-c(2002:2016)
n_yrs<-length(yrs)

# GRIDS #####
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

#cells assignment
EBS_cells<-15181:53464
BSS_cells<-53465:56505
NBS_cells<-1:15180

#plot settings ####

#Our transformation function
scaleFUN <- function(x) sprintf("%.3f", x)

#fxn to turn axis into scientific
scientific_10 <- function(x) {
  parse(text=gsub("e", " %*% 10^", scales::scientific_format()(x)))
}

# Shared legend items

labels_warm_cold <- c(
  "random\nstatic",
  "balanced random\nstatic",
  "random\nadaptive cold",
  "balanced random\nadaptive cold",
  "random\nadaptive warm",
  "balanced random\nadaptive warm"
)

colors_warm_cold <- c(
  "rand - all"  = "grey30",
  "sb - all"    = "grey30",
  "rand - cold" = "#1675ac",
  "sb - cold"   = "#1675ac",
  "rand - warm" = "#cc1d1f",
  "sb - warm"   = "#cc1d1f"
)

shapes_warm_cold <- c(
  "rand - all"  = 21,
  "sb - all"    = 24,
  "rand - cold" = 21,
  "sb - cold"   = 24,
  "rand - warm" = 21,
  "sb - warm"   = 24
)

linetype_warm_cold <- c(
  "rand - all"  = "solid",
  "sb - all"    = "dashed",
  "rand - cold" = "solid",
  "sb - cold"   = "dashed",
  "rand - warm" = "solid",
  "sb - warm"   = "dashed"
)

legend_title_warm_cold <- "sampling allocation and design regime approach"


labels_static_adaptive <- c(
  "random\nstatic",
  "balanced random\nstatic",
  "random\nadaptive",
  "balanced random\nadaptive"
)

colors_static_adaptive <- c(
  "rand - all" = "grey30",
  "sb - all"   = "grey30",
  "rand - dyn" = "#bcae19",
  "sb - dyn"   = "#bcae19"
)

shapes_static_adaptive <- c(
  "rand - all" = 21,
  "sb - all"   = 24,
  "rand - dyn" = 21,
  "sb - dyn"   = 24
)

linetype_static_adaptive <- c(
  "rand - all" = "solid",
  "sb - all"   = "dashed",
  "rand - dyn" = "solid",
  "sb - dyn"   = "dashed"
)

legend_title_static_adaptive <- "sampling allocation and design approach"


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
                     strat_var=c('depth'), #,'varTemp_forced','Depth_forced' #LonE and combinations
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
samp_df$scenario<-paste0(paste0('scn',1:nrow(samp_df)))

# TRUE ABUNDANCE INDEX predicted ####
# from predicted values from VAST
# OUTPUT #true_ind: abundance indices from predicted densities from OMs

#loop over OMs
true_index_file <-"./output slope//model_based_EBSNBSBSS.RData"

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
  
  ####### NBS + EBS + BSS  
  
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
  save(true_ind,file = paste0("./output slope//model_based_EBSNBSBSS.RData"))  
  
} else {
  # code to run when the file is missing
  message("File present, loading...")
  
  #load true ind
  load(file = true_index_file)  #true_ind
  
}

# TRUE ABUNDANCE simulated #####
# OUTPUT #true_est: abundance estimates from simulated dataset (aka replicate) 

#simulated densities
load(file = paste0('./output slope/species/ms_sim_dens_all.RData'))  #sim_dens1
dimnames(sim_dens1)

#df to store results
true_est<-array(dim=c(length(spp),length(yrs),4,100),
                dimnames = list(spp,yrs,c('EBS','EBS+BSS','EBS+NBS','EBS+NBS+BSS'),1:100))

#loop over species and crab stock to extract the true index
for (sim in 1:100) {
  
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
    true_est[,,region,sim]<-dens1
  }
  
  #true_est[,sp]<-drop_units(dens_index_hist_OM[[sp]]$index[,as.character(yrs),1])
  
}

#save true est
save(true_est,file = paste0("./output slope/true_ind_hist.RData"))  
#load(file = paste0("./output slope/true_ind_hist.RData"))  



# ESTIMATED ABUNDANCE simulated #####
#OUTPUT est_ind: abundance estimates from survey designs (100 replicas * 100 survey)
#store HIST simulated data
load(file = paste0('./data processed/index_dens_all.RData'))  #combined_sim_df
setDT(combined_sim_df)  # Convert ind2 to data.table if it's not already

#rename region for merging
combined_sim_df[
  ,
  region := gsub("SBS", "BSS", as.character(region))
]
combined_sim_df[, region := factor(region)]
combined_sim_df$year<-gsub('y','',combined_sim_df$year)

#remove years not matching season
combined_sim_df <- combined_sim_df[!(regime == "cold" & year %in% c(2002:2005,2014:2016))]
combined_sim_df <- combined_sim_df[!(regime == "warm" & year %in% 2006:2013)]
#summary(combined_sim_df$STRS_var)

#get the mean abundance estimate across replicates
est_ind <- combined_sim_df[
  ,
  .(
    est_mean = mean(STRS_mean, na.rm = TRUE)
  ),
  by = .(
    species,
    year,
    approach,
    #replicate,
    scenario,
    regime,
    region
  )
]

# PLOT SAMPLING DESIGNS ABUNDANCE ESTIMATES ####
#RELATIVE TO PREDICTED ABUNDANCE ESTIMATES FROM OM 

true_est #simulated abundance estimate 
true_ind #predicted abundance estimate 
est_ind #sampling designs abundance estimate

#merge wit spp common name and sampling design
setDT(df_spp)
setDT(samp_df)
levels(samp_df$region)<-c("EBS","EBS+NBS","EBS+BSS","EBS+NBS+BSS")

#mean true_est
true_est1 <- as.data.table(true_est, keep.rownames = TRUE)
setnames(true_est1, c("species", "year", "region", "replicate", "true_est"))
true_est_mean <- true_est1[
  , .(true_est_mean = mean(true_est, na.rm = TRUE)),
  by = .(species, year, region)
]
true_est1<-merge(true_est1,df_spp1,by='species')
true_est1$year<-as.integer(true_est1$year)
true_est1$dummy<-'true index'
true_est1$value<-true_est1$true_est

#true_ind true ind for area
#arrange true index data
true_ind1 <- melt(true_ind, varnames = c("year", "region", "species"), value.name = "value")
true_ind1<-subset(true_ind1,region!='year')
true_ind1<-true_ind1[which(true_ind1$species %in% df_spp1$species),]
true_ind1<-merge(true_ind1,df_spp1,by='species')
true_ind1$year<-as.integer(true_ind1$year)
true_ind1$dummy<-'true index'

#df as estimated index
df<-as.data.table(est_ind)   

#merge to spp common
# merge to add the common name
df <- merge(df, df_spp, by = "species", all.x = TRUE)
df <- merge(df, samp_df[,c("type","region","strat_var","scenario")],by=c('scenario','region'),all.x=TRUE)
#sort approach (station allocation)
df$approach <- factor(df$approach, levels = c("sb", "rand"))
df$year<-as.integer(df$year)

#to adjust y axis limits
df$value<-df$est_mean/1000
y_scale<-aggregate(value ~ common, df,max)
y_scale$scale<-y_scale$value+y_scale$value*0.25
y_scale$text<-y_scale$value+y_scale$value*0.20
y_scale$apr<-'sys'
y_scale$year<-2010
y_scale$scn<-'scn1'
y_scale[11,'text']<-77000
y_scale[11,'scale']<-80000

#plot abundance index for each sampling design
p<-
    ggplot() +
    geom_line(data=df, aes(x=year, y=est_mean/1000000000, color=region, group=interaction(scenario, approach, common,type), 
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
    #geom_text(data=df, aes(label = label), x = 1984, y = Inf, vjust = 1.5, size=5) +
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
ragg::agg_png(paste0('./figures slope/ms_ind_EBSNBSBSS.png'), width = 14, height = 8, units = "in", res = 300)
p
dev.off()

#1 TRUE CV  #####
    
#get the mean STRS_var across replicates
sd_est_ind <- combined_sim_df[
  ,
  .(
    sd_STRS = mean(sqrt(STRS_var), na.rm = TRUE)
  ),
  by = .(
    species,
    year,
    approach,
    replicate,
    scenario,
    regime,
    region
  )
]
sd_est_ind$year<-as.integer(sd_est_ind$year)

#mean true_est
true_est1 <- as.data.table(true_est, keep.rownames = TRUE)
setnames(true_est1, c("species", "year", "region", "replicate", "true_est"))
true_est1 <- true_est1[
  , .(true_est_mean = mean(true_est, na.rm = TRUE)),
  by = .(species, year, region,replicate)
]

true_est1$year<-as.integer(true_est1$year)
true_est1$dummy<-'true index'
true_est1$value<-true_est1$true_est
true_est1$replicate <- as.integer(true_est1$replicate)
    
# Merge
true_ind2 <- merge(sd_est_ind,true_est1,  by = c("species", "year",'region','replicate'), all.x = TRUE)
true_ind2$true_cv<-(true_ind2$sd_STRS/true_ind2$value)/10
true_ind3<-na.omit(true_ind2)

#plot true cv     
ggplot()+
  geom_boxplot(data=true_ind3,
               aes(x=scenario,y=true_cv,color=scenario))+
  facet_wrap(~species,scales='free_y')#+

#unique(true_ind3[, .(scn, region)])
true_ind31<-true_ind3

#sampling design table adjustments
setDT(samp_df)
levels(samp_df$region)<-c("EBS","EBS+NBS","EBS+BSS","EBS+NBS+BSS")
# Perform the merge to get data from sampling designs and species
true_ind31 <- merge(true_ind3, samp_df, by.x = c('scenario','region'), by.y = c('scenario','region'), all.x = TRUE)
true_ind31 <- merge(true_ind31,df_spp1,by='species')

# Create a new combined variable
true_ind31[, scn_label := paste0(region, "\n", strat_var)]
true_ind31[, combined_label := factor(
  paste(approach, regime, sep = " - "),
  levels = c('rand - all', 'sb - all', 'rand - cold', 'sb - cold', 'rand - warm', 'sb - warm')
)]

# Compute mean and standard deviation for ploting
df_summary <- true_ind31[, .(mean_value = mean(true_cv, na.rm = TRUE),
                             sd_value = sd(true_cv, na.rm = TRUE)),
                         by = .(region, combined_label, scenario, approach, species, 
                                regime, common, scn_label,strat_var,type)]


#add region1 label '+' with '\n' in 'region'
df_summary[, region1 := gsub("\\+", "\n", region)]

#scale
y_scale<-aggregate((mean_value+sd_value) ~ common, df_summary,max)
y_scale$scale<-y_scale$`(mean_value + sd_value)`+y_scale$`(mean_value + sd_value)`*0.2
y_scale$text<-y_scale$`(mean_value + sd_value)`+y_scale$`(mean_value + sd_value)`*0.1
y_scale$apr<-'sb'
y_scale$scn<-'scn1'
y_scale$year<-2022
y_scale$scn_label<-'EBS\ndepth'
y_scale$region<-'EBS'
y_scale$strat_var<-'depth'
  
#plot true cv cold-warm
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
    values = colors_warm_cold,
    labels = labels_warm_cold,
    name = legend_title_warm_cold
  ) +
  scale_color_manual(
    values = colors_warm_cold,
    labels = labels_warm_cold,
    name = legend_title_warm_cold
  ) +
  scale_shape_manual(
    values = shapes_warm_cold,
    labels = labels_warm_cold,
    name = legend_title_warm_cold
  ) +
  scale_linetype_manual(
    values = linetype_warm_cold,
    labels = labels_warm_cold,
    name = legend_title_warm_cold
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

  
#save warm cold true CV
ragg::agg_png(paste0('./figures slope/true_CV_warmcold.png'), width = 18, height = 12, units = "in", res = 300)
#ragg::agg_png(paste0('./figures/ms_hist_indices_cv_box_EBSNBS_suppl.png'), width = 13, height = 8, units = "in", res = 300)
p
dev.off()
  
#get summary based on static and adaptive
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

#plot true cv static - adaptive
p1 <- ggplot(na.omit(df_summary_clean)) +
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
    values = colors_static_adaptive,
    labels = labels_static_adaptive,
    name = legend_title_static_adaptive
  ) +
  scale_color_manual(
    values = colors_static_adaptive,
    labels = labels_static_adaptive,
    name = legend_title_static_adaptive
  ) +
  scale_shape_manual(
    values = shapes_static_adaptive,
    labels = labels_static_adaptive,
    name = legend_title_static_adaptive
  ) +
  scale_linetype_manual(
    values = linetype_static_adaptive,
    labels = labels_static_adaptive,
    name = legend_title_static_adaptive
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
  
ragg::agg_png(paste0('./figures slope/true_CV.png'), width = 18, height = 12, units = "in", res = 300)
#ragg::agg_png(paste0('./figures/ms_hist_indices_cv_box_EBSNBS_suppl.png'), width = 13, height = 8, units = "in", res = 300)
p1
dev.off()

#2 ESTIMATED CV #####  

#get the mean CVsim across replicates
cv2 <- combined_sim_df[
  ,
  .(
    cv_sim = mean(CV_sim, na.rm = TRUE)
  ),
  by = .(
    species,
    year,
    approach,
    replicate,
    scenario,
    regime,
    region
  )
]
    
#adjust year
cv2$year<-gsub('y','',cv2$year)
cv2$year<-as.numeric(as.character(cv2$year))
true_ind3$year<-as.numeric(true_ind3$year)

#sampling design table adjustments
setDT(samp_df)
levels(samp_df$region)<-c("EBS","EBS+NBS","EBS+BSS","EBS+NBS+BSS")

# Perform the merge sampling design tables and species common name
cv2 <- merge(cv2, samp_df, by= c('scenario','region'), all.x = TRUE)
cv31<-merge(cv2,df_spp1,by='species')

# Convert year to numeric
cv31[, year := as.numeric(year)]
  
# rename
cv31[, scn_label := paste0(region, "\n", strat_var)]
cv31[, value := cv_sim]
  
# Create a new combined variable
cv31[, combined_label := factor(
  paste(approach, regime, sep = " - "),
  levels = c('rand - all', 'sb - all', 'rand - cold', 'sb - cold', 'rand - warm', 'sb - warm')
)]
  
# Compute mean and standard deviation efficiently
df_summary <- cv31[, .(
  mean_value = mean(value, na.rm = TRUE),
  sd_value = sd(value, na.rm = TRUE)
), by = .(region, combined_label, scn_label, approach, species, regime, common, strat_var)]
  
# Replace '+' with '\n' in 'region'
df_summary[, region1 := gsub("\\+", "\n", region)]

#for geom_blank(0 and adjust scale)
#y_scale<-aggregate(q90 ~ common, subset(df_summary, spp %in% sel_spp),max)
y_scale<-aggregate((mean_value+sd_value) ~ common, df_summary,max)
y_scale$scale<-y_scale$`(mean_value + sd_value)`+y_scale$`(mean_value + sd_value)`*0.2
y_scale$text<-y_scale$`(mean_value + sd_value)`+y_scale$`(mean_value + sd_value)`*0.1
y_scale$apr<-'sb'
y_scale$scn<-'scn1'
y_scale$year<-2022
y_scale$region<-'EBS'
y_scale$strat_var<-'depth'
y_scale$scn_label<-'EBS\ndepth'
  
#plot estimated CV
p <-
  ggplot(df_summary) +
  geom_errorbar(
    aes(
      x = interaction(region, strat_var),
      ymin = mean_value-sd_value,
      ymax = mean_value+sd_value,
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
    values = colors_warm_cold,
    labels = labels_warm_cold,
    name = legend_title_warm_cold
  ) +
  scale_color_manual(
    values = colors_warm_cold,
    labels = labels_warm_cold,
    name = legend_title_warm_cold
  ) +
  scale_shape_manual(
    values = shapes_warm_cold,
    labels = labels_warm_cold,
    name = legend_title_warm_cold
  ) +
  scale_linetype_manual(
    values = linetype_warm_cold,
    labels = labels_warm_cold,
    name = legend_title_warm_cold
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
ragg::agg_png(paste0('./figures slope/est_cv_warmcold.png'),  width = 18, height = 12, units = "in", res = 300)
#ragg::agg_png(paste0('./figures/ms_hist_indices_cv_box_EBSNBS_suppl.png'), width = 13, height = 8, units = "in", res = 300)
p
dev.off()
 
#get summary based on static and adaptive
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
  
# PLOT WITH static vs adaptive
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
  labs(y = expression(widehat(CV)), x = "") +
  theme_bw() +
  facet_wrap(~ common, scales = "free_y", nrow = 5, dir = "h") +
  scale_fill_manual(
    values = colors_static_adaptive,
    labels = labels_static_adaptive,
    name = legend_title_static_adaptive
  ) +
  scale_color_manual(
    values = colors_static_adaptive,
    labels = labels_static_adaptive,
    name = legend_title_static_adaptive
  ) +
  scale_shape_manual(
    values = shapes_static_adaptive,
    labels = labels_static_adaptive,
    name = legend_title_static_adaptive
  ) +
  scale_linetype_manual(
    values = linetype_static_adaptive,
    labels = labels_static_adaptive,
    name = legend_title_static_adaptive
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
ragg::agg_png(paste0('./figures slope/est_cv.png'), width = 18, height = 12, units = "in", res = 300)
#ragg::agg_png(paste0('./figures/ms_hist_indices_cv_box_EBSNBS_suppl.png'), width = 13, height = 8, units = "in", res = 300)
p1
dev.off()
  
#3 CALCULATION RRMSE of INDEX and RBIAS OF INDEX ####

#true estimates
#mean true_est
true_est1 <- as.data.table(true_est, keep.rownames = TRUE)
setnames(true_est1, c("species", "year", "region", "replicate", "true_est"))
true_est1<-merge(true_est1,df_spp1,by='species')
true_est1$year<-as.integer(true_est1$year)
true_est1$replicate<-as.integer(true_est1$replicate)
true_est1$dummy<-'true index'

#estimated design estimates
#get the mean abundance estimate across replicates
est_ind <- combined_sim_df[
  ,
  .(
    est_mean = mean(STRS_mean, na.rm = TRUE)
  ),
  by = .(
    species,
    year,
    approach,
    replicate,
    scenario,
    regime,
    region
  )
]

#df as estimated index
est_ind1<-as.data.table(est_ind)   

#merge to spp common
#merge wit spp common name and sampling design
setDT(df_spp)
setDT(samp_df)
levels(samp_df$region)<-c("EBS","EBS+NBS","EBS+BSS","EBS+NBS+BSS")
# merge to add the common name
est_ind1 <- merge(est_ind1, df_spp, by = "species", all.x = TRUE)
est_ind1 <- merge(est_ind1, samp_df[,c("type","region","strat_var","scenario")],by=c('scenario','region'),all.x=TRUE)
#sort approach (station allocation)
est_ind1$approach <- factor(est_ind1$approach, levels = c("sb", "rand"))
est_ind1$year<-as.integer(est_ind1$year)

#to adjust units
est_ind1$est_mean<-est_ind1$est_mean/10

#merge estimated index and true index
rrmse_ind1<-merge(est_ind1,true_est1,by=c('species','region','year','replicate','common'))

# Compute relative bias
rrmse_ind1[, rel_bias := 100*((est_mean - true_est) / true_est)]
rrmse_ind1[, rel_log_bias := log10(est_mean / true_est) ]

# Compute mean relative bias and RRMSE by spp, year, and scenario
rel_bias_rrmse_dt <- rrmse_ind1[, .(
  rel_bias = mean(rel_bias, na.rm = TRUE),
  rel_log_bias = mean(rel_bias, na.rm = TRUE),
  rrmse = sqrt(mean((est_mean - true_est)^2, na.rm = TRUE)) / mean(true_est, na.rm = TRUE)
), by = .(species, year, scenario, approach, regime)]

# Keep only necessary simulations (e.g., sim 1) or average across sims if desired
df_rb_rrmse <- rel_bias_rrmse_dt[, .(
  mean_rel_bias = mean(rel_bias, na.rm = TRUE),
  sd_rel_bias   = sd(rel_bias, na.rm = TRUE),
  mean_rrmse    = mean(rrmse, na.rm = TRUE),
  sd_rrmse      = sd(rrmse, na.rm = TRUE)
), by = .(species, scenario, approach)]

# Join with scenario-level metadata if needed
# Assuming samp_df contains mapping of scn to region, strat_var, approach, regime1, etc.
df_rb_rrmse1<-merge(df_rb_rrmse,samp_df,by='scenario')

# Replace '+' with '\n' in 'region'
df_rb_rrmse1[, region1 := gsub("\\+", "\n", region)]

#rename label
df_rb_rrmse1$regime<-ifelse(df_rb_rrmse1$type=='static','all','dyn')
df_rb_rrmse1$regime1<-ifelse(df_rb_rrmse1$regime=='all','all','dyn')
df_rb_rrmse1$combined_label1<-paste0(df_rb_rrmse1$approach," - ",df_rb_rrmse1$regime1)
df_rb_rrmse1$combined_label1<-factor(df_rb_rrmse1$combined_label1,c("rand - all",'sb - all',"rand - dyn",'sb - dyn'))
df_rb_rrmse1<-merge(df_rb_rrmse1,df_spp,by='species')

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


#plot rrmse of index static-adaptive
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
    values = colors_static_adaptive,
    labels = labels_static_adaptive,
    name = legend_title_static_adaptive
  ) +
  
  scale_color_manual(
    values = colors_static_adaptive,
    labels = labels_static_adaptive,
    name = legend_title_static_adaptive
  ) +
  
  scale_shape_manual(
    values = shapes_static_adaptive,
    labels = labels_static_adaptive,
    name = legend_title_static_adaptive
  ) +
  
  scale_linetype_manual(
    values = linetype_static_adaptive,
    labels = labels_static_adaptive,
    name = legend_title_static_adaptive
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

#save plot
ragg::agg_png(paste0('./figures slope/RRMSE_index.png'), width = 18, height = 12, units = "in", res = 300)
p
dev.off()
  
# Keep only necessary simulations (e.g., sim 1) or average across sims if desired
df_rb_rrmse2 <- rel_bias_rrmse_dt[, .(
  mean_rel_bias = mean(rel_bias, na.rm = TRUE),
  sd_rel_bias   = sd(rel_bias, na.rm = TRUE),
  mean_rrmse    = mean(rrmse, na.rm = TRUE),
  sd_rrmse      = sd(rrmse, na.rm = TRUE)
), by = .(species, scenario, approach,regime)]
  
# Join with scenario-level metadata if needed
# Assuming samp_df contains mapping of scn to region, strat_var, approach, regime1, etc.
df_rb_rrmse2<-merge(df_rb_rrmse2,samp_df,by=c('scenario'))
# Replace '+' with '\n' in 'region'
df_rb_rrmse2[, region1 := gsub("\\+", "\n", region)]
  
#df_rb_rrmse2$regime<-ifelse(df_rb_rrmse2$type=='static','all','dyn')
df_rb_rrmse2$regime1<-ifelse(df_rb_rrmse2$regime=='all','all','dyn')
df_rb_rrmse2$combined_label<-paste0(df_rb_rrmse2$approach," - ",df_rb_rrmse2$regime)
df_rb_rrmse2$combined_label1<-paste0(df_rb_rrmse2$approach," - ",df_rb_rrmse2$regime1)
df_rb_rrmse2$combined_label1<-factor(df_rb_rrmse2$combined_label1,c("rand - all",'sb - all',"rand - dyn",'sb - dyn'))
df_rb_rrmse2<-merge(df_rb_rrmse2,df_spp,by='species')

#remove infinitive values
df_rb_rrmse2 <- df_rb_rrmse2[
  is.finite(mean_rrmse)
]

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

#scale
y_scale <- aggregate((mean_rrmse + sd_rrmse) ~ common, df_rb_rrmse2, max)
y_scale$scale <- y_scale$`(mean_rrmse + sd_rrmse)` * 1.1
y_scale$text  <- y_scale$`(mean_rrmse + sd_rrmse)` * 1.1
y_scale$apr       <- "sb"
y_scale$scn       <- "scn1"
y_scale$year      <- 2022
y_scale$region    <- "EBS"
y_scale$strat_var <- "depth"

#plot rrmse index warm-cold
p <- 
  ggplot(na.omit(df_rb_rrmse2)) +
  geom_errorbar(
    aes(x = interaction(region, strat_var),
        ymin = mean_rrmse - sd_rrmse,
        ymax = mean_rrmse + sd_rrmse,
        color = combined_label,
        group = interaction(scenario, approach, spp, regime)),
    width = 0.3,
    position = position_dodge(width = 0.9),
    size = 1,
    alpha = 0.8
  ) +
  geom_point(
    aes(x = interaction(region, strat_var),
        y = mean_rrmse,
        fill = combined_label,
        group = interaction(scenario, approach, spp, regime),
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
    values = colors_warm_cold,
    labels = labels_warm_cold,
    name = legend_title_warm_cold
  ) +
  scale_color_manual(
    values = colors_warm_cold,
    labels = labels_warm_cold,
    name = legend_title_warm_cold
  ) +
  scale_shape_manual(
    values = shapes_warm_cold,
    labels = labels_warm_cold,
    name = legend_title_warm_cold
  ) +
  scale_linetype_manual(
    values = linetype_warm_cold,
    labels = labels_warm_cold,
    name = legend_title_warm_cold
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
  geom_blank(
    data = y_scale,
    aes(
      x = interaction(region, strat_var),
      y = scale
    )
  )+
  coord_cartesian(clip = "off")
  

ragg::agg_png(paste0('./figures slope/RRMSE_index_warmcold.png'), width = 18, height = 12, units = "in", res = 300)
#ragg::agg_png(paste0('./figures/ms_hist_indices_cv_box_EBSNBS_suppl.png'), width = 13, height = 8, units = "in", res = 300)
p
dev.off()
  
#3.2 PLOT RBIAS OF INDEX  #####

#for geom_blank(0 and adjust scale)
# y-scale and artificial rows for geom_blank and facet labels
y_scale <- aggregate((mean_rel_bias + sd_rel_bias) ~ common,
                     df_rb_rrmse1, max)
y_scale$scale <- ifelse(
  y_scale$`(mean_rel_bias + sd_rel_bias)` >= 0,
  y_scale$`(mean_rel_bias + sd_rel_bias)` * 1.10,
  y_scale$`(mean_rel_bias + sd_rel_bias)` * 0.90
)
y_scale$text <- y_scale$scale
# manual override
y_scale[y_scale$common == "Shortspine thornyhead", "text"] <- 20
# artificial columns so geom_blank has valid x aesthetics
y_scale$apr       <- "sb"
y_scale$scn       <- "scn1"
y_scale$year      <- 2022
y_scale$region    <- "EBS"
y_scale$strat_var <- "depth"

#plot rbias index static-adaptive
p <-
  ggplot() +
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
    values = colors_static_adaptive,
    labels = labels_static_adaptive,
    name = legend_title_static_adaptive
  ) +
  scale_color_manual(
    values = colors_static_adaptive,
    labels = labels_static_adaptive,
    name = legend_title_static_adaptive
  ) +
  scale_shape_manual(
    values = shapes_static_adaptive,
    labels = labels_static_adaptive,
    name = legend_title_static_adaptive
  ) +
  scale_linetype_manual(
    values = linetype_static_adaptive,
    labels = labels_static_adaptive,
    name = legend_title_static_adaptive
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

# save
ragg::agg_png(
  './figures slope/RBIAS_index.png',
  width = 18, height = 12, units = "in", res = 300
)
p
dev.off()

#scale text  
y_scale <- aggregate((mean_rel_bias + sd_rel_bias) ~ common,
                     df_rb_rrmse2, max)
y_scale$scale <- ifelse(
  y_scale$`(mean_rel_bias + sd_rel_bias)` >= 0,
  y_scale$`(mean_rel_bias + sd_rel_bias)` * 1.10,
  y_scale$`(mean_rel_bias + sd_rel_bias)` * 0.90
)
y_scale$text <- y_scale$scale
# manual override
y_scale[y_scale$common == "Shortspine thornyhead", "text"] <- 20
# artificial columns so geom_blank has valid x aesthetics
y_scale$apr       <- "sb"
y_scale$scn       <- "scn1"
y_scale$year      <- 2022
y_scale$region    <- "EBS"
y_scale$strat_var <- "depth"

#plot rbias warm-cold 
p<-
  ggplot(na.omit(df_rb_rrmse2)) +
  #geom_hline(yintercept = 0, alpha = 0.5, linetype = 'dotted') +
  geom_errorbar(aes(x = interaction(region,strat_var), ymin = mean_rel_bias - sd_rel_bias, ymax = mean_rel_bias + sd_rel_bias, color = combined_label,
                    group = interaction(scenario, approach, spp, regime)),
                width = 0.3, position = position_dodge(width = 0.9),size=1,alpha=0.8) + 
  geom_point(aes(x = interaction(region,strat_var), y = mean_rel_bias, fill = combined_label, 
                 group = interaction(scenario, approach, spp, regime), 
                 shape = combined_label), 
             size = 2, position = position_dodge(width = 0.9), color = "black") + 
  labs(y = 'RBIAS of abundance estimates (%)', x = '') +
  theme_bw() + 
  facet_wrap(~common, scales = 'free_y', nrow = 5, dir = 'h') +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.01)) +
  scale_fill_manual(
    values = colors_warm_cold,
    labels = labels_warm_cold,
    name = legend_title_warm_cold
  ) +
  scale_color_manual(
    values = colors_warm_cold,
    labels = labels_warm_cold,
    name = legend_title_warm_cold
  ) +
  scale_shape_manual(
    values = shapes_warm_cold,
    labels = labels_warm_cold,
    name = legend_title_warm_cold
  ) +
  scale_linetype_manual(
    values = linetype_warm_cold,
    labels = labels_warm_cold,
    name = legend_title_warm_cold
  ) +
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


ragg::agg_png(paste0('./figures slope/RBIAS_index_warmcold.png'), width = 18, height = 12, units = "in", res = 300)
#ragg::agg_png(paste0('./figures/ms_hist_indices_cv_box_EBSNBS_suppl.png'), width = 13, height = 8, units = "in", res = 300)
p
dev.off()

#3.3 PLOT RRMSE OF INDEX TOTAL #####

# Get true index of EBS+NBS+BSS
true_est_whole<-subset(x = true_est1,region=='EBS+NBS+BSS')

#estimated design estimates
#get the mean abundance estimate across replicates
est_ind <- combined_sim_df[
  ,
  .(
    est_mean = mean(STRS_mean, na.rm = TRUE)
  ),
  by = .(
    species,
    year,
    approach,
    replicate,
    scenario,
    regime,
    region
  )
]

#df as estimated index
est_ind1<-as.data.table(est_ind)   

#merge to spp common
#merge wit spp common name and sampling design
setDT(df_spp)
setDT(samp_df)
levels(samp_df$region)<-c("EBS","EBS+NBS","EBS+BSS","EBS+NBS+BSS")
# merge to add the common name
est_ind1 <- merge(est_ind1, df_spp, by = "species", all.x = TRUE)
est_ind1 <- merge(est_ind1, samp_df[,c("type","region","strat_var","scenario")],by=c('scenario','region'),all.x=TRUE)
#sort approach (station allocation)
est_ind1$approach <- factor(est_ind1$approach, levels = c("sb", "rand"))
est_ind1$year<-as.integer(est_ind1$year)

#to adjust units
est_ind1$est_mean<-est_ind1$est_mean/10

#merge estimated index and true index
rrmse_ind1<-merge(est_ind1,true_est_whole,by=c('species','year','replicate','common'))

# Compute relative bias
rrmse_ind1[, rel_bias := 100*((est_mean - true_est) / true_est)]
rrmse_ind1[, rel_log_bias := log10(est_mean / true_est) ]

# Compute mean relative bias and RRMSE by spp, year, and scenario
rel_bias_rrmse_dt <- rrmse_ind1[, .(
  rel_bias = mean(rel_bias, na.rm = TRUE),
  rel_log_bias = mean(rel_bias, na.rm = TRUE),
  rrmse = sqrt(mean((est_mean - true_est)^2, na.rm = TRUE)) / mean(true_est, na.rm = TRUE)
), by = .(species, year, scenario, approach, regime)]

# Keep only necessary simulations (e.g., sim 1) or average across sims if desired
df_rb_rrmse <- rel_bias_rrmse_dt[, .(
  mean_rel_bias = mean(rel_bias, na.rm = TRUE),
  sd_rel_bias   = sd(rel_bias, na.rm = TRUE),
  mean_rrmse    = mean(rrmse, na.rm = TRUE),
  sd_rrmse      = sd(rrmse, na.rm = TRUE)
), by = .(species, scenario, approach)]

# Join with scenario-level metadata if needed
# Assuming samp_df contains mapping of scn to region, strat_var, approach, regime1, etc.
df_rb_rrmse1<-merge(df_rb_rrmse,samp_df,by='scenario')

# Replace '+' with '\n' in 'region'
df_rb_rrmse1[, region1 := gsub("\\+", "\n", region)]

#rename label
df_rb_rrmse1$regime<-ifelse(df_rb_rrmse1$type=='static','all','dyn')
df_rb_rrmse1$regime1<-ifelse(df_rb_rrmse1$regime=='all','all','dyn')
df_rb_rrmse1$combined_label1<-paste0(df_rb_rrmse1$approach," - ",df_rb_rrmse1$regime1)
df_rb_rrmse1$combined_label1<-factor(df_rb_rrmse1$combined_label1,c("rand - all",'sb - all',"rand - dyn",'sb - dyn'))
df_rb_rrmse1<-merge(df_rb_rrmse1,df_spp,by='species')

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

#plot rrmse of index static-adaptive
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
    values = colors_static_adaptive,
    labels = labels_static_adaptive,
    name = legend_title_static_adaptive
  ) +
  
  scale_color_manual(
    values = colors_static_adaptive,
    labels = labels_static_adaptive,
    name = legend_title_static_adaptive
  ) +
  
  scale_shape_manual(
    values = shapes_static_adaptive,
    labels = labels_static_adaptive,
    name = legend_title_static_adaptive
  ) +
  
  scale_linetype_manual(
    values = linetype_static_adaptive,
    labels = labels_static_adaptive,
    name = legend_title_static_adaptive
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

#save plot
ragg::agg_png(paste0('./figures slope/RRMSE_index_total.png'), width = 18, height = 12, units = "in", res = 300)
p
dev.off()

# Keep only necessary simulations (e.g., sim 1) or average across sims if desired
df_rb_rrmse2 <- rel_bias_rrmse_dt[, .(
  mean_rel_bias = mean(rel_bias, na.rm = TRUE),
  sd_rel_bias   = sd(rel_bias, na.rm = TRUE),
  mean_rrmse    = mean(rrmse, na.rm = TRUE),
  sd_rrmse      = sd(rrmse, na.rm = TRUE)
), by = .(species, scenario, approach,regime)]

# Join with scenario-level metadata if needed
# Assuming samp_df contains mapping of scn to region, strat_var, approach, regime1, etc.
df_rb_rrmse2<-merge(df_rb_rrmse2,samp_df,by=c('scenario'))
# Replace '+' with '\n' in 'region'
df_rb_rrmse2[, region1 := gsub("\\+", "\n", region)]

#df_rb_rrmse2$regime<-ifelse(df_rb_rrmse2$type=='static','all','dyn')
df_rb_rrmse2$regime1<-ifelse(df_rb_rrmse2$regime=='all','all','dyn')
df_rb_rrmse2$combined_label<-paste0(df_rb_rrmse2$approach," - ",df_rb_rrmse2$regime)
df_rb_rrmse2$combined_label1<-paste0(df_rb_rrmse2$approach," - ",df_rb_rrmse2$regime1)
df_rb_rrmse2$combined_label1<-factor(df_rb_rrmse2$combined_label1,c("rand - all",'sb - all',"rand - dyn",'sb - dyn'))
df_rb_rrmse2<-merge(df_rb_rrmse2,df_spp,by='species')

#remove infinitive values
df_rb_rrmse2 <- df_rb_rrmse2[
  is.finite(mean_rrmse)
]

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

#scale
y_scale <- aggregate((mean_rrmse + sd_rrmse) ~ common, df_rb_rrmse2, max)
y_scale$scale <- y_scale$`(mean_rrmse + sd_rrmse)` * 1.1
y_scale$text  <- y_scale$`(mean_rrmse + sd_rrmse)` * 1.1
y_scale$apr       <- "sb"
y_scale$scn       <- "scn1"
y_scale$year      <- 2022
y_scale$region    <- "EBS"
y_scale$strat_var <- "depth"

#plot rrmse index warm-cold
p <- 
  ggplot(na.omit(df_rb_rrmse2)) +
  geom_errorbar(
    aes(x = interaction(region, strat_var),
        ymin = mean_rrmse - sd_rrmse,
        ymax = mean_rrmse + sd_rrmse,
        color = combined_label,
        group = interaction(scenario, approach, spp, regime)),
    width = 0.3,
    position = position_dodge(width = 0.9),
    size = 1,
    alpha = 0.8
  ) +
  geom_point(
    aes(x = interaction(region, strat_var),
        y = mean_rrmse,
        fill = combined_label,
        group = interaction(scenario, approach, spp, regime),
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
    values = colors_warm_cold,
    labels = labels_warm_cold,
    name = legend_title_warm_cold
  ) +
  scale_color_manual(
    values = colors_warm_cold,
    labels = labels_warm_cold,
    name = legend_title_warm_cold
  ) +
  scale_shape_manual(
    values = shapes_warm_cold,
    labels = labels_warm_cold,
    name = legend_title_warm_cold
  ) +
  scale_linetype_manual(
    values = linetype_warm_cold,
    labels = labels_warm_cold,
    name = legend_title_warm_cold
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
  geom_blank(
    data = y_scale,
    aes(
      x = interaction(region, strat_var),
      y = scale
    )
  )+
  coord_cartesian(clip = "off")


ragg::agg_png(paste0('./figures slope/RRMSE_index_warmcold_total.png'), width = 18, height = 12, units = "in", res = 300)
#ragg::agg_png(paste0('./figures/ms_hist_indices_cv_box_EBSNBS_suppl.png'), width = 13, height = 8, units = "in", res = 300)
p
dev.off()


#4 CALCULATION RRMSE AND RBIAS OF CV  #####
#cv_true
#get the mean STRS_var across replicates
sd_est_ind <- combined_sim_df[
  ,
  .(
    sd_STRS = mean(sqrt(STRS_var), na.rm = TRUE)
  ),
  by = .(
    species,
    year,
    approach,
    replicate,
    scenario,
    regime,
    region
  )
]
sd_est_ind$year<-as.integer(sd_est_ind$year)

#mean true_est
true_est1 <- as.data.table(true_est, keep.rownames = TRUE)
setnames(true_est1, c("species", "year", "region", "replicate", "true_est"))
true_est1 <- true_est1[
  , .(true_est_mean = mean(true_est, na.rm = TRUE)),
  by = .(species, year, region,replicate)
]

true_est1$year<-as.integer(true_est1$year)
true_est1$dummy<-'true index'
true_est1$value<-true_est1$true_est
true_est1$replicate <- as.integer(true_est1$replicate)

# Merge
true_ind2 <- merge(sd_est_ind,true_est1,  by = c("species", "year",'region','replicate'), all.x = TRUE)
true_ind2$true_cv<-(true_ind2$sd_STRS/true_ind2$value)/10
true_ind3<-na.omit(true_ind2)

#unique(true_ind3[, .(scn, region)])
true_ind31<-true_ind3

#sampling design table adjustments
setDT(samp_df)
levels(samp_df$region)<-c("EBS","EBS+NBS","EBS+BSS","EBS+NBS+BSS")
# Perform the merge to get data from sampling designs and species
true_ind31 <- merge(true_ind3, samp_df, by.x = c('scenario','region'), by.y = c('scenario','region'), all.x = TRUE)
true_ind31 <- merge(true_ind31,df_spp1,by='species')

# Create a new combined variable
true_ind31[, scn_label := paste0(region, "\n", strat_var)]
true_ind31[, combined_label := factor(
  paste(approach, regime, sep = " - "),
  levels = c('rand - all', 'sb - all', 'rand - cold', 'sb - cold', 'rand - warm', 'sb - warm')
)]

#cv_sim
#get the mean CVsim across replicates
cv2 <- combined_sim_df[
  ,
  .(
    cv_sim = mean(CV_sim, na.rm = TRUE)
  ),
  by = .(
    species,
    year,
    approach,
    replicate,
    scenario,
    regime,
    region
  )
]

#adjust year
cv2$year<-gsub('y','',cv2$year)
cv2$year<-as.numeric(as.character(cv2$year))
true_ind3$year<-as.numeric(true_ind3$year)

#sampling design table adjustments
setDT(samp_df)
levels(samp_df$region)<-c("EBS","EBS+NBS","EBS+BSS","EBS+NBS+BSS")

# Perform the merge sampling design tables and species common name
cv2 <- merge(cv2, samp_df, by= c('scenario','region'), all.x = TRUE)
cv31<-merge(cv2,df_spp1,by='species')

# Convert year to numeric
cv31[, year := as.numeric(year)]

# rename
cv31[, scn_label := paste0(region, "\n", strat_var)]

common_cols <- intersect(names(cv31), names(true_ind31))

rrmse_cv <- merge(
  cv31,
  true_ind31,
  by = common_cols,
  all.x = TRUE
)

# Calculate relative bias and squared error
rrmse_cv[, rel_bias := 100*((cv_sim - true_cv) / true_cv)]
rrmse_cv[, sqrtdiff := (cv_sim - true_cv)^2]

# Calculate RRMSE
rrmse_cv[, rrmse := sqrt(sqrtdiff) / true_cv]

# Keep only necessary simulations (e.g., sim 1) or average across sims if desired
df_rb_rrmse <- rrmse_cv[, .(
  mean_rel_bias = mean(rel_bias, na.rm = TRUE),
  sd_rel_bias   = sd(rel_bias, na.rm = TRUE),
  mean_rrmse    = mean(rrmse, na.rm = TRUE),
  sd_rrmse      = sd(rrmse, na.rm = TRUE)
), by = .(species, scenario, approach)]

# Join with scenario-level metadata if needed
# Assuming samp_df contains mapping of scn to region, strat_var, approach, regime1, etc.
df_rb_rrmse1<-merge(df_rb_rrmse,samp_df,by='scenario')

# Replace '+' with '\n' in 'region'
df_rb_rrmse1[, region1 := gsub("\\+", "\n", region)]

#rename label
df_rb_rrmse1$regime<-ifelse(df_rb_rrmse1$type=='static','all','dyn')
df_rb_rrmse1$regime1<-ifelse(df_rb_rrmse1$regime=='all','all','dyn')
df_rb_rrmse1$combined_label1<-paste0(df_rb_rrmse1$approach," - ",df_rb_rrmse1$regime1)
df_rb_rrmse1$combined_label1<-factor(df_rb_rrmse1$combined_label1,c("rand - all",'sb - all',"rand - dyn",'sb - dyn'))
df_rb_rrmse1<-merge(df_rb_rrmse1,df_spp,by='species')

#4.1 PLOT RRMSE OF CV  #####

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

#plot rrmse of cv static-adaptive
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
  labs(y = "RRMSE of CV", x = "") +
  theme_bw() +
  facet_wrap(~ common, scales = "free_y", nrow = 5, dir = "h") +
  scale_fill_manual(
    values = colors_static_adaptive,
    labels = labels_static_adaptive,
    name = legend_title_static_adaptive
  ) +
  
  scale_color_manual(
    values = colors_static_adaptive,
    labels = labels_static_adaptive,
    name = legend_title_static_adaptive
  ) +
  
  scale_shape_manual(
    values = shapes_static_adaptive,
    labels = labels_static_adaptive,
    name = legend_title_static_adaptive
  ) +
  
  scale_linetype_manual(
    values = linetype_static_adaptive,
    labels = labels_static_adaptive,
    name = legend_title_static_adaptive
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

#save plot
ragg::agg_png(paste0('./figures slope/RRMSE_cv.png'), width = 18, height = 12, units = "in", res = 300)
p
dev.off()

# Keep only necessary simulations (e.g., sim 1) or average across sims if desired
df_rb_rrmse2 <- rel_bias_rrmse_dt[, .(
  mean_rel_bias = mean(rel_bias, na.rm = TRUE),
  sd_rel_bias   = sd(rel_bias, na.rm = TRUE),
  mean_rrmse    = mean(rrmse, na.rm = TRUE),
  sd_rrmse      = sd(rrmse, na.rm = TRUE)
), by = .(species, scenario, approach,regime)]

# Join with scenario-level metadata if needed
# Assuming samp_df contains mapping of scn to region, strat_var, approach, regime1, etc.
df_rb_rrmse2<-merge(df_rb_rrmse2,samp_df,by=c('scenario'))
# Replace '+' with '\n' in 'region'
df_rb_rrmse2[, region1 := gsub("\\+", "\n", region)]

#df_rb_rrmse2$regime<-ifelse(df_rb_rrmse2$type=='static','all','dyn')
df_rb_rrmse2$regime1<-ifelse(df_rb_rrmse2$regime=='all','all','dyn')
df_rb_rrmse2$combined_label<-paste0(df_rb_rrmse2$approach," - ",df_rb_rrmse2$regime)
df_rb_rrmse2$combined_label1<-paste0(df_rb_rrmse2$approach," - ",df_rb_rrmse2$regime1)
df_rb_rrmse2$combined_label1<-factor(df_rb_rrmse2$combined_label1,c("rand - all",'sb - all',"rand - dyn",'sb - dyn'))
df_rb_rrmse2<-merge(df_rb_rrmse2,df_spp,by='species')

#remove infinitive values
df_rb_rrmse2 <- df_rb_rrmse2[
  is.finite(mean_rrmse)
]

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

#scale
y_scale <- aggregate((mean_rrmse + sd_rrmse) ~ common, df_rb_rrmse2, max)
y_scale$scale <- y_scale$`(mean_rrmse + sd_rrmse)` * 1.1
y_scale$text  <- y_scale$`(mean_rrmse + sd_rrmse)` * 1.1
y_scale$apr       <- "sb"
y_scale$scn       <- "scn1"
y_scale$year      <- 2022
y_scale$region    <- "EBS"
y_scale$strat_var <- "depth"

#plot rrmse cv warm-cold
p <- 
  ggplot(na.omit(df_rb_rrmse2)) +
  geom_errorbar(
    aes(x = interaction(region, strat_var),
        ymin = mean_rrmse - sd_rrmse,
        ymax = mean_rrmse + sd_rrmse,
        color = combined_label,
        group = interaction(scenario, approach, spp, regime)),
    width = 0.3,
    position = position_dodge(width = 0.9),
    size = 1,
    alpha = 0.8
  ) +
  geom_point(
    aes(x = interaction(region, strat_var),
        y = mean_rrmse,
        fill = combined_label,
        group = interaction(scenario, approach, spp, regime),
        shape = combined_label),
    size = 2,
    position = position_dodge(width = 0.9),
    color = "black"
  ) +
  labs(y = "RRMSE of CV", x = "") +
  theme_bw() +
  facet_wrap(~ common, scales = "free_y", nrow = 5, dir = "h") +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.01)) +
  scale_fill_manual(
    values = colors_warm_cold,
    labels = labels_warm_cold,
    name = legend_title_warm_cold
  ) +
  scale_color_manual(
    values = colors_warm_cold,
    labels = labels_warm_cold,
    name = legend_title_warm_cold
  ) +
  scale_shape_manual(
    values = shapes_warm_cold,
    labels = labels_warm_cold,
    name = legend_title_warm_cold
  ) +
  scale_linetype_manual(
    values = linetype_warm_cold,
    labels = labels_warm_cold,
    name = legend_title_warm_cold
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
  geom_blank(
    data = y_scale,
    aes(
      x = interaction(region, strat_var),
      y = scale
    )
  )+
  coord_cartesian(clip = "off")

ragg::agg_png(paste0('./figures slope/RRMSE_cv_warmcold.png'), width = 18, height = 12, units = "in", res = 300)
#ragg::agg_png(paste0('./figures/ms_hist_indices_cv_box_EBSNBS_suppl.png'), width = 13, height = 8, units = "in", res = 300)
p
dev.off()


#4.1 PLOT RBIAS OF CV ####
#for geom_blank(0 and adjust scale)
# y-scale and artificial rows for geom_blank and facet labels
y_scale <- aggregate((mean_rel_bias + sd_rel_bias) ~ common,
                     df_rb_rrmse1, max)
y_scale$scale <- ifelse(
  y_scale$`(mean_rel_bias + sd_rel_bias)` >= 0,
  y_scale$`(mean_rel_bias + sd_rel_bias)` * 1.10,
  y_scale$`(mean_rel_bias + sd_rel_bias)` * 0.90
)
y_scale$text <- y_scale$scale
# manual override
y_scale[y_scale$common == "Shortspine thornyhead", "text"] <- 20
# artificial columns so geom_blank has valid x aesthetics
y_scale$apr       <- "sb"
y_scale$scn       <- "scn1"
y_scale$year      <- 2022
y_scale$region    <- "EBS"
y_scale$strat_var <- "depth"


#plot rbias index static-adaptive
p <-
  ggplot() +
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
  labs(y = "RBIAS of CV (%)", x = "") +
  theme_bw() +
  facet_wrap(~ common, scales = "free_y", nrow = 5, dir = "h") +
  scale_fill_manual(
    values = colors_static_adaptive,
    labels = labels_static_adaptive,
    name = legend_title_static_adaptive
  ) +
  
  scale_color_manual(
    values = colors_static_adaptive,
    labels = labels_static_adaptive,
    name = legend_title_static_adaptive
  ) +
  
  scale_shape_manual(
    values = shapes_static_adaptive,
    labels = labels_static_adaptive,
    name = legend_title_static_adaptive
  ) +
  
  scale_linetype_manual(
    values = linetype_static_adaptive,
    labels = labels_static_adaptive,
    name = legend_title_static_adaptive
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

# save
ragg::agg_png(
  './figures slope/RBIAS_cv.png',
  width = 18, height = 12, units = "in", res = 300
)
p
dev.off()

#scale for plotting
y_scale <- aggregate((mean_rel_bias + sd_rel_bias) ~ common,
                     df_rb_rrmse2, max)
y_scale$scale <- ifelse(
  y_scale$`(mean_rel_bias + sd_rel_bias)` >= 0,
  y_scale$`(mean_rel_bias + sd_rel_bias)` * 1.10,
  y_scale$`(mean_rel_bias + sd_rel_bias)` * 0.90
)
y_scale$text <- y_scale$scale
# manual override
y_scale[y_scale$common == "Shortspine thornyhead", "text"] <- 20
# artificial columns so geom_blank has valid x aesthetics
y_scale$apr       <- "sb"
y_scale$scn       <- "scn1"
y_scale$year      <- 2022
y_scale$region    <- "EBS"
y_scale$strat_var <- "depth"

#plot rbias warm-cold 
p<-
  ggplot(na.omit(df_rb_rrmse2)) +
  #geom_hline(yintercept = 0, alpha = 0.5, linetype = 'dotted') +
  geom_errorbar(aes(x = interaction(region,strat_var), ymin = mean_rel_bias - sd_rel_bias, ymax = mean_rel_bias + sd_rel_bias, color = combined_label,
                    group = interaction(scenario, approach, spp, regime)),
                width = 0.3, position = position_dodge(width = 0.9),size=1,alpha=0.8) + 
  geom_point(aes(x = interaction(region,strat_var), y = mean_rel_bias, fill = combined_label, 
                 group = interaction(scenario, approach, spp, regime), 
                 shape = combined_label), 
             size = 2, position = position_dodge(width = 0.9), color = "black") + 
  labs(y = 'RBIAS of CV (%)', x = '') +
  theme_bw() + 
  facet_wrap(~common, scales = 'free_y', nrow = 5, dir = 'h') +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.01)) +
  scale_fill_manual(
    values = colors_warm_cold,
    labels = labels_warm_cold,
    name = legend_title_warm_cold
  ) +
  scale_color_manual(
    values = colors_warm_cold,
    labels = labels_warm_cold,
    name = legend_title_warm_cold
  ) +
  scale_shape_manual(
    values = shapes_warm_cold,
    labels = labels_warm_cold,
    name = legend_title_warm_cold
  ) +
  scale_linetype_manual(
    values = linetype_warm_cold,
    labels = labels_warm_cold,
    name = legend_title_warm_cold
  ) +
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


ragg::agg_png(paste0('./figures slope/RBIAS_cv_warmcold.png'), width = 18, height = 12, units = "in", res = 300)
#ragg::agg_png(paste0('./figures/ms_hist_indices_cv_box_EBSNBS_suppl.png'), width = 13, height = 8, units = "in", res = 300)
p
dev.off()

#5 CV ratio ####
#LOAD
#get the mean CVsim across replicates
cv2 <- combined_sim_df[
  ,
  .(
    cv_sim = mean(CV_sim, na.rm = TRUE)
  ),
  by = .(
    species,
    year,
    approach,
    replicate,
    scenario,
    regime,
    region
  )
]

#adjust year
cv2$year<-gsub('y','',cv2$year)
cv2$year<-as.numeric(as.character(cv2$year))

#cv EBS
cv_ebs<-subset(cv2,region=='EBS')
cv_ebs <- cv_ebs[
  ,
  .(
    cv_ebs = mean(cv_sim, na.rm = TRUE)
  ),
  by = .(
    species,
    year,
    approach,
    replicate,
    #scenario,
    regime
  )
]
#sampling design table adjustments
setDT(samp_df)
levels(samp_df$region)<-c("EBS","EBS+NBS","EBS+BSS","EBS+NBS+BSS")

# Perform the merge sampling design tables and species common name
cv2 <- merge(cv2, cv_ebs, by= c('species','year','approach','replicate','regime'), all.x = TRUE)
cv2 <- merge(cv2, samp_df, by= c('scenario','region'), all.x = TRUE)
cv31<-merge(cv2,df_spp1,by='species')

# Convert year to numeric
cv31[, year := as.numeric(year)]

# rename
cv31[, scn_label := paste0(region, "\n", strat_var)]
cv31[, value := cv_sim]

# Create a new combined variable
cv31[, combined_label := factor(
  paste(approach, regime, sep = " - "),
  levels = c('rand - all', 'sb - all', 'rand - cold', 'sb - cold', 'rand - warm', 'sb - warm')
)]

#LOG CV RATIO
cv31$ratio<-log(cv31$cv_sim/cv31$cv_ebs)

# Compute mean and standard deviation while keeping 'common' and 'strat_var'
df_summary <- cv31[, .(
  mean_ratio = mean(ratio, na.rm = TRUE),
  q90 = quantile(ratio,probs=0.90, na.rm = TRUE),
  q10 = quantile(ratio,probs=0.10, na.rm = TRUE)
), by = .(region, approach, species, scenario, regime, strat_var,common,combined_label)]
#unique(df_summary[, .(scn, region)])

df_summary$region1<-gsub('+','\n',df_summary$region)
#label scn
df_summary$scn_label<-paste0(df_summary$region,'\n',df_summary$strat_var)
#df_summary$mean_sd<-df_summary$mean_ratio+df_summary$sd_ratio
# Create a new combined variable in your dat
df_summary$combined_label <- paste(df_summary$approach, df_summary$regime, sep = " - ")
# #sort factors just in case
df_summary$combined_label<-factor(df_summary$combined_label,levels=c('rand - all' ,
                                                       'sb - all' ,
                                                       'rand - cold' ,
                                                       'sb - cold' ,
                                                       'rand - warm' ,
                                                       'sb - warm' ) )
#remove EBS 
df_summary<-subset(df_summary,df_summary$region!='EBS')

#for geom_blank(0 and adjust scale)
y_scale <- aggregate((mean_ratio + q90) ~ common, df_summary, max)
y_scale$scale <- y_scale$`(mean_ratio + q90)` * 1.10
y_scale$text  <- y_scale$`(mean_ratio + q90)` * 1.10
y_scale[which((y_scale$common=='Shortspine thornyhead')),'text']<-20
# artificial columns so geom_blank has valid x aesthetics
y_scale$apr       <- "sb"
y_scale$scn       <- "scn1"
y_scale$year      <- 2022
y_scale$region    <- "EBS+BSS"
y_scale$strat_var <- "depth"

# enforce fixed order
df_summary$combined_label <- factor(
  df_summary$combined_label,
  levels = c(
    "rand - all",
    "sb - all",
    "rand - cold",
    "sb - cold",
    "rand - warm",
    "sb - warm"
  )
)

#plot warm/cold
p<-
  ggplot(df_summary) +
    geom_hline(yintercept = 0,linetype='dashed')+
  geom_errorbar(aes(x = interaction(region,strat_var), ymin = q10, ymax = q90,  
                    color = combined_label,
                    group = combined_label),
                width = 0.3, position = position_dodge(width = 0.8),size=1,alpha=0.8) + 
  geom_point(aes(x = interaction(region,strat_var), y = mean_ratio, 
                 fill = combined_label,
                 shape = combined_label,
                 group = combined_label), 
             size = 2, position = position_dodge(width = 0.8), color = "black") + 
  labs(y = expression(log(widehat(CV)/widehat(CV)[EBS])), x = '') +   
  theme_bw() + 
  facet_wrap(~species , nrow = 5, dir = 'h') +
    theme_bw() + 
    facet_wrap(~common, scales = 'free_y', nrow = 5, dir = 'h') +
    scale_y_continuous(labels = scales::label_number(accuracy = 0.01)) +
    scale_fill_manual(
      values = colors_warm_cold,
      labels = labels_warm_cold,
      name = legend_title_warm_cold
    ) +
    scale_color_manual(
      values = colors_warm_cold,
      labels = labels_warm_cold,
      name = legend_title_warm_cold
    ) +
    scale_shape_manual(
      values = shapes_warm_cold,
      labels = labels_warm_cold,
      name = legend_title_warm_cold
    ) +
    scale_linetype_manual(
      values = linetype_warm_cold,
      labels = labels_warm_cold,
      name = legend_title_warm_cold
    ) +
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

#save plot
ragg::agg_png(paste0('./figures slope/logcvratio_warmcold.png'), width = 18, height = 12, units = "in", res = 300)
#ragg::agg_png(paste0('./figures/ms_hist_indices_cv_box_EBSNBS_suppl.png'), width = 13, height = 8, units = "in", res = 300)
p
dev.off()

#get by static or adaptive
df_summary$regime1<-ifelse(df_summary$regime=='all','all','dyn')
df_summary$combined_label1<-paste0(df_summary$approach," - ",df_summary$regime1)
df_summary$combined_label1<-factor(df_summary$combined_label1,c("rand - all",'sb - all',"rand - dyn",'sb - dyn'))


# Keep only necessary simulations (e.g., sim 1) or average across sims if desired
df_summary_clean <- df_summary[, .(
  mean_ratio = mean(mean_ratio, na.rm = TRUE),
  q10 = mean(q10, na.rm = TRUE),  # or another logic
  q90 = mean(q90, na.rm = TRUE)  # or another logic
), by = .(species, scenario, approach,region,common,combined_label1,regime1)]

# Join with scenario-level metadata if needed
# Assuming samp_df contains mapping of scn to region, strat_var, approach, regime1, etc.
df_summary_clean<-merge(df_summary_clean,samp_df,by=c('scenario','region'))
# Replace '+' with '\n' in 'region'
df_summary_clean[, region1 := gsub("\\+", "\n", region)]

#remove infinitive values
df_summary_clean <- df_summary_clean[
  is.finite(mean_ratio)
]

#scale
y_scale <- aggregate((mean_ratio + q90) ~ common, df_summary_clean, max)
y_scale$scale <- y_scale$`(mean_ratio + q90)` * 1.1
y_scale$text  <- y_scale$`(mean_ratio + q90)` * 1.1
y_scale$apr       <- "sb"
y_scale$scn       <- "scn1"
y_scale$year      <- 2022
#y_scale$region    <- "EBS"
y_scale$strat_var <- "depth"

#plot static adaptive
p1 <-
    ggplot(subset(df_summary_clean, region != 'EBS')) +
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
    theme_bw() +
    facet_wrap(~ common, scales = "free_y", nrow = 5, dir = "h") +
    scale_fill_manual(
      values = colors_static_adaptive,
      labels = labels_static_adaptive,
      name = legend_title_static_adaptive
    ) +
    
    scale_color_manual(
      values = colors_static_adaptive,
      labels = labels_static_adaptive,
      name = legend_title_static_adaptive
    ) +
    
    scale_shape_manual(
      values = shapes_static_adaptive,
      labels = labels_static_adaptive,
      name = legend_title_static_adaptive
    ) +
    
    scale_linetype_manual(
      values = linetype_static_adaptive,
      labels = labels_static_adaptive,
      name = legend_title_static_adaptive
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

ragg::agg_png(paste0("./figures slope/logcvratio.png"), width = 18, height = 12, units = "in", res = 300)
p1
dev.off()
