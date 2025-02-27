#####################################
# Settings
#####################################

#libraries from cran to call or install/load
pack_cran<-c('shadowtext',
             'cowplot',
             'ggspatial',
             'raster',
             'rasterVis',
             'rgeos',
             'scales',
             'rnaturalearth',
             'grid',
             'ggplot2',
             'lubridate',
             'ragg',
             'rgdal',
             'dplyr',
             'sp',
             'ggtext',
             'magick',
             'shadowtext',
             'dplyr',
             'googledrive',
             'sf',
             'knitr',
             'data.table',
             'ggshadow',
             'devtools',
             'reshape2')

#install pacman to use p_load function - call library and if not installed, then install
if (!('pacman' %in% installed.packages())) {
  install.packages("pacman");library(pacman)} else {
    library(pacman)
  }

#install akgfmaps to extract shapefile of Alaska
if (!('akgfmaps' %in% installed.packages())) {
  install_github('afsc-gap-products/akgfmaps')};library(akgfmaps)

#install VAST
if (!('VAST' %in% installed.packages())) {
  install_github("james-thorson/VAST@main", INSTALL_opts="--no-staged-install")};library(VAST)

#load/install packages
p_load(pack_cran,character.only = TRUE)

# Set the root directory for knitting to a work directory
dir <- '/Users/daniel/Work/Adapting Monitoring to a Changing Seascape/'

#setwd
setwd(dir)

#selected species
sel_sp<-c("Gadus chalcogrammus", #Alaskan pollock
          "Gadus macrocephalus", #pacific cod
          "Reinhardtius hippoglossoides", #Greenland turbot
          "Chionoecetes opilio") #snow crab

df_sp<-data.frame(sci=sel_sp,
                  common=c('Alaska pollock',
                           'Pacific cod',
                           'Greenland turbot',
                           'Snow crab'))
  
  
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
grid<-grid2[,c('Lat','Lon','region','cell')]

bold_years<-c(2002,2004,2008,2010,2012,2016)

###########################
# TEMP AMD REGIME
###########################

#load file grid
load('./data processed/grid_EBS_NBS.RData') #grid.ebs_year

# Define a custom function to calculate mean and SD
calc_stats <- function(x) {
  c(mean = mean(x, na.rm = TRUE),
    SD = sd(x, na.rm = TRUE))
}

# Use aggregate to calculate statistics for Temp by Year and region
temp_region <- aggregate(Temp ~ Year + region, data = grid.ebs_year, FUN = calc_stats)

# Convert the output to a dataframe
temp_region <- do.call(data.frame, temp_region)
names(temp_region) <- c('year', 'region', 'mean_temp', 'SD')

# Calculate the average for all regions
alltemp <- aggregate(mean_temp ~ year, data = temp_region, FUN = mean)
alltemp$region <- 'all'
alltemp$SD <- aggregate(SD ~ year, data = temp_region, FUN = mean)$SD

# Combine the datasets
temp_region <- rbind(temp_region, alltemp)

# Scaling values by group
temp_region1 <- temp_region %>%
  group_by(region) %>%
  mutate(scaledtemp = scale(mean_temp)) %>%
  ungroup()

# Classify by cold/warm/normal years
temp_region1$typeyear <- ifelse(temp_region1$scaledtemp > 1, 
                                "warm", ifelse(temp_region1$scaledtemp < -1, 
                                               "cold", "normal"))

# Create custom labels function
custom_labels <- function(x) {
  sapply(x, function(year) {
    if (year %in% bold_years) {
      paste0("**", year, "**")
    } else {
      as.character(year)
    }
  })
}

# Plot with SD
p<-
  ggplot() +
    geom_rect(aes(xmin = 2002, xmax = 2005, ymin = -Inf, ymax = Inf, fill = "red")) +
    geom_rect(aes(xmin = 2014, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "red")) +
    geom_rect(aes(xmin = 2005, xmax = 2014, ymin = -Inf, ymax = Inf, fill = "blue")) +
    geom_vline(xintercept = seq(1985, 2020, by = 5), color = rgb(0, 0, 0,90, maxColorValue = 285), linetype = 'solid') +  # Custom gridlines
    geom_vline(xintercept = setdiff(1982:2022,seq(1985, 2020, by = 5)), color = rgb(0, 0, 0,90, maxColorValue = 285), linetype = 'solid',size=0.2) +  # Custom gridlines
    geom_hline(yintercept = c(0:5), color = rgb(0, 0, 0,90, maxColorValue = 285), linetype = 'solid',size=0.2) +  # Custom gridlines
    geom_ribbon(data = subset(temp_region1, region == 'all' & year %in% c(1982:2022)),
                aes(x = year, ymin = mean_temp - SD, ymax = mean_temp + SD), fill = 'gray', alpha = 0.4) +
    geom_hline(yintercept = colMeans(subset(temp_region1, region == 'all' & year %in% 1982:2022)[,'mean_temp']), alpha = 0.5, linetype = 'dashed') +
    geom_line(data = subset(temp_region1, region == 'all' & year %in% 1982:2022),
              aes(x = year, y = mean_temp, color = region)) +
    #geom_point(data = subset(temp_region1, region == 'all' & year %in% 1982:2022),
    #           aes(x = year, y = mean_temp, color = region),size=.7) +
    geom_vline(xintercept = 2002, color = 'black',linewidth=1, linetype = 'solid') +  # Custom gridlines
  annotation_custom(
    grob = linesGrob(gp = gpar(col = "black", lwd = 4)),  # Thicker top border
    xmin = 2002, xmax = 2022, ymin = -Inf, ymax = -Inf
  )+
  annotation_custom(
    grob = linesGrob(gp = gpar(col = "black", lwd = 4)),  # Thicker top border
    xmin = 2002, xmax = 2022, ymin = Inf, ymax = Inf
  )+
   annotation_custom(
          grob = linesGrob(gp = gpar(col = "black", lwd = 4)),  # Thicker top border
            xmin = 2022, xmax = 2022, ymin = -Inf, ymax = Inf
       )+
    scale_fill_manual(values = c("red" = "#cc1d1f", "blue" = "#1675ac"),
                      labels = c("cold", "warm"),
                      name = "SBT regime") +
    scale_color_manual(values = c('all' = 'black'),
                       breaks = c('all'),
                       labels = c('Bering\nSea'),name='',guide=FALSE) +
    theme_bw() +
    scale_x_continuous(breaks = c(1985,1990,1995,2000,bold_years,2020),
                       minor_breaks = c(1982:2022), 
                       labels = custom_labels, limits = c(1982, 2022),
                       expand = c(0,0)) +
    theme(axis.text.x = element_markdown(angle = 45,hjust=1),
          axis.text.y = element_text(),text = element_text(size = 12),
          panel.grid = element_blank()) +
    labs(x = '', y = 'mean SBT')


#save env plot
agg_png(paste0('./figures slope/temperature_regime2.png'), width = 7, height = 4, units = "in", res = 300)
print(p)
dev.off()

# # Define a custom color scale function
# custom_colors <- colorRampPalette(c("#1675ac", "white", "#cc1d1f"))
# 
# ggplot() +
#   # Adding vertical rects per year, filled by temperature
#   geom_rect(data = subset(temp_region1, region == 'all' & year %in% 1982:2022),
#             aes(xmin = year - 0.5, xmax = year + 0.5, ymin = -Inf, ymax = Inf, 
#                 fill = temp)) +
#   
#   # Adding horizontal dashed line for mean temperature
#   geom_hline(yintercept = mean(subset(temp_region1, region == 'all' & year %in% 1982:2022)[,'temp']), 
#              alpha = 0.5, linetype = 'dashed') +
#   
#   # Plotting the temperature time series for the 'all' region
#   geom_line(data = subset(temp_region1, region == 'all' & year %in% 1982:2022),
#             aes(x = year, y = temp, color = region)) +
#   geom_point(data = subset(temp_region1, region == 'all' & year %in% 1982:2022),
#              aes(x = year, y = temp, color = region)) +
#   
#   # Define the color gradient based on the temperature values
#   scale_fill_gradientn(colors = custom_colors(20), 
#                        name = "SBT (°C)") +
#   
#   # Manually setting the color of the line for 'all' region
#   scale_color_manual(values = c('all' = 'black'), 
#                      breaks = c('all'), 
#                      labels = c('Bering\nSea'), 
#                      name = '', guide = FALSE) +
#   
#   # Customizing x-axis with custom breaks and labels
#   scale_x_continuous(breaks = c(1985, 1990, 1995, 2000, bold_years, 2020), 
#                      minor_breaks = c(1982:2022), 
#                      labels = custom_labels, 
#                      limits = c(1982, 2022), 
#                      expand = c(0.01, 0)) +
#   
#   # Customizing theme elements
#   theme_bw() +
#   theme(axis.text.x = element_markdown(angle = 45, hjust = 1), 
#         axis.text.y = element_text(), 
#         text = element_text(size = 12)) +
#   
#   # Adding axis labels
#   labs(x = '', y = 'mean SBT')

###########################
# DEPTH DISTRIBUTION OF OCCURRENCE
###########################

#load file observed dataframe input 
load('./output slope//obs_df.RData') #obs_df

#survey as factors and rename
obs_df$survey_name<-as.factor(obs_df$survey_name)
levels(obs_df$survey_name)<-c('EBSshelf','EBSslope','NBS')

#only presences, EBSshelf+EBSslope and selected species
obs_df<-subset(obs_df,cpue_kgkm2!=0 &
                 survey_name %in% c('EBSshelf','EBSslope','NBS') &
                 species %in% sel_sp)

#filter by cold and warm years
#obs_df1<-obs_df[which(obs_df$year %in% c(cyrs,wyrs)),]
obs_df1<-obs_df[which(obs_df$year %in% bold_years),]

#year type
obs_df1$year_type<-ifelse(obs_df1$year %in% c(2002,2004,2016), "warm",'cold')

#filtered to <600m
filt_obs_df1<-subset(obs_df1,depth_m<=600)

#get common
filt_obs_df1$common<-df_sp$common[match(filt_obs_df1$species, df_sp$sci)]
filt_obs_df1$common<-factor(filt_obs_df1$common,levels=c("Alaska pollock","Greenland turbot","Pacific cod","Snow crab" ))

#plot Kernel density estimation for depth distributions
p<-
  ggplot(filt_obs_df1, aes(x = depth_m, fill = year_type)) +
    geom_density(alpha = 0.5) +
    labs(x = 'depth (m)', y = 'probability of occurrence')+ 
         #title = 'Empirical depth distribution of occurrence in Warm vs. Cold Years') +
    scale_fill_manual(values = c('cold' = '#1675ac',
                                 'warm' = '#cc1d1f'),
                      labels = c("cold", "warm"),
                      name = 'SBT regime') +
    theme_bw()+
    scale_x_continuous(limits = c(0,600))+
    theme(strip.text = element_text(size=12),strip.background = element_blank(),
          text= element_text(size=12))+
    facet_wrap(~common)


library(ggplot2)
library(dplyr)

# Load data
load('./output slope//obs_df.RData')

# Convert survey names to factors and rename
obs_df$survey_name <- as.factor(obs_df$survey_name)
levels(obs_df$survey_name) <- c('EBSshelf', 'EBSslope', 'NBS')

# Filter to selected surveys, species, and <600m depth
obs_df1 <- obs_df %>%
  filter(survey_name %in% c('EBSshelf', 'EBSslope', 'NBS') & species %in% sel_sp & depth_m <= 600)

# Assign year type (cold vs. warm)
obs_df1 <- obs_df1 %>%
  mutate(year_type = ifelse(year %in% c(2002, 2004, 2016), "warm", "cold"),
         year_type = factor(year_type, levels = c("cold", "warm")))  # Ensure proper factor levels

# Map species scientific names to common names
obs_df1$common <- factor(df_sp$common[match(obs_df1$species, df_sp$sci)], 
                         levels = c("Alaska pollock", "Greenland turbot", "Pacific cod", "Snow crab"))

# Ensure no NA values in cpue_kgkm2 and depth_m
obs_df1 <- obs_df1 %>% filter(!is.na(cpue_kgkm2) & !is.na(depth_m))

# Bin depths
obs_df1$depth_bin <- cut(obs_df1$depth_m, breaks = seq(0, 600, by = 25), include.lowest = TRUE)

# Calculate probability of presence per depth bin
prob_df <- obs_df1 %>%
  group_by(common, year_type, depth_bin) %>%
  summarise(n_present = sum(cpue_kgkm2 > 0),
            n_total = n(),
            prob_presence = n_present / n_total,  # Probability of presence
            depth_mid = mean(as.numeric(sub("\\((.+),(.+)\\]", "\\1", depth_bin)))) %>% # Extract bin midpoint
  ungroup()

# Plot smoothed probability of presence
p <- 
  ggplot(prob_df, aes(x = depth_mid, y = prob_presence, color = year_type)) +
  geom_point(alpha = 0.5) +  # Show raw probabilities
  geom_smooth(method = "loess", se = FALSE) +  # Smooth probability trend
    coord_cartesian(ylim = c(0, 1))+
  labs(x = 'depth (m)', y = 'probability of occurrence') +
  #scale_y_continuous(limits = c(0, 1)) +  # Ensure probability scale from 0 to 1
  scale_x_continuous(limits = c(0, 600),expand = c(0,0),breaks = c(100,300,500))+#,) +
  scale_color_manual(values = c('cold' = '#1675ac', 'warm' = '#cc1d1f'),
                     labels = c("cold", "warm"),
                     name = 'SBT regime') +
  #scale_y_continuous( )+
  theme_bw() +
  theme(strip.text = element_text(size = 12),
        strip.background = element_blank(),
        text = element_text(size = 12)) +
  facet_wrap(~common)

print(p)


#save env plot
agg_png(paste0('./figures slope/depth_distribution_occurrence1.png'), width = 7, height = 4, units = "in", res = 300)
print(p)
dev.off()

#load files density slope and shelf data 
load('./output slope/species/ms_sim_dens_all.RData')  #sim_dens1
dimnames(sim_dens1)

dens<-sim_dens1[,sel_sp,as.character(bold_years),]
dimnames(dens)

dens1<-as.data.frame.table(dens)
names(dens1)<-c('cell','species','Year','sim','dens')

#year type
wyrs<-c(2002,2004,2016)
dens1$year_type<-ifelse(dens1$Year %in% wyrs, "warm",'cold')

#load env
load('./grid_EBS_NBS_envs.RData') #xx
envs<-subset(xx,Year=='2002')
grid1<-merge(grid,envs,by=c('Lat','Lon'))

dens2<-merge(dens1,grid1[,c('cell','depth_m')],by=c('cell'))
dim(dens2)
dim(dens1)

#filtered to <600m
filt_dens2<-subset(dens2,depth_m<=600 & dens!=0)

#get common
filt_dens2$common<-df_sp$common[match(filt_dens2$species, df_sp$sci)]
filt_dens2$common<-factor(filt_dens2$common,levels=c(df_sp$common))

#plot Kernel density estimation for depth distributions
p<-
  ggplot(filt_dens2, aes(x = depth_m, fill = year_type)) +
  geom_density(alpha = 0.5) +
  labs(x = 'depth (m)', y = 'density of occurrence')+ 
  #title = 'Empirical depth distribution of occurrence in Warm vs. Cold Years') +
  scale_fill_manual(values = c('cold' = '#1675ac',
                               'warm' = '#cc1d1f'),
                    labels = c("cold", "warm"),
                    name = 'SBT regime') +
  theme_bw()+
  scale_x_continuous(limits = c(0,600))+
  theme(strip.text = element_text(size=12),strip.background = element_blank(),
        text= element_text(size=12))+
  facet_wrap(~common)


###########################
# FRACTION ABUNDANCE
###########################

#load file index of selected species
load('./ind_sel.RData') #ind_all

#reshape index
ind_all1<-melt(ind_all,id.vars=c('year','species'))

#keep NBS and EBSshelf
ind_all2<-subset(ind_all1,variable %in% c('NBS','EBSshelf'))

#get fraction
ind_all3<-ind_all2 %>% 
  group_by(species, year) %>%
  mutate(frac = value / sum(value))

# Create a dummy data frame for legend rectangles
legend_rects <- data.frame(
  ymin = -Inf,
  ymax = Inf,
  xmin = c(-Inf, 2005.5,2013.5),
  xmax = c(2005.5, 2013.5,Inf),
  fill = c('red', 'blue','red'),
  label = c('Period 1', 'Period 2','Period 1')
)

#plot
p<-
ggplot() +
  geom_rect(data = legend_rects, aes(xmin = xmin, xmax = xmax, 
                                     ymin = ymin, ymax = ymax, fill = label), 
            show.legend = TRUE,alpha=0.8) +
  # annotate("rect", xmin = 2002, xmax = 2005.5, 
  #          ymin = -Inf, ymax = Inf, alpha = 0.5, fill = 'red') +
  # annotate("rect", xmin = 2013.5, xmax = 2022.5, 
  #          ymin = -Inf, ymax = Inf, alpha = 0.5, fill = 'red') +
  # annotate("rect", xmin = 2005.5, xmax = 2013.5, 
  #          ymin = -Inf, ymax = Inf, alpha = 0.5, fill = 'blue') +
  geom_bar(data = ind_all3, aes(x = year, y = frac, fill = variable), 
           stat = 'identity',alpha=0.8) +
  scale_fill_manual(values = c('EBSshelf' = '#046407', 
                               'NBS' = '#B4AF46',
                               'Period 1' = '#cc1d1f',
                               'Period 2' = '#1675ac'),
                    breaks = c('EBSshelf', 'NBS', 'Period 2','Period 1'),
                    labels = c('EBS', 'NBS', 'cold', 'warm'),
                    name = 'region and\nSBT regime') +
  scale_x_continuous(breaks = seq(from = 1985, to = 2020, by = 5),limits = c(2001.5,2022.5),
                     expand = c(0.01,0)) +
  scale_y_continuous(expand = c(0.02,0))+
  labs(y = 'abundance proportion') +
  theme_bw() +
  theme(strip.text = element_text(size = 12),
        strip.background = element_blank(),
        text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 0.7, vjust = 0.8),
        axis.title.x = element_blank()) +
  facet_wrap(~ species, scales = 'free_x', nrow = 2) +
  guides(fill = guide_legend(override.aes = list(alpha = 1)))

#save env plot
agg_png(paste0('./figures slope/abundance_fraction.png'), width = 7, height = 4, units = "in", res = 300)
print(p)
dev.off()

#load files density slope and shelf data 
load('./output slope/species/ms_sim_dens_all.RData')  #sim_dens1
dimnames(sim_dens1)

#subset years and species densities
dens<-sim_dens1[,sel_sp,as.character(2002:2022),]
dimnames(dens)

#load grid of NBS and EBS
load('./extrapolation grids/northern_bering_sea_grid.rda')
load('./extrapolation grids/eastern_bering_sea_grid.rda')
#load('./extrapolation grids/bering_sea_slope_grid.rda')
#colnames(bering_sea_slope_grid)[4]<-'Stratum'
#bering_sea_slope_grid$Stratum<-NA
grid<-as.data.frame(rbind(data.frame(northern_bering_sea_grid,region='NBS'),
                          data.frame(eastern_bering_sea_grid,region='EBS')))
grid$cell<-1:nrow(grid)
grid2<-grid
grid<-grid2[,c('Area_in_survey_km2','region','cell')]

#data input
bio_df<-
expand.grid('sp'=dimnames(dens)[[2]],
            'year'=dimnames(dens)[[3]],
            'sim'=dimnames(dens)[[4]])

bio_df$EBSbio<-NA
bio_df$EBSpct<-NA
bio_df$NBSbio<-NA
bio_df$NBSpct<-NA

for (r in 1:nrow(bio_df)) {
  
  #r<-1
  
  cat(paste('#',r,"-",nrow(bio_df),'\n'))
  
  s<-bio_df[r,'sp']
  y<-bio_df[r,'year']
  sim<-bio_df[r,'sim']
  region<-bio_df[r,'region']
  
  bio<-dens[1:53464,s,y,sim]*grid$Area_in_survey_km2
  
  bio_df[r,'EBSbio']<-sum(bio[15181:53464])
  bio_df[r,'NBSbio']<-sum(bio[1:15180])
  bio_df[r,'EBSpct']<-sum(bio[15181:53464])/(sum(bio[15181:53464])+sum(bio[1:15180]))
  bio_df[r,'NBSpct']<-sum(bio[1:15180])/(sum(bio[15181:53464])+sum(bio[1:15180]))

}

#reshape
bio_df1<-bio_df[,c('sp','year','sim','EBSpct','NBSpct')]
bio_df2<-melt(bio_df1)

#mean over region and simulated data
sim_indmean<-aggregate(value ~ year + variable + sp,bio_df2,FUN=mean)
sim_indsd<-aggregate(value ~ year + variable + sp,bio_df2,FUN=sd)

#rename
names(sim_indmean)[ncol(sim_indmean)]<-'mean'
sim_indmean1<-cbind(sim_indmean,'sd'=sim_indsd$value)
sim_indmean1$year<-as.numeric(sim_indmean1$year)+2001

#sort levels
sim_indmean1$sp<-factor(sim_indmean1$sp,
                                levels=c("Chionoecetes opilio" ,"Gadus chalcogrammus","Gadus macrocephalus","Reinhardtius hippoglossoides" ))

#get common
sim_indmean1$common<-df_sp$common[match(sim_indmean1$sp, df_sp$sci)]
sim_indmean1$common<-factor(sim_indmean1$common,levels=c("Alaska pollock","Greenland turbot","Pacific cod","Snow crab" ))

#plot
p<-
  ggplot() +
  geom_rect(data = legend_rects, aes(xmin = xmin, xmax = xmax, 
                                     ymin = ymin, ymax = ymax, fill = label), 
            show.legend = TRUE,alpha=0.8) +
  # annotate("rect", xmin = 2002, xmax = 2005.5, 
  #          ymin = -Inf, ymax = Inf, alpha = 0.5, fill = 'red') +
  # annotate("rect", xmin = 2013.5, xmax = 2022.5, 
  #          ymin = -Inf, ymax = Inf, alpha = 0.5, fill = 'red') +
  # annotate("rect", xmin = 2005.5, xmax = 2013.5, 
  #          ymin = -Inf, ymax = Inf, alpha = 0.5, fill = 'blue') +
  geom_bar(data = sim_indmean1, aes(x = year, y = mean, fill = variable), 
           stat = 'identity',alpha=0.8) +
  geom_errorbar(data = subset(sim_indmean1,variable=='NBSpct'), aes(x = year, ymin = mean-sd, ymax=mean+sd), 
            stat = 'identity',alpha=0.5) +
  scale_fill_manual(values = c('EBSpct' = '#046407', 
                               'NBSpct' = '#B4AF46',
                               'Period 1' = '#cc1d1f',
                               'Period 2' = '#1675ac'),
                    breaks = c('EBSpct', 'NBSpct', 'Period 2','Period 1'),
                    labels = c('EBS', 'NBS', 'cold', 'warm'),
                    name = 'region and\nSBT regime') +
  scale_x_continuous(breaks = seq(from = 1985, to = 2020, by = 5),limits = c(2001.5,2022.5),
                     expand = c(0.01,0)) +
  scale_y_continuous(expand = c(0.02,0))+
  labs(y = 'abundance proportion') +
  theme_bw() +
  theme(strip.text = element_text(size = 12),
        strip.background = element_blank(),
        text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 0.7, vjust = 0.8),
        axis.title.x = element_blank()) +
  facet_wrap(~ common, scales = 'free_x', nrow = 2) +
  guides(fill = guide_legend(override.aes = list(alpha = 1)))

#save env plot
agg_png(paste0('./figures slope/abundance_fraction_sim.png'), width = 7, height = 4, units = "in", res = 300)
print(p)
dev.off()


###########################
# SIMULATED METRICS
###########################

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
grid<-grid2[,c('Lat','Lon','region','cell')]

#load files density slope and shelf data 
load('./output slope/species/ms_sim_dens_all.RData')  #sim_dens1
load('./grid_EBS_NBS_envs.RData') #xx

#df to store results
metrics_df<-data.frame(matrix(NA,nrow = 0,ncol=16))
names(metrics_df)<-c("Year","COG_lat","COG_lon","COG_depth","COG_temp",
                     "total_bio",'mean_dens','mean_temp','depthq10','depthq90',"q10","q20","q80",
                     "q90","species",'sim')

#shelf slope spp
#shelfslope_spp<-unique(dens_all$species)

#yrs with slope data()
yrs<-as.character(c(2002,2004,2008,2010,2012,2016))

#subset dens
dens_all<-sim_dens1[,sel_sp,yrs,]

#calculate center of gravity, percentiles and effective area occupied
#loop over common spp in shelf and slope
#for (s in sel_sp) { 

for (y in yrs) { 
    
  #y<-yrs[1]
  
  #dens
  dens<-dens_all[,,y,]  
  
  #grid with envs
  grid.ebs_year1<-subset(xx,Year %in% y)
  grids<-merge(grid,grid.ebs_year1,by=c('Lon','Lat'))
  grids<-grids[order(grids$cell, decreasing = FALSE), ]   

  
  for (sim in 1:100) {
    for (s in sel_sp) {
    
      #sim<-1;s<-sel_sp[1]
      
      cat(paste(y,s,'\n'))

      #merge dens and grid
      dens1<-cbind(dens[,s,sim],grids)
      names(dens1)[1]<-'dens'
      
      #calculate quantiles using tapply
      quantiles <- aggregate(dens ~ Year, data = dens1, 
                             FUN = quantile,c(0.10,0.20,0.80,0.90))
      quantiles<- data.frame(as.matrix(quantiles))
      names(quantiles)<-c('Year','q10','q20','q80','q90')
      
      #get quantiles
      p1 <- quantiles$q10
      p2 <- quantiles$q90
      
      #filter
      dens1$quantile <- ifelse(dens1$dens < p2 & dens1$dens > p1, 1, 0)
      
      # Sort the dataframe by DepthGEBCO
      all_bio4 <- dens1[order(dens1$DepthGEBCO), ]
      
      # Calculate cumulative biomass
      all_bio4$bio<-all_bio4$dens*all_bio4$Area_in_survey_km2
      all_bio4$cumulative_biomass <- cumsum(all_bio4$bio)
      
      # Normalize the cumulative biomass to get a cumulative distribution (0 to 1)
      all_bio4$cumulative_biomass_scale <- all_bio4$cumulative_biomass / 
        max(all_bio4$cumulative_biomass)
      
      # Find the depths corresponding to the 10th and 90th percentiles
      ps <- c(0.10, 0.90)
      depth_10th <- all_bio4$DepthGEBCO[which.min(
        abs(all_bio4$cumulative_biomass_scale - ps[1]))]
      depth_90th <- all_bio4$DepthGEBCO[which.min(
        abs(all_bio4$cumulative_biomass_scale - ps[2]))]
      
      #COG and total abundance
      centroids <- dens1 %>%
        group_by(Year) %>%
        summarise(COG_lat=sum(dens*Lat,na.rm=TRUE)/sum(dens,na.rm=TRUE),
                  COG_lon=sum(dens*Lon,na.rm=TRUE)/sum(dens,na.rm=TRUE),
                  COG_depth=sum(dens*DepthGEBCO,na.rm=TRUE)/sum(dens,na.rm=TRUE),
                  COG_temp=sum(dens*Temp,na.rm = TRUE)/sum(dens,na.rm=TRUE),
                  total_bio=sum(dens*Area_in_survey_km2,na.rm=TRUE),
                  mean_dens=mean(dens,na.rm=TRUE),
                  mean_temp=mean(Temp,na.rm=TRUE),
                  depthq10=depth_10th,
                  depthq90=depth_90th)
      
      #cbind data
      metrics<-cbind(centroids,quantiles[,c('q10','q20','q80','q90')],'species'=s,'sim'=sim)
      
      #append
      metrics_df<-rbind(metrics_df,metrics) 
    }
  }
}

#save table
save(metrics_df,file='./output slope/simulated_metrics.RData')
load(file='./output slope/simulated_metrics.RData') #metrics_df

#aggregate by year and species
aggregate(q10 ~ species + Year, metrics_df,FUN=length)

#check same species and same depth10 and depth90
#metrics_dfi<-subset(metrics_df,species=='Gadus macrocephalus')
#metrics_dfi2<-metrics_dfi[duplicated(metrics_dfi$depthq10) & duplicated(metrics_dfi$depthq90) ,]

# Define a custom color scale function
custom_colors <- colorRampPalette(c("#1675ac", "white", "#cc1d1f"))

#selected species
sel_sp<-c("Gadus chalcogrammus", #Alaskan pollock
          "Gadus macrocephalus", #pacific cod
          "Reinhardtius hippoglossoides", #Greenland turbot
          "Chionoecetes opilio") #snow crab

# Load required package
library(dplyr)

#scale to keep one legend
metrics_df <- metrics_df %>%
  group_by(species) %>%
  mutate(bio_scaled = scale(total_bio))

# Calculate mean and SD for COG_lat and COG_depth grouped by Year
summary_stats <- metrics_df %>%
  group_by(Year,species) %>%
  summarise(
    mean_COG_lat = mean(COG_lat, na.rm = TRUE),
    mean_temp=mean(mean_temp,na.rm=TRUE),
    sd_COG_lat = sd(COG_lat, na.rm = TRUE),
    mean_COG_depth = mean(COG_depth, na.rm = TRUE),
    sd_COG_depth = sd(COG_depth, na.rm = TRUE),
    mean_depth10 = mean(depthq10, na.rm = TRUE),
    mean_depth90 = mean(depthq90, na.rm = TRUE),
    mean_scaledbio = mean(bio_scaled,na.rm=TRUE)
  )

# View the summary
print(summary_stats)

###########################
# COG PLOT
###########################

#plot
#p<-
ggplot(data = summary_stats) +
  #geom_errorbar(aes(xmin = mean_COG_depth - sd_COG_depth, xmax = mean_COG_depth + sd_COG_depth, 
  #                   y = mean_COG_lat, color = mean_temp)) +
  # Add vertical error bars for COG_lat (latitudinal uncertainty)
  #geom_errorbar(aes(ymin = mean_COG_lat - sd_COG_lat, ymax = mean_COG_lat + sd_COG_lat, 
  #                  x = mean_COG_depth, color = mean_temp)) +
  geom_shadowtext(aes(x = mean_COG_depth, y = mean_COG_lat, color = mean_temp, label = Year),
                  fontface = 'bold', bg.r = 0.05) +
  theme_bw() +
  labs(x = 'bathymetrical COG (m)', y = 'latitudinal COG') +
  # Add horizontal error bars for COG_depth (bathymetrical uncertainty)
  
  theme(aspect.ratio = 1,
        text = element_text(size = 12),
        strip.background = element_blank(),
        strip.text = element_text(size = 12)) +
  scale_color_gradientn(colors = custom_colors(20), name = 'SBT (°C)',
                        guide = guide_colorbar(frame.colour = "black", ticks.colour = "black")) +
  scale_x_continuous(expand = expansion(mult = 0.1)) +  # Add buffer on the x-axis
  scale_y_continuous(expand = expansion(mult = 0.1)) +  # Add buffer on the y-axis
  facet_wrap(~species, scales = 'free')

#get common
summary_stats$common<-df_sp$common[match(summary_stats$species, df_sp$sci)]
summary_stats$common<-factor(summary_stats$common,levels=c("Alaska pollock","Greenland turbot","Pacific cod","Snow crab" ))


p <- ggplot(data = summary_stats) +
  # geom_errorbar code (if needed) +
  geom_point(aes(x = mean_COG_depth, y = mean_COG_lat, fill = mean_temp, size = mean_scaledbio), shape = 21) +
  theme_bw() +
  labs(x = 'bathymetrical COG (m)', y = 'latitudinal COG') +
  theme(
    aspect.ratio = 1,
    text = element_text(size = 12),
    strip.background = element_blank(),
    strip.text = element_text(size = 12)
  ) +
  scale_fill_gradientn(colors = custom_colors(20), name = 'SBT (°C)',
                       guide = guide_colorbar(frame.colour = "black", ticks.colour = "black")) +
  scale_size(name = 'abundance', breaks = c(min(summary_stats$mean_scaledbio), 
                                            mean(summary_stats$mean_scaledbio), 
                                            max(summary_stats$mean_scaledbio)),
             labels = c('low', 'medium', 'high'),range = c(2, 10)) +
  scale_x_continuous(expand = expansion(mult = 0.1)) +
  scale_y_continuous(expand = expansion(mult = 0.1)) +
  facet_wrap(~common, scales = 'free')

#save env plot
agg_png(paste0('./figures slope/cog.tiff'), width = 7, height = 6.5, units = "in", res = 300)
print(p)
dev.off()

#get common
metrics_df$common<-df_sp$common[match(metrics_df$species, df_sp$sci)]
metrics_df$common<-factor(metrics_df$common,levels=c("Alaska pollock","Greenland turbot","Pacific cod","Snow crab" ))

#plot using SBT color
p<-
ggplot(data = subset(metrics_df, species %in% sel_sp)) +
  #geom_shadowtext(aes(x = COG_depth, y = COG_lat, color = mean_temp, label = Year),
  #                fontface = 'bold', bg.r = 0.05) +
  geom_point(aes(x = COG_depth, y = COG_lat, fill = mean_temp,size=bio_scaled),shape=21,color=rgb(0, 0, 0, alpha = 0.2)) +
  theme_bw() +
  labs(x = 'bathymetrical COG (m)', y = 'latitudinal COG (°)') +
  theme(aspect.ratio = 1,
        strip.background = element_blank(),
        strip.text = element_text(size = 12)) +
  scale_size(name = 'abundance', breaks = c(min(summary_stats$mean_scaledbio), 
                                            mean(summary_stats$mean_scaledbio), 
                                            max(summary_stats$mean_scaledbio)),
             labels = c('low', 'medium', 'high'),range = c(2, 10)) +
  scale_fill_gradientn(colors = custom_colors(100), name = 'SBT (°C)',
                        guide = guide_colorbar(frame.colour = "black", ticks.colour = "black")) +
  scale_x_continuous(expand = expansion(mult = 0.1)) +  # Add buffer on the x-axis
  scale_y_continuous(expand = expansion(mult = 0.1)) +  # Add buffer on the y-axis
  facet_wrap(~common, scales = 'free')

#save env plot
agg_png(paste0('./figures slope/cog_abu.png'), width = 7, height = 6.5, units = "in", res = 300)
print(p)
dev.off()

p <- 
  ggplot(data = subset(metrics_df, species %in% sel_sp)) +
  # geom_errorbar code (if needed) +
  geom_point(aes(x = COG_depth, y = COG_lat, fill = mean_temp), shape = 21,size=3,color='transparent') + #color=rgb(0, 0, 0, alpha = 0.2),
  theme_bw() +
  labs(x = 'bathymetrical COG (m)', y = 'latitudinal COG (°)') +
  theme(
    aspect.ratio = 1,
    text = element_text(size = 12),
    strip.background = element_blank(),
    strip.text = element_text(size = 12),
    panel.grid.minor = element_line(linetype = 'dashed')
  ) +
  scale_fill_gradientn(colors = custom_colors(20), name = 'SBT (°C)',
                       guide = guide_colorbar(frame.colour = "black", ticks.colour = "black")) +
  scale_size(name = 'abundance', breaks = c(min(summary_stats$mean_scaledbio), 
                                            mean(summary_stats$mean_scaledbio), 
                                            max(summary_stats$mean_scaledbio)),
             labels = c('low', 'medium', 'high'),range = c(2, 10)) +
  scale_x_continuous(expand = expansion(mult = 0.1)) +
  scale_y_continuous(expand = expansion(mult = 0.1)) +
  facet_wrap(~common, scales = 'free')

p1<-
  ggplot(data = subset(metrics_df, species %in% sel_sp)) + 
    # Adding 2D density contours with color based on mean_temp
    geom_density_2d(aes(x = COG_depth, y = COG_lat, color = mean_temp, group = mean_temp), 
                    contour_var = "ndensity", bins = 100) +  # Change 'bins' to adjust the number of contours
    theme_bw() + 
    labs(x = 'bathymetrical COG (m)', y = 'latitudinal COG') + 
    theme(
      aspect.ratio = 1,
      text = element_text(size = 12),
      strip.background = element_blank(),
      strip.text = element_text(size = 12),
      panel.grid.minor = element_line(linetype = 'dashed')
    ) + 
    scale_color_gradientn(colors = custom_colors(20), name = 'SBT (°C)', 
                          guide = guide_colorbar(frame.colour = "black", ticks.colour = "black")) + 
    scale_size(name = 'abundance', breaks = c(min(summary_stats$mean_scaledbio), 
                                              mean(summary_stats$mean_scaledbio), 
                                              max(summary_stats$mean_scaledbio)), 
               labels = c('low', 'medium', 'high'), range = c(2, 10)) + 
    scale_x_continuous(expand = expansion(mult = 0.1)) + 
    scale_y_continuous(expand = expansion(mult = 0.1)) + 
    facet_wrap(~common, scales = 'free')
  
  agg_png(paste0('./figures slope/cog_sim1.png'), width = 6.5, height = 6.5, units = "in", res = 300)
  print(p1)
  dev.off()
  
  
  
agg_png(paste0('./figures slope/cog_sim.png'), width = 6.5, height = 6.5, units = "in", res = 300)
print(p)
dev.off()

ggplot(data = subset(metrics_df, species %in% sel_sp)) +
  #geom_point(aes(x = COG_depth, y = COG_lat, color = mean_temp)) +
  stat_density_2d(aes(x = COG_depth, y = COG_lat,group=Year,color=mean_temp), contour = TRUE) + 
  theme_bw() +
  labs(x = 'bathymetrical COG (m)', y = 'latitudinal COG (°)') +
  theme(aspect.ratio = 1,
        strip.background = element_blank(),
        strip.text = element_text(size = 12)) +
  scale_fill_gradientn(colors = custom_colors(100), name = 'SBT (°C)', 
                       guide = guide_colorbar(frame.colour = "black", ticks.colour = "black")) +
  scale_color_gradientn(colors = custom_colors(100), name = 'SBT (°C)', 
                        guide = guide_colorbar(frame.colour = "black", ticks.colour = "black")) +
  scale_x_continuous(expand = expansion(mult = 0.1)) +
  scale_y_continuous(expand = expansion(mult = 0.1)) +
  facet_wrap(~species, scales = 'free')

#scale to keep one legend
metrics_df1 <- metrics_df %>%
  group_by(species) %>%
  mutate(bio_scaled = scale(total_bio))

#plot using scaled bio color
ggplot(data=subset(metrics_df1,species %in% sel_sp))+
  geom_shadowtext(aes(x=COG_depth,y=COG_lat,color=bio_scaled,label=Year),
                  fontface='bold',bg.r = 0.05)+
  theme_bw()+
  labs(x='bathymetrical COG (m)',y='latitudinal COG')+
  theme(aspect.ratio = 1,strip.background = element_blank(),strip.text = element_text(size=12))+
  scale_color_viridis_c(name = 'relative\ntotal biomass',guide = guide_colorbar(frame.colour = "black", 
                                                                                ticks.colour = "black"))+
  facet_wrap(~species,scales='free')

#define a data frame for the rectangles
rect_data <- data.frame(xmin = c(2002, 2005.5, 2013.5),
                        xmax = c(2005.5, 2013.5, 2016),
                        fill = factor(c("warm", "cold", "warm")))

###########################
# EFFECTIVE AREA OCCUPIED
###########################

library(ggplot2)

# Create a data frame for the rectangles
rect_data <- data.frame(
  xmin = c(-Inf, 2013.5, 2005.5),
  xmax = c(2005.5, Inf, 2013.5),
  ymin = rep(-Inf, 3),
  ymax = rep(Inf, 3),
  fill = c("warm", "warm", "cold")  # Labels for the legend
)

#get common
metrics_df$common<-df_sp$common[match(metrics_df$species, df_sp$sci)]
metrics_df$common<-factor(metrics_df$common,levels=c("Alaska pollock","Greenland turbot","Pacific cod","Snow crab" ))

p <- 
  ggplot(data = metrics_df) + 
  # Use geom_rect to add rectangles with fill aesthetics
  geom_rect(data = rect_data, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill), alpha = 0.5) +
  #geom_line(aes(x = Year, y = total_bio / mean_dens / 1000, group = sim), color = 'black', alpha = 0.3) +  # Convert to thousands
  #geom_point(aes(x = Year, y = total_bio / mean_dens / 1000, group = sim), color = 'black', alpha = 0.3) +  # Convert to thousands
  geom_boxplot(aes(x = Year, y = total_bio / mean_dens / 1000, group = Year), color = 'black', alpha = 0.3) +  # Convert to thousands
    
  labs(x = '', y = 'effective area occupied (thousands km²)') +  # Adjust y-axis label
  theme_bw() + 
  scale_x_continuous(breaks = bold_years,minor_breaks = c(2002:2016), expand = c(0, 0.5)) + 
  scale_y_continuous(labels = comma) +  # Add commas to y-axis numbers
  theme(aspect.ratio = 1, 
        strip.background = element_blank(), 
        strip.text = element_text(size = 12), 
        axis.text.x = element_text(angle = 45, hjust = 0.7, vjust = 0.8),
        text = element_text(size = 12)) +   
  facet_wrap(~common, nrow = 2, scales = 'free') + 
  scale_fill_manual(values = c("warm" = "#cc1d1f", "cold" = "#1675ac"), 
                    name = "SBT regime") + 
  theme(legend.position = 'right')  # Position the legend on the right


#save env plot
agg_png(paste0('./figures slope/effective_area_sim.png'), width = 7.5, height = 7, units = "in", res = 300)
print(p)
dev.off()

###########################
# INTERDECILE DEPTH
###########################

#plot depth range
p <- 
ggplot(data = metrics_df) +  
  # Add smoothed density plot for 'depthq90 - depthq10' relative to 'mean_temp'
  # Add jittered points on top, colored by 'mean_temp'
  geom_density(aes(x = depthq90 - depthq10, fill = mean_temp, group = mean_temp), 
               color = "black", size = 0.5, adjust = 2) +  # adjust for smoothness
  geom_jitter(aes(x = depthq90 - depthq10, y = species, fill = mean_temp), 
              alpha = 0.5, shape = 21, size = 2, color = rgb(0, 0, 0, alpha = 0.2), 
              width = 0.01, height = 0.1) +  # Only jitter along the y-axis
 
  
  scale_x_continuous(expand = c(0, 0)) +  # Adjust x-axis scale
  

  
  facet_wrap(~common, scales = 'free') +
  scale_fill_gradientn(colors = custom_colors(20), name = 'SBT (°C)', 
                       guide = guide_colorbar(frame.colour = "black", ticks.colour = "black")) + 
  theme_bw() + 
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    strip.background = element_blank(),
    text = element_text(size = 12),
    strip.text = element_text(size = 12)
  ) +
  labs(x = 'depth at 90th percentile - depth at 10th percentile (m)')


  library(forcats)

p1<-
ggplot(data = metrics_df) +  
  # Reorder the 'common' variable by 'mean_temp' for sorting the violins
  geom_violin(aes(x = depthq90 - depthq10, y = fct_reorder(common, mean_temp), fill = mean_temp, group = mean_temp),  
              color = "black", size = 0.4, adjust = 2, scale = "width", position = position_dodge(width = 0.5), 
              trim = FALSE) +  # Adjusted scale for height
  
  scale_x_continuous() +  # Adjust x-axis scale
  
  facet_wrap(~common, scales = 'free') +  # Facet by 'common' variable
  
  scale_fill_gradientn(colors = custom_colors(20), name = 'SBT (°C)',  
                       guide = guide_colorbar(frame.colour = "black", ticks.colour = "black")) +  
  
  theme_bw() +  
  theme(
    axis.text.y = element_blank(),  # Hide y-axis text
    axis.ticks.y = element_blank(),  # Remove y-axis ticks
    axis.title.y = element_blank(),  # Remove y-axis title
    strip.background = element_blank(),  # Clean facet strip background
    text = element_text(size = 12),  # General font size
    strip.text = element_text(size = 12)  # Size for facet labels
  ) +  
  
  labs(x = 'Depth at 90th percentile - Depth at 10th percentile (m)')


#save env plot
agg_png(paste0('./figures slope/interdecile_depth_sim1.png'), width = 7, height = 3, units = "in", res = 300)
print(p)
dev.off()

#save env plot
agg_png(paste0('./figures slope/interdecile_depth_sim2.png'), width = 7, height = 3, units = "in", res = 300)
print(p1)
dev.off()
