# SETTINGS ####

#clean
rm(list = ls(all.names = TRUE)) 

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
             'tidyr',
             'knitr',
             'data.table',
             'ggshadow',
             'stringr',
             'devtools',
             'reshape2',
             'dplyr',
             'ggridges')

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

# Define a custom color scale function
custom_colors <- colorRampPalette(c("#1675ac", "white", "#cc1d1f"))

# directory
dir <- '/Users/daniel/Work/UW-NOAA/Adapting Monitoring to a Changing Seascape/'

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

# GRID TEMPERATURE AND REGIME PLOT ####

#load file grid
load('./data processed/grid_EBS_NBS.RData') #grid.ebs_year

#add num cell
n_cells<-nrow(grid.ebs_year)/length(unique(grid.ebs_year$Year))
grid.ebs_year$cell<-rep(1:n_cells,length(unique(grid.ebs_year$Year)))

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

# Calculate the average over regions
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

# Plot with regime temp and temp
p<-
  ggplot() +
    geom_rect(aes(xmin = 2002, xmax = 2005, ymin = -Inf, ymax = Inf, fill = "red")) +
    geom_rect(aes(xmin = 2014, xmax = 2016, ymin = -Inf, ymax = Inf, fill = "red")) +
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
    geom_vline(xintercept = 2016, color = 'black',linewidth=1, linetype = 'solid') +  # Custom gridlines
  annotation_custom(
    grob = linesGrob(gp = gpar(col = "black", lwd = 4)),  # Thicker top border
    xmin = 2002, xmax = 2016, ymin = -Inf, ymax = -Inf
  )+
  annotation_custom(
    grob = linesGrob(gp = gpar(col = "black", lwd = 4)),  # Thicker top border
    xmin = 2002, xmax = 2016, ymin = Inf, ymax = Inf
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
agg_png(paste0('./figures slope/temperature_regime.png'), width = 6, height = 2, units = "in", res = 300)
print(p)
dev.off()

# SMOOTHED MEAN DENSITY ACROSS DEPTH FOR COLD AND WARM YEARS ####
#load predicted densities (truth OM)
load(file = paste0("./output slope//species/dens_index_hist_OM_ebsnbs.RData")) 
ind_ebsnbs<-dens_index_hist_OM
load(file = paste0("./output slope//species/dens_index_hist_OM_slope.RData")) 
ind_slope<-dens_index_hist_OM

#store output
pred_all<-list()

#loop over selected species to get predicted densities
for (sp in sel_sp) {
  #sp<-sel_sp[1]
  #load true ebsnbs index
  
  ind_ebsnbs1<-as.data.frame(as.table(ind_ebsnbs[[sp]]$dens))
  colnames(ind_ebsnbs1) <- c("cell", 'category',"year", "dens")
  ind_ebsnbs1<-ind_ebsnbs1[,c('cell','year','dens')]
  ind_ebsnbs1$species<-sp
  ind_slope1<-as.data.frame(as.table(ind_slope[[sp]]$dens))
  colnames(ind_slope1) <- c("cell", "year", "dens")
  ind_slope1$species<-sp
  ind_slope1$cell<-as.factor(as.numeric(ind_slope1$cell)+length(unique(ind_ebsnbs1$cell)))
  
  pred_all[[sp]] <-rbind(ind_ebsnbs1,ind_slope1)
  
}

#join densities
ind_ebsnbsslope <- do.call(rbind, pred_all)
dens_all<-ind_ebsnbsslope[which(ind_ebsnbsslope$year %in% 2002:2016),]
#get data for each cell
depth_cell <- unique(
  grid.ebs_year[, c("cell", "depth_m")]
)
dens_all<-merge(dens_all,depth_cell,by='cell',all.x=TRUE)

#add year type, common sp name and depth bin
dens_all$year_type<-ifelse(dens_all$year %in% c(2002:2005,2014:2016), "warm",'cold')
#get common
dens_all$common<-df_sp$common[match(dens_all$species, df_sp$sci)]
dens_all$common<-factor(dens_all$common,levels=c("Alaska pollock","Greenland turbot","Pacific cod","Snow crab" ))
#get depth bin
dens_all$depth_bin <- cut(dens_all$depth_m, breaks = seq(0, 600, by = 25), include.lowest = TRUE)

#subset over depth (swept space)
dens_all<-subset(dens_all,depth_m<=600)

#mean density over species year and depth_bin
mean_dens_df <- dens_all %>%
  group_by(common, year_type, depth_bin) %>%
  summarise(
    mean_density = mean(dens, na.rm = TRUE),
    depth_mid = {
      nums <- as.numeric(unlist(str_extract_all(depth_bin, "\\d+\\.*\\d*")))
      mean(nums)
    },
    .groups = "drop"
  )

#plot mean density over depth_bin and cold/warm years
p<-
ggplot(mean_dens_df, aes(x = depth_mid, y = drop_units(mean_density), color = year_type)) +
  geom_point(alpha = 0.5) +  # raw points
  stat_smooth(
    method = "gam",
    formula = y ~ s(x, bs = "cs"),
    method.args = list(family = Gamma(link = "log")),  # positive predictions
    se = TRUE,    # show confidence interval
    aes(ymin = ..ymin.., ymax = ..ymax.., fill= year_type),alpha=0.2  # default, but just for clarity
  ) +
  labs(x = "depth (m)", y = "mean density (kg/km²)") +
  scale_y_continuous(limits = c(0, NA)) +  # clip axis below 0
  scale_color_manual(values = c(cold = "#1675ac", warm = "#cc1d1f"),
                     labels = c("cold", "warm"),
                     name = "SBT regime") +
  scale_fill_manual(values = c(cold = "#1675ac", warm = "#cc1d1f"),
                    labels = c("cold", "warm"),
                    name = "SBT regime") +
  theme_bw() +
  theme(strip.text       = element_text(size = 12),
        strip.background = element_blank(),
        text             = element_text(size = 12)) +
  facet_wrap(~common, scales = "free_y")

#seve figure
agg_png(paste0('./figures slope/density_depth.png'), width = 7, height = 4, units = "in", res = 300)
print(p)
dev.off()


# ABUNDANCE FRACTION ACROSS WARM/COLD YEARS OVER NBS + EBS ####
#save true ind
load(file = paste0("./output slope//model_based_EBSNBSBSS.RData"))  
head(true_ind)
#select speices and EBS+NBS and EBS to get NBS and EBS and perform fraction
true_ind1<-true_ind[,c('EBS','EBS+NBS'),sel_sp]
# Compute NBS
NBS_ind <- true_ind1[, "EBS+NBS", ] - true_ind1[, "EBS", ]

# Convert EBS to long df
df_EBS <- as.data.frame(as.table(true_ind1[, "EBS", ]))
names(df_EBS) <- c("year", "species", "EBS")

# Convert NBS to long df
df_NBS <- as.data.frame(as.table(NBS_ind))
names(df_NBS) <- c("year", "species", "NBS")

# Fix year type
df_EBS$year <- as.integer(as.character(df_EBS$year))
df_NBS$year <- as.integer(as.character(df_NBS$year))

#Merge NBS and EBS abundance estimates
big_df <- merge(df_EBS, df_NBS, by = c("year", "species"))

#pivot longer
ind_all2 <- big_df %>%
  pivot_longer(
    cols = c(EBS, NBS),
    names_to = "region",
    values_to = "value"
  )

#get fraction EBS-NBS
ind_all3 <- ind_all2 %>%
  group_by(species, year) %>%
  mutate(frac = value / sum(value)) %>%
  ungroup()

# Create legend rectangles with small buffer for edges
legend_rects <- data.frame(
  xmin = c(-Inf, 2005.5, 2013.5) - 0.5,
  xmax = c(2005.5, 2013.5, Inf) + 0.5,
  ymin = -Inf,
  ymax = Inf,
  fill = c('red', 'blue', 'red'),
  label = c('Period 1', 'Period 2', 'Period 1')
)

#plot abundance fraction over regions and years
p<-
  ggplot() +
  geom_rect(data = legend_rects, aes(xmin = xmin, xmax = xmax, 
                                     ymin = ymin, ymax = ymax, fill = label), 
            show.legend = TRUE,alpha=0.8) +
  geom_bar(data = ind_all3, aes(x = year, y = frac, fill = region), 
           stat = 'identity',alpha=0.8) +
  scale_fill_manual(values = c('EBS' = '#046407', 
'NBS' = '#B4AF46',
'Period 1' = '#cc1d1f',
'Period 2' = '#1675ac'),
breaks = c('EBS', 'NBS', 'Period 2','Period 1'),
labels = c('EBS', 'NBS', 'cold', 'warm'),
name = 'region and\nSBT regime') +
  scale_x_continuous(
    breaks = seq(1985, 2020, by = 5),
    limits = c(2001.5, 2016.5),
    expand = c(0.02, 0)  # small padding at edges
  )+
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

# CENTER OF GRAVITY (COG) OVER MEAN ANNUAL TEMPERATURE  ####

#load predicted densities (truth OM)
load(file = paste0("./output slope//species/dens_index_hist_OM_ebsnbs.RData")) 
ind_ebsnbs<-dens_index_hist_OM
load(file = paste0("./output slope//species/dens_index_hist_OM_slope.RData")) 
ind_slope<-dens_index_hist_OM

#store ooutputs
pred_all<-list()

#loop over species
for (sp in sel_sp) {
  #sp<-sel_sp[1]
  #load true ebsnbs index
  
  ind_ebsnbs1<-as.data.frame(as.table(ind_ebsnbs[[sp]]$dens))
  colnames(ind_ebsnbs1) <- c("cell", 'category',"year", "dens")
  ind_ebsnbs1<-ind_ebsnbs1[,c('cell','year','dens')]
  ind_ebsnbs1$species<-sp
  ind_slope1<-as.data.frame(as.table(ind_slope[[sp]]$dens))
  colnames(ind_slope1) <- c("cell", "year", "dens")
  ind_slope1$species<-sp
  ind_slope1$cell<-as.factor(as.numeric(ind_slope1$cell)+length(unique(ind_ebsnbs1$cell)))
  
  pred_all[[sp]] <-rbind(ind_ebsnbs1,ind_slope1)
  
}

#join densities
ind_ebsnbsslope <- do.call(rbind, pred_all)

#select years
dens_all<-ind_ebsnbsslope[which(ind_ebsnbsslope$year %in% 2002:2016),]

#merge with cell data from grid
depth_cell <- unique(
  grid.ebs_year[, c("cell", "depth_m",'Area_in_survey_km2','Lat','Lon','DepthGEBCO')]
)
dens_all<-merge(dens_all,depth_cell,by='cell',all.x=TRUE)

# add year type, common sp name
#get year type
dens_all$year_type<-ifelse(dens_all$year %in% c(2002:2005,2014:2016), "warm",'cold')
#get common
dens_all$common<-df_sp$common[match(dens_all$species, df_sp$sci)]
dens_all$common<-factor(dens_all$common,levels=c("Alaska pollock","Greenland turbot","Pacific cod","Snow crab" ))

#subset over depth (swept space)
dens_all<-subset(dens_all,depth_m<=600)

# Data frame to store results
metrics_df <- data.frame(matrix(NA, nrow = 0, ncol = 15))
names(metrics_df) <- c("Year","COG_lat","COG_lon","COG_depth","COG_temp",
                       "total_bio","mean_dens","mean_temp","depthq10","depthq90",
                       "q10","q20","q80","q90","species")

#years
yrs <- as.character(2002:2016)

#loop over years and species to calculate metrics (COG)
for (y in yrs) {
  
  cat("Year:", y, "\n")
  
  # Compute mean temperature of the grid for this year
  mean_temp <- mean(subset(grid.ebs_year, Year == as.numeric(y))$Temp, na.rm = TRUE)
  
  for (s in sel_sp) {
    
    cat(" Species:", s, "\n")
    
    # Filter by species and year
    dens1 <- subset(dens_all, species == s & year == as.numeric(y))
    dens1$dens <- drop_units(dens1$dens)
    
    if (nrow(dens1) == 0) next  # skip if no data
    
    # Calculate quantiles
    quantiles <- aggregate(dens ~ year, data = dens1, FUN = quantile, c(0.10,0.20,0.80,0.90))
    quantiles <- data.frame(as.matrix(quantiles))
    names(quantiles) <- c('Year','q10','q20','q80','q90')
    
    p1 <- quantiles$q10
    p2 <- quantiles$q90
    
    # Filter densities within 10th-90th percentile
    dens1$quantile <- ifelse(dens1$dens < p2 & dens1$dens > p1, 1, 0)
    
    # Sort by depth
    all_bio4 <- dens1[order(dens1$DepthGEBCO), ]
    
    # Calculate cumulative biomass
    all_bio4$bio <- all_bio4$dens * all_bio4$Area_in_survey_km2
    all_bio4$cumulative_biomass <- cumsum(all_bio4$bio)
    all_bio4$cumulative_biomass_scale <- all_bio4$cumulative_biomass / max(all_bio4$cumulative_biomass)
    
    # Depth 10th and 90th percentile
    ps <- c(0.10, 0.90)
    depth_10th <- all_bio4$DepthGEBCO[which.min(abs(all_bio4$cumulative_biomass_scale - ps[1]))]
    depth_90th <- all_bio4$DepthGEBCO[which.min(abs(all_bio4$cumulative_biomass_scale - ps[2]))]
    
    # Compute COG and total abundance
    centroids <- dens1 %>%
      group_by(year) %>%
      summarise(
        COG_lat = sum(dens*Lat, na.rm=TRUE)/sum(dens, na.rm=TRUE),
        COG_lon = sum(dens*Lon, na.rm=TRUE)/sum(dens, na.rm=TRUE),
        COG_depth = sum(dens*DepthGEBCO, na.rm=TRUE)/sum(dens, na.rm=TRUE),
        total_bio = sum(dens*Area_in_survey_km2, na.rm=TRUE),
        mean_dens = mean(dens, na.rm=TRUE),
        mean_temp = mean_temp,  # grid mean for this year
        depthq10 = depth_10th,
        depthq90 = depth_90th
      )
    
    # Combine with quantiles and species
    metrics <- cbind(centroids, quantiles[,c('q10','q20','q80','q90')], species = s)
    
    # Append to results
    metrics_df <- rbind(metrics_df, metrics)
    
  }
}

#copy original object
metrics_df1<-metrics_df

#get common
metrics_df$common<-df_sp$common[match(metrics_df$species, df_sp$sci)]
metrics_df$common<-factor(metrics_df$common,levels=c("Alaska pollock","Greenland turbot","Pacific cod","Snow crab" ))

#plot COG depth adn COG_lat relative to mean bottom temp from Bering10k
p1<-
  ggplot(metrics_df) + 
  geom_point(aes(x = COG_depth, y = COG_lat, color = mean_temp, group = mean_temp),size=3
  ) +  # Change 'bins' to adjust the number of contours
  geom_point(aes(x = COG_depth, y = COG_lat, group = mean_temp),color='black',shape=1,alpha=0.5,size=3) + 
  theme_bw() + 
  labs(x = 'bathymetrical COG (m)', y = 'latitudinal COG (°)') + 
  theme(
    aspect.ratio = 1,
    text = element_text(size = 12),
    strip.background = element_blank(),
    strip.text = element_text(size = 12),
    panel.grid.minor = element_line(linetype = 'dashed')
  ) + 
  scale_color_gradientn(colors = custom_colors(20), name = 'SBT (°C)',
                        guide = guide_colorbar(frame.colour = "black", ticks.colour = "black")) +
  scale_x_continuous(expand = expansion(mult = 0.1)) + 
  scale_y_continuous(expand = expansion(mult = 0.1)) + 
  facet_wrap(~common,scales='free')

#save plot
agg_png(paste0('./figures slope/COG_temp.png'), width = 5.5, height = 5.5, units = "in", res = 300)
print(p1)
dev.off()


# DEPTH RANGE AND NICHE OVER SPECIES AND YEAR ####

#get deopth range
metrics_df$depth_range <- metrics_df$depthq90 - metrics_df$depthq10
metrics_df$depth_range <- metrics_df$depthq90 - metrics_df$depthq10

# Define a custom color scale function
custom_colors <- colorRampPalette(c("#1675ac", "white", "#cc1d1f"))

metrics_df$depth_range <- metrics_df$depthq90 - metrics_df$depthq10

# Ensure Year is treated as factor for discrete y-axis
metrics_df$year <- factor(metrics_df$year)

#calculate scaled_range  
metrics_df <- metrics_df %>%
  group_by(common) %>%
  mutate(
    local_range = depthq90 - depthq10,
    scaled_range = scales::rescale(local_range, to = c(1, 6))  # Adjust size range here
  ) %>%
  ungroup()

#calculate depth range
metrics_df <- metrics_df %>%
  mutate(depth_range = depthq90 - depthq10)

#plot depth niche and range
p3<-
  ggplot(metrics_df, aes(y = year)) +
  geom_segment(
    aes(x = depthq10, xend = depthq90, yend = year, color = mean_temp,linetype="depth range (Q10–Q90)"),
    size = 1
  ) +
  geom_point(
    aes(x = local_range, y = year, size = scaled_range, fill = mean_temp,shape="depth niche (Q90 - Q10)"),
    stroke = 1,
    color = "black"
  ) +
  scale_y_discrete(limits = rev) +
  # Gradient scales (no guide specified here to avoid being overwritten)
  scale_color_gradientn(
    colors = custom_colors(20),
    name = "SBT (°C)"
  ) +
  scale_fill_gradientn(
    colors = custom_colors(20),
    name = "SBT (°C)"
  ) +
  scale_size_continuous(range = c(1, 4), guide = "none") +  # size legend off
  facet_wrap(~common, scales = "free_x") +
  labs(x = "depth (m)", y = NULL) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12),
    text = element_text(size = 12),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.box.just = "center",
    legend.spacing.x = unit(0.4, "cm"),
    legend.title = element_text(hjust = 0.5)
  ) +
  # Manual linetype and shape scales
  scale_linetype_manual(
    name = NULL,
    values = c("depth range (Q10–Q90)" = "solid"),
    labels = c(expression("depth range"~(Q[10]*","*Q[90])))
  ) +
  scale_shape_manual(
    name = NULL,
    values = c("depth niche (Q90 - Q10)" = 21),
    labels = c(expression("depth niche"~(Q[90] - Q[10])))
  ) +
  # Guides (now including frame/tick settings)
  guides(
    color = guide_colorbar(
      order = 1,
      title.position = "top",
      direction = "horizontal",
      title.hjust = 0.5,
      frame.colour = "black",
      ticks.colour = "black"
    ),
    fill = guide_colorbar(
      order = 1,
      title.position = "top",
      direction = "horizontal",
      title.hjust = 0.5,
      frame.colour = "black",
      ticks.colour = "black"
    ),
    linetype = guide_legend(
      order = 2,
      override.aes = list(
        size = 10,
        colour = "black"
      )
    ),
    shape = guide_legend(
      order = 3,
      override.aes = list(size = 5)
    )
  )


#save range depth cold/warm
agg_png(paste0('./figures slope/depth_range.png'), width = 7.5, height = 6.5, units = "in", res = 300)
print(p3)
dev.off()

#additional code for - stratification figure ####

# Dummy data for the plot (replace with your actual data)
df <- data.frame(
  Year = 2000:2020,
  Value = rnorm(21)
)

# Define rectangles and labels
rects <- data.frame(
  xmin = c(2002, 2005, 2002, 2014),
  xmax = c(2016, 2014, 2005, 2016),
  ymin = c(0, 0, 0, 0),
  ymax = c(1, 1, 1, 1),
  fill = c("gray90", "#1675ac", "#cc1d1f", "#cc1d1f"),
  label = c(NA, "cold", "warm", "warm")
)

# Center-top label position
rects$label_x <- (rects$xmin + rects$xmax) / 2
rects$label_y <- rects$ymax - 0.05

p1<-
  # Plot
  ggplot() +
  # Background rectangles — add first
  geom_rect(
    data = rects,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill),
    inherit.aes = FALSE
  ) +
  
  # Identity fill to use exact colors
  scale_fill_identity() +
  
  # Text labels for cold/warm only
  geom_text(
    data = subset(rects, !is.na(label)),
    aes(x = label_x, y = label_y, label = label),
    size = 4.5,
    fontface = "bold",
    color = "white"
  ) +
  geom_vline(xintercept = 2005,linewidth =0.5,linetype='dashed',color='grey20')+
  geom_vline(xintercept = 2014,linewidth =0.5,linetype='dashed',color='grey20')+
  
  # Axis and grid options
  scale_x_continuous(breaks = 2002:2016) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_line(),
    axis.title.y = element_blank(),panel.grid.major.y = element_blank(),panel.grid.minor = element_blank(),panel.grid.major.x = element_blank(),
    #panel.grid.major = element_line(color = "gray80"),
    #panel.grid.minor = element_line(color = "gray90"),
    panel.ontop = TRUE  # <- gridlines rendered over geom_rect
  ) +
  labs(x = "year", y = "",title='environmentally informed stratification')


library(ggplot2)

rects <- data.frame(
  xmin = 2002,
  xmax = 2016,
  ymin = 0,
  ymax = 1,
  fill = "gray30",
  label = "static",
  label_x = (2002 + 2016) / 2,
  label_y = 1 - 0.05
)

p2<-
  ggplot() +
  geom_rect(
    data = rects,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill),
    inherit.aes = FALSE
  ) +
  scale_fill_identity() +
  scale_x_continuous(breaks = 2002:2016) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.x = element_line(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.ontop = TRUE
  ) +
  labs(x = "year", y = "",title='static stratification')


agg_png(paste0('./figures slope/flexible_stratification.png'), width = 7, height = 7, units = "in", res = 300)
cowplot::plot_grid(p2,p1,ncol = 1)
dev.off()

