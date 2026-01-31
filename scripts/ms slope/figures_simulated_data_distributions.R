# Settings ####

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
             'mgcv',
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
# dir <- '/Users/daniel/Work/Adapting Monitoring to a Changing Seascape/'
# 
# #setwd
# setwd(dir)

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
load('data/extrapolation_grids/northern_bering_sea_grid.rda')
load('data/extrapolation_grids/eastern_bering_sea_grid.rda')
load('data/extrapolation_grids/bering_sea_slope_grid.rda')
colnames(bering_sea_slope_grid)[4]<-'Stratum'
bering_sea_slope_grid$Stratum<-NA
grid<-as.data.frame(rbind(data.frame(northern_bering_sea_grid,region='NBS'),
                          data.frame(eastern_bering_sea_grid,region='EBS'),
                          data.frame(bering_sea_slope_grid,region='SBS')))
grid$cell<-1:nrow(grid)
grid2<-grid
grid<-grid2[,c('Lat','Lon','region','cell')]

bold_years<-c(2002,2004,2008,2010,2012,2016)

# grid temperature and regime plot ####
# 
# #load file grid
# load('data/data_processed/grid_EBS_NBS.RData') #grid.ebs_year
# 
# # Define a custom function to calculate mean and SD
# calc_stats <- function(x) {
#   c(mean = mean(x, na.rm = TRUE),
#     SD = sd(x, na.rm = TRUE))
# }
# 
# # Use aggregate to calculate statistics for Temp by Year and region
# temp_region <- aggregate(Temp ~ Year + region, data = grid.ebs_year, FUN = calc_stats)
# 
# # Convert the output to a dataframe
# temp_region <- do.call(data.frame, temp_region)
# names(temp_region) <- c('year', 'region', 'mean_temp', 'SD')
# 
# # Calculate the average for all regions
# alltemp <- aggregate(mean_temp ~ year, data = temp_region, FUN = mean)
# alltemp$region <- 'all'
# alltemp$SD <- aggregate(SD ~ year, data = temp_region, FUN = mean)$SD
# 
# # Combine the datasets
# temp_region <- rbind(temp_region, alltemp)
# 
# # Scaling values by group
# temp_region1 <- temp_region %>%
#   group_by(region) %>%
#   mutate(scaledtemp = scale(mean_temp)) %>%
#   ungroup()
# 
# # Classify by cold/warm/normal years
# temp_region1$typeyear <- ifelse(temp_region1$scaledtemp > 1, 
#                                 "warm", ifelse(temp_region1$scaledtemp < -1, 
#                                                "cold", "normal"))
# 
# # Create custom labels function
# custom_labels <- function(x) {
#   sapply(x, function(year) {
#     if (year %in% bold_years) {
#       paste0("**", year, "**")
#     } else {
#       as.character(year)
#     }
#   })
# }
# 
# # Plot with SD
# p<-
#   ggplot() +
#     geom_rect(aes(xmin = 2002, xmax = 2005, ymin = -Inf, ymax = Inf, fill = "red")) +
#     geom_rect(aes(xmin = 2014, xmax = 2016, ymin = -Inf, ymax = Inf, fill = "red")) +
#     geom_rect(aes(xmin = 2005, xmax = 2014, ymin = -Inf, ymax = Inf, fill = "blue")) +
#     geom_vline(xintercept = seq(1985, 2020, by = 5), color = rgb(0, 0, 0,90, maxColorValue = 285), linetype = 'solid') +  # Custom gridlines
#     geom_vline(xintercept = setdiff(1982:2022,seq(1985, 2020, by = 5)), color = rgb(0, 0, 0,90, maxColorValue = 285), linetype = 'solid',size=0.2) +  # Custom gridlines
#     geom_hline(yintercept = c(0:5), color = rgb(0, 0, 0,90, maxColorValue = 285), linetype = 'solid',size=0.2) +  # Custom gridlines
#     geom_ribbon(data = subset(temp_region1, region == 'all' & year %in% c(1982:2022)),
#                 aes(x = year, ymin = mean_temp - SD, ymax = mean_temp + SD), fill = 'gray', alpha = 0.4) +
#     geom_hline(yintercept = colMeans(subset(temp_region1, region == 'all' & year %in% 1982:2022)[,'mean_temp']), alpha = 0.5, linetype = 'dashed') +
#     geom_line(data = subset(temp_region1, region == 'all' & year %in% 1982:2022),
#               aes(x = year, y = mean_temp, color = region)) +
#     #geom_point(data = subset(temp_region1, region == 'all' & year %in% 1982:2022),
#     #           aes(x = year, y = mean_temp, color = region),size=.7) +
#     geom_vline(xintercept = 2002, color = 'black',linewidth=1, linetype = 'solid') +  # Custom gridlines
#     geom_vline(xintercept = 2016, color = 'black',linewidth=1, linetype = 'solid') +  # Custom gridlines
#   annotation_custom(
#     grob = linesGrob(gp = gpar(col = "black", lwd = 4)),  # Thicker top border
#     xmin = 2002, xmax = 2016, ymin = -Inf, ymax = -Inf
#   )+
#   annotation_custom(
#     grob = linesGrob(gp = gpar(col = "black", lwd = 4)),  # Thicker top border
#     xmin = 2002, xmax = 2016, ymin = Inf, ymax = Inf
#   )+
#    # annotation_custom(
#    #        grob = linesGrob(gp = gpar(col = "black", lwd = 4)),  # Thicker top border
#    #          xmin = 2016, xmax = 2016, ymin = -Inf, ymax = Inf
#    #     )+
#     scale_fill_manual(values = c("red" = "#cc1d1f", "blue" = "#1675ac"),
#                       labels = c("cold", "warm"),
#                       name = "SBT regime") +
#     scale_color_manual(values = c('all' = 'black'),
#                        breaks = c('all'),
#                        labels = c('Bering\nSea'),name='',guide=FALSE) +
#     theme_bw() +
#     scale_x_continuous(breaks = c(1985,1990,1995,2000,bold_years,2020),
#                        minor_breaks = c(1982:2022), 
#                        labels = custom_labels, limits = c(1982, 2022),
#                        expand = c(0,0)) +
#     theme(axis.text.x = element_markdown(angle = 45,hjust=1),
#           axis.text.y = element_text(),text = element_text(size = 12),
#           panel.grid = element_blank()) +
#     labs(x = '', y = 'mean SBT')
# 
# 
# #save env plot
# agg_png(paste0('.figures/slope/temperature_regime2.png'), width = 6, height = 2, units = "in", res = 300)
# print(p)
# dev.off()

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


# # Depth distribution occurrence #####
# 
# #load file observed dataframe input 
# load('data/data_processed/obs_df.RData') #obs_df
# 
# #survey as factors and rename
# obs_df$survey_name<-as.factor(obs_df$survey_name)
# levels(obs_df$survey_name)<-c('EBSshelf','EBSslope','NBS')
# 
# #only presences, EBSshelf+EBSslope and selected species
# obs_df<-subset(obs_df,cpue_kgkm2!=0 &
#                  survey_name %in% c('EBSshelf','EBSslope','NBS') &
#                  species %in% sel_sp)
# 
# #filter by cold and warm years
# #obs_df1<-obs_df[which(obs_df$year %in% c(cyrs,wyrs)),]
# obs_df1<-obs_df[which(obs_df$year %in% bold_years),]
# 
# #year type
# obs_df1$year_type<-ifelse(obs_df1$year %in% c(2002,2004,2016), "warm",'cold')
# 
# #filtered to <600m
# filt_obs_df1<-subset(obs_df1,depth_m<=600)
# 
# #get common
# filt_obs_df1$common<-df_sp$common[match(filt_obs_df1$species, df_sp$sci)]
# filt_obs_df1$common<-factor(filt_obs_df1$common,levels=c("Alaska pollock","Greenland turbot","Pacific cod","Snow crab" ))
# 
# #plot Kernel density estimation for depth distributions
# p<-
#   ggplot(filt_obs_df1, aes(x = depth_m, fill = year_type)) +
#     geom_density(alpha = 0.5) +
#     labs(x = 'depth (m)', y = 'probability of occurrence')+ 
#          #title = 'Empirical depth distribution of occurrence in Warm vs. Cold Years') +
#     scale_fill_manual(values = c('cold' = '#1675ac',
#                                  'warm' = '#cc1d1f'),
#                       labels = c("cold", "warm"),
#                       name = 'SBT regime') +
#     theme_bw()+
#     scale_x_continuous(limits = c(0,600))+
#     theme(strip.text = element_text(size=12),strip.background = element_blank(),
#           text= element_text(size=12))+
#     facet_wrap(~common)
# 
# 
# #data.table
# sum_df<-obs_df
# sum_df1<-aggregate(lat_start ~ year, sum_df,FUN=length)
# sum_df1$lat_start<-sum_df1$lat_start
# # Convert survey names to factors and rename
# obs_df$survey_name <- as.factor(obs_df$survey_name)
# levels(obs_df$survey_name) <- c('EBSshelf', 'EBSslope', 'NBS')
# 
# # Filter to selected surveys, species, and <600m depth
# obs_df1 <- obs_df %>%
#   filter(survey_name %in% c('EBSshelf', 'EBSslope', 'NBS') & 
#            species %in% sel_sp & depth_m <= 600 & 
#            year %in% bold_years)
# 
# # Assign year type (cold vs. warm)
# obs_df1 <- obs_df1 %>%
#   mutate(year_type = ifelse(year %in% c(2002, 2004, 2016), "warm", "cold"),
#          year_type = factor(year_type, levels = c("cold", "warm")))  # Ensure proper factor levels
# 
# # Map species scientific names to common names
# obs_df1$common <- factor(df_sp$common[match(obs_df1$species, df_sp$sci)], 
#                          levels = c("Alaska pollock", "Greenland turbot", "Pacific cod", "Snow crab"))
# 
# # Ensure no NA values in cpue_kgkm2 and depth_m
# obs_df1 <- obs_df1 %>% filter(!is.na(cpue_kgkm2) & !is.na(depth_m))
# 
# # Bin depths
# obs_df1$depth_bin <- cut(obs_df1$depth_m, breaks = seq(0, 600, by = 25), include.lowest = TRUE)
# 
# #get probs for each
# prob_df <- obs_df1 %>%
#   group_by(common, year_type, depth_bin) %>%
#   summarise(
#     n_present = sum(cpue_kgkm2 > 0),
#     n_total = n(),
#     prob_presence = n_present / n_total,
#     # Extract both bounds and compute midpoint
#     depth_mid = {
#       nums <- as.numeric(unlist(str_extract_all(depth_bin, "\\d+\\.*\\d*")))
#       mean(nums)
#     },
#     .groups = "drop"
#   )
# 
# # Plot smoothed probability of presence
# p <- 
#   ggplot(prob_df, aes(x = depth_mid, y = prob_presence, color = year_type)) +
#   geom_point(alpha = 0.5) +  # Show raw probabilities
#   geom_smooth(method = "loess", se = FALSE) +  # Smooth probability trend
#     coord_cartesian(ylim = c(0, 1))+
#   labs(x = 'depth (m)', y = 'probability of occurrence') +
#   #scale_y_continuous(limits = c(0, 1)) +  # Ensure probability scale from 0 to 1
#   scale_x_continuous(limits = c(0, 600),expand = c(0,0),breaks = c(100,300,500))+#,) +
#   scale_color_manual(values = c('cold' = '#1675ac', 'warm' = '#cc1d1f'),
#                      labels = c("cold", "warm"),
#                      name = 'SBT regime') +
#   #scale_y_continuous( )+
#   theme_bw() +
#   theme(strip.text = element_text(size = 12),
#         strip.background = element_blank(),
#         text = element_text(size = 12)) +
#   facet_wrap(~common)
# 
# print(p)
# 
# #save env plot
# agg_png(paste0('.figures/slope/depth_distribution_occurrence1.png'), width = 7, height = 4, units = "in", res = 300)
# print(p)
# dev.off()
# 
# # Ensure year_type and common are treated as factors
# prob_df <- prob_df %>%
#   mutate(year_type = factor(year_type),
#          common = factor(common))
# 
# # Reshape to wide format: one column for warm, one for cold
# prob_wide <- prob_df %>%
#   dplyr::select(common, year_type, depth_bin, prob_presence) %>%
#   pivot_wider(names_from = year_type, values_from = prob_presence)
# 
# # Run Wilcoxon signed-rank test for each species (paired across depth bins)
# test_results_prob <- prob_wide %>%
#   group_by(common) %>%
#   filter(!is.na(warm) & !is.na(cold)) %>%
#   summarise(
#     wilcox_p = tryCatch(wilcox.test(warm, cold)$p.value, error = function(e) NA),
#     .groups = "drop"
#   )
# 
# # View results
# print(test_results_prob)
# 
# 
# # Initialize empty result data frame
# test_results <- data.frame(
#   species = character(),
#   n_warm = numeric(),
#   n_cold = numeric(),
#   ks_p = numeric(),
#   wilcox_p = numeric(),
#   stringsAsFactors = FALSE
# )
# 
# # Only include positive occurrences
# pres_df <- obs_df1 %>%
#   filter(cpue_kgkm2 > 0 & year_type %in% c("cold", "warm"))
# 
# library(dplyr)
# 
# prob_df <- prob_df %>%
#   mutate(
#     # Remove brackets and split by comma
#     bounds = gsub("\\[|\\]|\\(|\\)", "", depth_bin),
#     depth_low = as.numeric(sub(",.*", "", bounds)),
#     depth_high = as.numeric(sub(".*,", "", bounds)),
#     depth_mid = (depth_low + depth_high) / 2
#   ) %>%
#   select(-bounds)
# 
# library(tidyr)
# 
# prob_wide <- prob_df %>%
#   select(common, year_type, depth_mid, prob_presence) %>%
#   pivot_wider(names_from = year_type, values_from = prob_presence)
# 
# library(purrr)
# 
# test_results_prob <- prob_wide %>%
#   group_by(common) %>%
#   summarise(
#     ks_p = tryCatch(ks.test(warm, cold)$p.value, error = function(e) NA),
#     wilcox_p = tryCatch(wilcox.test(warm, cold, paired = TRUE)$p.value, error = function(e) NA),
#     .groups = "drop"
#   )
# 
# 
# # Replace with the species you're testing
# for (sp in sel_sp) {
# 
#   #sp<-sel_sp[1]
# depth_warm <- pres_df %>% filter(species == sp, year_type == "warm") %>% pull(depth_m)
# depth_cold <- pres_df %>% filter(species == sp, year_type == "cold") %>% pull(depth_m)
# 
# # Only test if both groups have enough data
# if (length(depth_warm) >= 10 && length(depth_cold) >= 10) {
#   ks_p <- ks.test(depth_warm, depth_cold)$p.value
#   wilcox_p <- wilcox.test(depth_warm, depth_cold)$p.value
# } else {
#   ks_p <- NA
#   wilcox_p <- NA
# }
# 
# test_results <- rbind(test_results, data.frame(
#   species = sp,
#   n_warm = length(depth_warm),
#   n_cold = length(depth_cold),
#   ks_p = ks_p,
#   wilcox_p = wilcox_p
# ))
# }
# 
# test_results <- test_results %>%
#   mutate(
#     ks_fdr = p.adjust(ks_p, method = "fdr"),
#     wilcox_fdr = p.adjust(wilcox_p, method = "fdr")
#   )

# Merge p-values with species names
test_results_named <- test_results_prob %>%
  left_join(df_sp, by = c("common" = "common"))  # assumes species in test_results is sci

# Create labels with p-values
label_df <- test_results_prob %>%
  mutate(
    wilcox_label = case_when(
      is.na(wilcox_p) ~ "n/a",
      wilcox_p < 0.001 ~ "< 0.001",
      TRUE ~ formatC(wilcox_p, format = "f", digits = 3)
    ),
    label = paste0("W test\np = ", wilcox_label)
  ) %>%
  dplyr::select(common, label)

p <- 
ggplot(prob_df, aes(x = depth_mid, y = prob_presence, color = year_type)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = 'depth (m)', y = 'probability of occurrence') +
  scale_x_continuous(limits = c(0, 600), expand = c(0, 0), breaks = c(100, 300, 500)) +
  scale_color_manual(values = c('cold' = '#1675ac', 'warm' = '#cc1d1f'),
                     labels = c("cold", "warm"),
                     name = 'SBT regime') +
  theme_bw() +
  theme(strip.text = element_text(size = 12),
        strip.background = element_blank(),
        text = element_text(size = 12)) +
  facet_wrap(~common) #+
  # geom_text(
  #   data = label_df,
  #   aes(x = 75, y = 0.50, label = label),
  #   inherit.aes = FALSE,
  #   size = 3,
  #   hjust = 0,
  #   vjust = 1,
  #   lineheight = 0.8  # Reduce line spacing
  # )


#save env plot
agg_png(paste0('.figures/slope/depth_distribution_occurrence3.png'), width = 7, height = 4, units = "in", res = 300)
print(p)
dev.off()



# Depth distribution occurrence pred ##### ----

#change to predicted from the OMs (so open each OM for these 4 species, get predicted values with depth and associated temp and plot)
load('data/data_processed/dens_sel.RData')
dimnames(dens_all)
aggregate(Lat ~ Year + species, dens_all,FUN=length)


#obs_df1<-obs_df[which(obs_df$year %in% c(cyrs,wyrs)),]
dens_all<-dens_all[which(dens_all$Year %in% 2002:2016),]
aggregate(Lat ~ Year + species, dens_all,FUN=length)

#check
unique(dens_all$region)
subset(dens_all,Year=='2003' & region=='EBSslope')

#year type
dens_all$year_type<-ifelse(dens_all$Year %in% c(2002:2005,2014:2016), "warm",'cold')

#filtered to <600m
filt_obs_df1<-subset(obs_df1,depth_m<=600)

#get common
dens_all$common<-df_sp$common[match(dens_all$species, df_sp$sci)]
dens_all$common<-factor(dens_all$common,levels=c("Alaska pollock","Greenland turbot","Pacific cod","Snow crab" ))

dens_all$depth_bin <- cut(dens_all$depth_m, breaks = seq(0, 600, by = 25), include.lowest = TRUE)

#get probs for each
prob_df <- dens_all %>%
  group_by(common, year_type, depth_bin) %>%
  summarise(
    n_present = sum(dens > 0),
    n_total = n(),
    prob_presence = n_present / n_total,
    # Extract both bounds and compute midpoint
    depth_mid = {
      nums <- as.numeric(unlist(str_extract_all(depth_bin, "\\d+\\.*\\d*")))
      mean(nums)
    },
    .groups = "drop"
  )

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

#get mean density (instead of prob of presence because predicted data have a lot of 0.0000000001, why? thats because of additional error?)
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

#p <- 
  ggplot(mean_dens_df, aes(x = depth_mid, y = mean_density, color = year_type)) +
  geom_point(alpha = 0.5) +  # Raw mean densities
  geom_smooth(method = "loess", se = TRUE) +  # Smooth trend
  labs(x = 'depth (m)', y = 'mean density (kg/km²)') +
  #scale_x_continuous(limits = c(0, 600), expand = c(0, 0), breaks = c(100, 300, 500)) +
  scale_color_manual(values = c('cold' = '#1675ac', 'warm' = '#cc1d1f'),
                     labels = c("cold", "warm"),
                     name = 'SBT regime') +
  theme_bw() +
  theme(strip.text = element_text(size = 12),
        strip.background = element_blank(),
        text = element_text(size = 12)) +
  facet_wrap(~common,scales='free_y')

  p<-
  ggplot(mean_dens_df,
         aes(x = depth_mid, y = mean_density, color = year_type)) +
    geom_point(alpha = 0.5) +                               # raw points (original scale)
    stat_smooth(
      aes(y = mean_density),                            # use positive version for the fit
      method       = "gam",
      formula      = y ~ s(x, bs = "cs"),                  # cubic spline
      method.args  = list(family = Gamma(link = "log")),   # guarantees > 0 predictions
      se           = FALSE
    ) +
    labs(x = "depth (m)", y = "mean density (kg/km²)") +
    scale_y_continuous(limits = c(0,NA))+
    scale_color_manual(values = c(cold = "#1675ac", warm = "#cc1d1f"),
                       labels  = c("cold", "warm"),
                       name    = "SBT regime") +
    theme_bw() +
    theme(strip.text       = element_text(size = 12),
          strip.background = element_blank(),
          text             = element_text(size = 12)) +
    facet_wrap(~common, scales = "free_y")
  
  p <- ggplot(mean_dens_df, aes(x = depth_mid, y = mean_density, color = year_type)) +
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
  
  p
  
  
#then add wilcoxon densities from observed data

library(dplyr)
library(tidyr)
library(purrr)
library(broom)

# df <- your data frame


## 1. Wilcoxon test per species ----
wilcox_df <- mean_dens_df %>%                                    # your plotting data
  distinct(common, year_type, depth_bin, mean_density) %>%       # keep 1 row per bin/regime
  pivot_wider(names_from = year_type, values_from = mean_density) %>%
  drop_na(cold, warm) %>%
  group_by(common) %>%
  summarise(
    W_test = list(wilcox.test(warm, cold, paired = TRUE, exact = FALSE)),
    .groups = "drop"
  ) %>%
  mutate(
    p = map_dbl(W_test, ~ .x$p.value),
    p_label = case_when(
      is.na(p)           ~ "n/a",
      p < 0.001          ~ "< 0.001",
      TRUE               ~ paste0('= ',formatC(p, format = "f", digits = 3))
    ),
    label = paste0("W test\np ", p_label)
  ) %>%
  dplyr::select(common, label)

## 2. Merge labels onto plotting data ----
plot_df <- mean_dens_df %>% 
  left_join(wilcox_df, by = "common")


## 3. Build the plot ----
p0<-
ggplot(plot_df, aes(x = depth_mid, y = mean_density, color = year_type)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(x = 'depth (m)', y = 'mean density (kg/km²)') +
  scale_x_continuous(limits = c(0, 600), expand = c(0, 0),
                     breaks = c(100, 300, 500)) +
  scale_color_manual(values = c(cold = '#1675ac', warm = '#cc1d1f'),
                     name = 'SBT regime') +
  theme_bw() +
  theme(
    strip.text = element_text(size = 12),
    strip.background = element_blank(),
    text = element_text(size = 12)
  ) +
  facet_wrap(~ common, scales = 'free_y') +
  # put label in the upper‑right of each facet
  geom_text(
    data = . %>% 
      group_by(common) %>%                       # one row per facet
      summarise(
        depth_mid = max(depth_mid, na.rm = TRUE),      # right edge
        mean_density = max(mean_density, na.rm = TRUE) # top edge
      ) %>% 
      left_join(wilcox_df, by = "common"),
    aes(x = depth_mid, y = mean_density, label = label),
    hjust = 1, vjust = 1, size = 3, color = "black",lineheight = 0.8
  )


#save env plot
agg_png(paste0('.figures/slope/depth_distribution_density3.png'), width = 7, height = 4, units = "in", res = 300)
print(p)
dev.off()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#load file observed dataframe input 
load('data/data_processed/obs_df.RData') #obs_df

#survey as factors and rename
obs_df$survey_name<-as.factor(obs_df$survey_name)
levels(obs_df$survey_name)<-c('EBSshelf','EBSslope','NBS')

# Filter to selected surveys, species, and <600m depth
obs_df1 <- obs_df %>%
  filter(survey_name %in% c('EBSshelf', 'EBSslope', 'NBS') & 
           species %in% sel_sp & depth_m <= 600 & 
           year %in% bold_years)

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


mean_dens_obs <- obs_df1 %>%
  group_by(common, year_type, depth_bin) %>%
  summarise(
    mean_density = mean(cpue_kgkm2, na.rm = TRUE),
    depth_mid = {
      nums <- as.numeric(unlist(str_extract_all(depth_bin, "\\d+\\.*\\d*")))
      mean(nums)
    },
    .groups = "drop"
  )


p <- ggplot(mean_dens_obs, aes(x = depth_mid, y = mean_density, color = year_type)) +
  geom_point(alpha = 0.5) +  # Raw mean densities
  geom_smooth(method = "loess", se = FALSE) +  # Smooth trend
  labs(x = 'depth (m)', y = 'mean density (kg/km²)') +
  scale_x_continuous(limits = c(0, 600), expand = c(0, 0), breaks = c(100, 300, 500)) +
  scale_color_manual(values = c('cold' = '#1675ac', 'warm' = '#cc1d1f'),
                     labels = c("cold", "warm"),
                     name = 'SBT regime') +
  theme_bw() +
  theme(strip.text = element_text(size = 12),
        strip.background = element_blank(),
        text = element_text(size = 12)) +
  facet_wrap(~common,scales='free_y')

p

#get wilcoxon on observations!
# Ensure year_type and common are treated as factors
obs_df1 <- obs_df1 %>%
  mutate(year_type = factor(year_type),
         common = factor(common))

library(dplyr)
library(tidyr)
library(stringr)

# Step 1: Summarize mean density per species, year_type, depth_bin
obs_df2 <- obs_df1 %>%
  group_by(common, year_type, depth_bin) %>%
  summarise(
    mean_dens = mean(cpue_kgkm2, na.rm = TRUE),
    depth_mid = {
      nums <- as.numeric(unlist(str_extract_all(depth_bin, "\\d+\\.*\\d*")))
      mean(nums)
    },
    .groups = "drop"
  )

# Step 2: Pivot wider for mean density per depth bin (warm and cold columns)
obs_wide <- obs_df2 %>%
  dplyr::select(common, depth_bin, mean_dens, year_type) %>%
  pivot_wider(names_from = year_type, values_from = mean_dens)

# Step 3: Wilcoxon test (paired by depth_bin) for mean density differences warm vs cold per species
test_results_density <- obs_wide %>%
  filter(!is.na(warm) & !is.na(cold)) %>%
  group_by(common) %>%
  summarise(
    wilcox_p_density = tryCatch(
      wilcox.test(warm, cold, paired = TRUE, exact = FALSE)$p.value,
      error = function(e) NA
    )
    ,
    .groups = "drop"
  )

# Step 4: Join species info and format labels
test_results_density <- test_results_density %>%
  left_join(df_sp, by = "common") %>%
  mutate(
    wilcox_label = case_when(
      is.na(wilcox_p_density) ~ "n/a",
      wilcox_p_density < 0.001 ~ "< 0.001",
      TRUE ~ formatC(wilcox_p_density, format = "f", digits = 3)
    ),
    label = paste0("Wilcoxon test\np = ", wilcox_label)
  ) %>%
  dplyr::select(common, label)
# Merge p-values with species names
test_results <- test_results_density %>%
  left_join(df_sp, by = c("common" = "common"))  # assumes species in test_results is sci

# Create labels with p-values
# Create labels with p-values from density comparison
label_df <- test_results %>%
  mutate(
    wilcox_label = case_when(
      is.na(wilcox_p_density) ~ "n/a",
      wilcox_p_density < 0.001 ~ "< 0.001",
      TRUE ~ formatC(wilcox_p_density, format = "f", digits = 3)
    ),
    label = paste0("W test\np = ", wilcox_label)
  ) %>%
  dplyr::select(common, label)


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

top_y_df <- mean_dens_df %>%
  group_by(common) %>%
  summarise(y = max(mean_density, na.rm = TRUE), .groups = "drop") %>%
  mutate(x = 300)  # Midpoint of depth axis
label_df_pos <- label_df %>%
  left_join(top_y_df, by = "common")



p <- 
  ggplot(mean_dens_df, aes(x = depth_mid, y = mean_density, color = year_type)) +
  geom_point(alpha = 0.5) +  # Raw mean densities
  geom_smooth(method = "loess", se = FALSE) +  # Smooth trend
  labs(x = 'depth (m)', y = 'mean density (kg/km²)') +
  scale_x_continuous(limits = c(0, 600), expand = c(0, 0), breaks = c(100, 300, 500)) +
  scale_color_manual(values = c('cold' = '#1675ac', 'warm' = '#cc1d1f'),
                     labels = c("cold", "warm"),
                     name = 'SBT regime') +
  theme_bw() +
  theme(strip.text = element_text(size = 12),
        strip.background = element_blank(),
        text = element_text(size = 12)) +
  facet_wrap(~common,scales='free_y')+


#then add wilcoxon densities from observed data
  geom_text(
    data = label_df_pos,
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    size = 3,
    hjust = 0.5,  # Center horizontally
    vjust = 1,    # Top of panel
    lineheight = 0.8
  )

#save env plot
agg_png(paste0('.figures/slope/depth_distribution_occurrence5.png'), width = 7, height = 4, units = "in", res = 300)
print(p)
dev.off()

# fraction of abundance ####

#load file index of selected species
load('data/data_processed/ind_sel.RData') #ind_all

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
agg_png(paste0('.figures/slope/abundance_fraction.png'), width = 7, height = 4, units = "in", res = 300)
print(p)
dev.off()

#load files density slope and shelf data 
load('./output/slope/species/ms_sim_dens_all.RData')  #sim_dens1
dimnames(sim_dens1)

#subset years and species densities
dens<-sim_dens1[,sel_sp,as.character(2002:2022),]
dimnames(dens)

#load grid of NBS and EBS
load('data/extrapolation_grids/northern_bering_sea_grid.rda')
load('data/extrapolation_grids/eastern_bering_sea_grid.rda')
#load('data/extrapolation_grids/bering_sea_slope_grid.rda')
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
agg_png(paste0('.figures/slope/abundance_fraction_sim.png'), width = 7, height = 4, units = "in", res = 300)
print(p)
dev.off()

# COG observed #######

# Load data
load('./output/slope//obs_df.RData')

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
obs_df1 <- obs_df1 %>% filter(!is.na(cpue_kgkm2) & !is.na(depth_m) & year %in% bold_years)

#df to store results
metrics_df<-data.frame(matrix(NA,nrow = 0,ncol=6))
names(metrics_df)<-c("year","COG_lat","COG_lon","COG_depth",'mean_temp',"species")

#shelf slope spp
#shelfslope_spp<-unique(dens_all$species)

#yrs with slope data()
yrs<-as.character(c(2002,2004,2008,2010,2012,2016))

#calculate center of gravity, percentiles and effective area occupied
#loop over common spp in shelf and slope
#for (s in sel_sp) { 

for (y in yrs) { 
  
  #y<-yrs[1]
  
  #dens
  dens<-subset(obs_df1,year==y)
  
  #grid with envs
  grid.ebs_year1<-subset(xx,Year %in% y ) #& region!= 'EBSslope'
  grids<-merge(grid,grid.ebs_year1,by=c('Lon','Lat'))
  grids<-grids[order(grids$cell, decreasing = FALSE), ]   
  
    for (s in sel_sp) {
      
      #s<-sel_sp[3]
      
      cat(paste(y,s,'\n'))
      
      #merge dens and grid
      dens1<-subset(dens, species==s)
    
      
      #COG and total abundance
      centroids <- dens1 %>%
        group_by(year) %>%
        summarise(COG_lat=sum(cpue_kgkm2*lat_start,na.rm=TRUE)/sum(cpue_kgkm2,na.rm=TRUE),
                  COG_lon=sum(cpue_kgkm2*lon_start,na.rm=TRUE)/sum(cpue_kgkm2,na.rm=TRUE),
                  COG_depth=sum(cpue_kgkm2*depth_m,na.rm=TRUE)/sum(cpue_kgkm2,na.rm=TRUE),
                  mean_temp=mean(grids$Temp,na.rm=TRUE))
      
      #cbind data
      metrics<-cbind(centroids,'species'=s)
      
      #append
      metrics_df<-rbind(metrics_df,metrics) 
    }
  }


metrics_df$year_type<-ifelse(metrics_df$year %in% c(2002, 2004, 2016),'warm','cold')


library(dplyr)

# List of COG variables to test
cog_vars <- c("COG_lat", "COG_lon", "COG_depth")

# Wilcoxon test for each species and each COG variable
test_results_cog <- lapply(cog_vars, function(var) {
  metrics_df %>%
    filter(!is.na(.data[[var]])) %>%
    group_by(species) %>%
    summarise(
      p_value = tryCatch(
        wilcox.test(.data[[var]] ~ year_type)$p.value,
        error = function(e) NA
      ),
      .groups = "drop"
    ) %>%
    mutate(metric = var)
}) %>%
  bind_rows() %>%
  dplyr::select(species, metric, p_value)


# COG pred #### ----

#change to predicted from the OMs (so open each OM for these 4 species, get predicted values with depth and associated temp and plot)
load('data/data_processed/dens_sel.RData')
dimnames(dens_all)
aggregate(Lat ~ Year + species, dens_all,FUN=length)


#obs_df1<-obs_df[which(obs_df$year %in% c(cyrs,wyrs)),]
dens_all<-dens_all[which(dens_all$Year %in% 2002:2016 & dens_all$DepthGEBCO <= 600),]

#year type
dens_all$year_type<-ifelse(dens_all$Year %in% c(2002:2005,2014:2016), "warm",'cold')

#filtered to <600m
filt_obs_df1<-subset(obs_df1,depth_m<=600)

#get common
dens_all$common<-df_sp$common[match(dens_all$species, df_sp$sci)]
dens_all$common<-factor(dens_all$common,levels=c("Alaska pollock","Greenland turbot","Pacific cod","Snow crab" ))

dens_all$depth_bin <- cut(dens_all$depth_m, breaks = seq(0, 600, by = 25), include.lowest = TRUE)



#df to store results
#df to store results
metrics_df<-data.frame(matrix(NA,nrow = 0,ncol=15))
names(metrics_df)<-c("Year","COG_lat","COG_lon","COG_depth","COG_temp",
                     "total_bio",'mean_dens','mean_temp','depthq10','depthq90',"q10","q20","q80",
                     "q90","species")

#shelf slope spp
#shelfslope_spp<-unique(dens_all$species)

#yrs with slope data()
yrs<-as.character(c(2002:2016))

#calculate center of gravity, percentiles and effective area occupied
#loop over common spp in shelf and slope
#for (s in sel_sp) { 

for (y in yrs) { 
  
  #y<-yrs[2]
  
  #dens
  dens<-subset(dens_all,Year==y)
  
  #grid with envs
  grid.ebs_year1<-subset(xx,Year %in% y ) #& region!= 'EBSslope'
  grids<-merge(grid,grid.ebs_year1,by=c('Lon','Lat'))
  grids<-grids[order(grids$cell, decreasing = FALSE), ]   
  
  for (s in sel_sp) {
    
    #s<-sel_sp[3]
    
    cat(paste(y,s,'\n'))
    
    #merge dens and grid
    dens1<-subset(dens, species==s)
    
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
                mean_temp=mean(grids$Temp,na.rm=TRUE),
                depthq10=depth_10th,
                depthq90=depth_90th)
    
    #cbind data
    metrics<-cbind(centroids,quantiles[,c('q10','q20','q80','q90')],'species'=s)
    
    #append
    metrics_df<-rbind(metrics_df,metrics) 

  }
}


metrics_df$year_type<-ifelse(metrics_df$Year %in% c(2002:2005,2014:2016),'warm','cold')

#get common
metrics_df$common<-df_sp$common[match(metrics_df$species, df_sp$sci)]
metrics_df$common<-factor(metrics_df$common,levels=c("Alaska pollock","Greenland turbot","Pacific cod","Snow crab" ))

# 
# p1<-
#   ggplot(data = subset(metrics_df, species %in% sel_sp)) + 
#   # Adding 2D density contours with color based on mean_temp
#     
#   geom_point(aes(x = COG_depth, y = COG_lat, color = mean_temp, group = mean_temp),size=3
#                   ) +  # Change 'bins' to adjust the number of contours
#     geom_point(aes(x = COG_depth, y = COG_lat, group = mean_temp),color='black',shape=1,alpha=0.5,size=3) + 
#   theme_bw() + 
#   labs(x = 'bathymetrical COG (m)', y = 'latitudinal COG (°)') + 
#   theme(
#     aspect.ratio = 1,
#     text = element_text(size = 12),
#     strip.background = element_blank(),
#     strip.text = element_text(size = 12),
#     panel.grid.minor = element_line(linetype = 'dashed')
#   ) + 
#   scale_color_gradientn(colors = custom_colors(20), name = 'SBT (°C)',
#                         guide = guide_colorbar(frame.colour = "black", ticks.colour = "black")) +
#   # scale_size(name = 'abundance', breaks = c(min(summary_stats$mean_scaledbio), 
#   #                                           mean(summary_stats$mean_scaledbio), 
#   #                                           max(summary_stats$mean_scaledbio)), 
#   #            labels = c('low', 'medium', 'high'), range = c(2, 10)) + 
#   scale_x_continuous(expand = expansion(mult = 0.1)) + 
#   scale_y_continuous(expand = expansion(mult = 0.1)) + 
#   facet_wrap(~common,scales='free')
#   
#   agg_png(paste0('.figures/slope/cog_pred1.png'), width = 5.5, height = 5.5, units = "in", res = 300)
#   print(p1)
#   dev.off()
  
  #wilcoxon for pred 
  
  # Add depth range to metrics_df
  metrics_df <- metrics_df %>%
    mutate(depth_range = depthq90 - depthq10)
  
  library(dplyr)
  library(tidyr)
  library(purrr)
  
  # Prepare data wide format for each metric
  lat_wide <- metrics_df %>%
    dplyr::select(common, year_type, COG_lat) %>%
    pivot_wider(names_from = year_type, values_from = COG_lat) %>%
    drop_na(cold, warm)
  
  lon_wide <- metrics_df %>%
    dplyr::select(common, year_type, COG_lon) %>%
    pivot_wider(names_from = year_type, values_from = COG_lon) %>%
    drop_na(cold, warm)
  
  # Wilcoxon test for COG_lat per species
  wilcox_lat <- lat_wide %>%
    group_by(common) %>%
    summarise(
      W_test = list(
        wilcox.test(
          warm[[1]],
          cold[[1]],
          paired = FALSE,
          exact = FALSE
        )
      ),
      .groups = "drop"
    ) %>%
    mutate(
      p_lat = map_dbl(W_test, ~ .x$p.value),
      p_lat_label = case_when(
        is.na(p_lat) ~ "n/a",
        p_lat < 0.001 ~ "< 0.001",
        TRUE ~ paste0("lat p = ", formatC(p_lat, format = "f", digits = 3))
      )
    ) %>%
    dplyr::select(common, p_lat_label)
  
  
  wilcox_lon <- lon_wide %>%
    group_by(common) %>%
    summarise(
      W_test = list(
        wilcox.test(
          warm[[1]],
          cold[[1]],
          paired = FALSE,
          exact = FALSE
        )
      ),
      .groups = "drop"
    ) %>%
    mutate(
      p_lon = map_dbl(W_test, ~ .x$p.value),
      p_lon_label = case_when(
        is.na(p_lon) ~ "n/a",
        p_lon < 0.001 ~ "< 0.001",
        TRUE ~ paste0("Lon p = ", formatC(p_lon, format = "f", digits = 3))
      )
    ) %>%
    dplyr::select(common, p_lon_label)
  
  # Prepare wide format for COG_depth
  depth_wide <- metrics_df %>%
    dplyr::select(common, year_type, COG_depth) %>%
    pivot_wider(names_from = year_type, values_from = COG_depth) %>%
    drop_na(cold, warm)
  
  # Wilcoxon test for COG_depth
  wilcox_depth <- depth_wide %>%
    group_by(common) %>%
    summarise(
      W_test = list(wilcox.test(warm[[1]], cold[[1]], paired = FALSE, exact = FALSE)),
      .groups = "drop"
    ) %>%
    mutate(
      p_depth = map_dbl(W_test, ~ .x$p.value),
      p_depth_label = case_when(
        is.na(p_depth) ~ "depth p = n/a",
        p_depth < 0.001 ~ "depth p < 0.001",
        TRUE ~ paste0("depth p = ", formatC(p_depth, format = "f", digits = 3))
      )
    ) %>%
    dplyr::select(common, p_depth_label)
  
  # COG_lat label for top-left (facet)
  lat_labels <- wilcox_lat %>%
    mutate(label_lat = gsub("lat p = ", "lat W test\np = ", p_lat_label)) %>%
    dplyr::select(common, label_lat)
  
  # COG_depth label for bottom-right (facet)
  depth_labels <- wilcox_depth %>%
    mutate(label_depth = gsub("depth p = ", "depth W test\np = ", p_depth_label)) %>%
    dplyr::select(common, label_depth)
  
  plot_df <- metrics_df %>%
    left_join(lat_labels, by = "common") %>%
    left_join(depth_labels, by = "common")
  
  p1<-
    ggplot(data = subset(plot_df, species %in% sel_sp)) + 
    geom_point(aes(x = COG_depth, y = COG_lat, color = mean_temp), size = 3) +
    geom_point(aes(x = COG_depth, y = COG_lat), color = 'black', shape = 1, alpha = 0.5, size = 3) +
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
    facet_wrap(~common, scales = 'free') #+
    
    # # Lat W test label - top left
    # geom_text(
    #   data = plot_df %>% group_by(common) %>% slice(1),
    #   aes(x = -Inf, y = Inf, label = label_lat),
    #   hjust = -0.1, vjust = 1.2,
    #   inherit.aes = FALSE,
    #   size = 3.5,lineheight = 0.8
    # ) +
    # 
    # # Depth W test label - bottom right
    # geom_text(
    #   data = plot_df %>% group_by(common) %>% slice(1),
    #   aes(x = Inf, y = -Inf, label = label_depth),
    #   hjust = 1.1, vjust = -0.5,
    #   inherit.aes = FALSE,
    #   size = 3.5,lineheight = 0.8
    # )
  agg_png(paste0('.figures/slope/cog_pred.png'), width = 5.5, height = 5.5, units = "in", res = 300)
  print(p1)
  dev.off()
  
  
  
  # Load data
load('data/data_processed/obs_df.RData')

#data.table
sum_df<-obs_df
sum_df1<-aggregate(lat_start ~ year, sum_df,FUN=length)
sum_df1$lat_start<-sum_df1$lat_start
# Convert survey names to factors and rename
obs_df$survey_name <- as.factor(obs_df$survey_name)
levels(obs_df$survey_name) <- c('EBSshelf', 'EBSslope', 'NBS')

# Filter to selected surveys, species, and <600m depth
obs_df1 <- obs_df %>%
  filter(survey_name %in% c('EBSshelf', 'EBSslope', 'NBS') & 
           species %in% sel_sp & depth_m <= 600 & 
           year %in% bold_years)

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

  # Define function to apply Wilcoxon test safely
  wilcox_safe <- function(x, y) {
    if (length(unique(x)) > 1 && length(unique(y)) > 1) {
      tryCatch(wilcox.test(x, y, exact = FALSE)$p.value, error = function(e) NA)
    } else {
      NA
    }
  }
  
  obs_df1$denslat<-obs_df1$cpue_kgkm2*obs_df1$lat_start
  obs_df1$densdepth<-obs_df1$cpue_kgkm2*obs_df1$depth_m
  
  aggregate(lat_start ~ year + species,obs_df1,FUN=length)
  
  
  # Run Wilcoxon test for each species and each variable
  wilcox_results <- obs_df1 %>%
    group_by(common) %>%
    summarise(
      p_COG_depth = wilcox_safe(densdepth[year_type == "warm"], densdepth[year_type == "cold"]),
      p_COG_lat   = wilcox_safe(denslat[year_type == "warm"], denslat[year_type == "cold"]),
      .groups = "drop"
    )
  
  
  
# 
# #df to store results
# metrics_df<-data.frame(matrix(NA,nrow = 0,ncol=16))
# names(metrics_df)<-c("Year","COG_lat","COG_lon","COG_depth","COG_temp",
#                      "total_bio",'mean_dens','mean_temp','depthq10','depthq90',"q10","q20","q80",
#                      "q90","species",'sim')
# 
# #shelf slope spp
# #shelfslope_spp<-unique(dens_all$species)
# 
# #yrs with slope data()
# yrs<-as.character(c(2002,2004,2008,2010,2012,2016))
# 
# #subset dens
# dens_all<-sim_dens1[,sel_sp,yrs,]
# 
# #calculate center of gravity, percentiles and effective area occupied
# #loop over common spp in shelf and slope
# #for (s in sel_sp) { 
# 
# for (y in yrs) { 
#   
#   #y<-yrs[1]
#   
#   #dens
#   dens<-dens_all[,,y,]  
#   
#   #grid with envs
#   grid.ebs_year1<-subset(xx,Year %in% y)
#   grids<-merge(grid,grid.ebs_year1,by=c('Lon','Lat'))
#   grids<-grids[order(grids$cell, decreasing = FALSE), ]   
#   
#   
#   for (sim in 1:100) {
#     for (s in sel_sp) {
#       
#       #sim<-1;s<-sel_sp[3]
#       
#       cat(paste(y,s,'\n'))
#       
#       #merge dens and grid
#       dens1<-cbind(dens[,s,sim],grids)
#       names(dens1)[1]<-'dens'
#       
#       #calculate quantiles using tapply
#       quantiles <- aggregate(dens ~ Year, data = dens1, 
#                              FUN = quantile,c(0.10,0.20,0.80,0.90))
#       quantiles<- data.frame(as.matrix(quantiles))
#       names(quantiles)<-c('Year','q10','q20','q80','q90')
#       
#       #get quantiles
#       p1 <- quantiles$q10
#       p2 <- quantiles$q90
#       
#       #filter
#       dens1$quantile <- ifelse(dens1$dens < p2 & dens1$dens > p1, 1, 0)
#       
#       # Sort the dataframe by DepthGEBCO
#       all_bio4 <- dens1[order(dens1$DepthGEBCO), ]
#       
#       # Calculate cumulative biomass
#       all_bio4$bio<-all_bio4$dens*all_bio4$Area_in_survey_km2
#       all_bio4$cumulative_biomass <- cumsum(all_bio4$bio)
#       
#       # Normalize the cumulative biomass to get a cumulative distribution (0 to 1)
#       all_bio4$cumulative_biomass_scale <- all_bio4$cumulative_biomass / 
#         max(all_bio4$cumulative_biomass)
#       
#       # Find the depths corresponding to the 10th and 90th percentiles
#       ps <- c(0.10, 0.90)
#       depth_10th <- all_bio4$DepthGEBCO[which.min(
#         abs(all_bio4$cumulative_biomass_scale - ps[1]))]
#       depth_90th <- all_bio4$DepthGEBCO[which.min(
#         abs(all_bio4$cumulative_biomass_scale - ps[2]))]
#       
#       #COG and total abundance
#       centroids <- dens1 %>%
#         group_by(Year) %>%
#         summarise(COG_lat=sum(dens*Lat,na.rm=TRUE)/sum(dens,na.rm=TRUE),
#                   COG_lon=sum(dens*Lon,na.rm=TRUE)/sum(dens,na.rm=TRUE),
#                   COG_depth=sum(dens*DepthGEBCO,na.rm=TRUE)/sum(dens,na.rm=TRUE),
#                   COG_temp=sum(dens*Temp,na.rm = TRUE)/sum(dens,na.rm=TRUE),
#                   total_bio=sum(dens*Area_in_survey_km2,na.rm=TRUE),
#                   mean_dens=mean(dens,na.rm=TRUE),
#                   mean_temp=mean(Temp,na.rm=TRUE),
#                   depthq10=depth_10th,
#                   depthq90=depth_90th)
#       
#       #cbind data
#       metrics<-cbind(centroids,quantiles[,c('q10','q20','q80','q90')],'species'=s,'sim'=sim)
#       
#       #append
#       metrics_df<-rbind(metrics_df,metrics) 
#     }
#   }
# }

# SIMULATED METRICS ####
  
#load grid of NBS and EBS
load('data/extrapolation_grids/northern_bering_sea_grid.rda')
load('data/extrapolation_grids/eastern_bering_sea_grid.rda')
load('data/extrapolation_grids/bering_sea_slope_grid.rda')
colnames(bering_sea_slope_grid)[4]<-'Stratum'
bering_sea_slope_grid$Stratum<-NA
grid<-as.data.frame(rbind(data.frame(northern_bering_sea_grid,region='NBS'),
                          data.frame(eastern_bering_sea_grid,region='EBS'),
                          data.frame(bering_sea_slope_grid,region='SBS')))
grid$cell<-1:nrow(grid)
grid2<-grid
grid<-grid2[,c('Lat','Lon','region','cell')]

#load files density slope and shelf data 
load('./output/slope/species/ms_sim_dens_all.RData')  #sim_dens1
load('data/data_processed/grid_EBS_NBS_envs.RData') #xx

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
    
      #sim<-1;s<-sel_sp[3]
      
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
save(metrics_df,file='./output/slope/simulated_metrics.RData')
load(file='./output/slope/simulated_metrics.RData') #metrics_df

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

# COG sim ####

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
agg_png(paste0('.figures/slope/cog.tiff'), width = 7, height = 6.5, units = "in", res = 300)
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
agg_png(paste0('.figures/slope/cog_abu.png'), width = 7, height = 6.5, units = "in", res = 300)
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
  
  agg_png(paste0('.figures/slope/cog_sim1.png'), width = 6.5, height = 6.5, units = "in", res = 300)
  print(p1)
  dev.off()
  

  
  summary_points <- metrics_df %>%
    filter(species %in% sel_sp) %>%
    group_by(common, mean_temp) %>%
    summarise(
      mean_lat = mean(COG_lat, na.rm = TRUE),
      mean_depth = mean(COG_depth, na.rm = TRUE),
      se_lat = sd(COG_lat, na.rm = TRUE) / sqrt(n()),
      se_depth = sd(COG_depth, na.rm = TRUE) / sqrt(n()),
      ci_lat = 1.96 * se_lat,
      ci_depth = 1.96 * se_depth,
      sd_lat = sd(COG_lat, na.rm = TRUE),
      sd_depth = sd(COG_depth, na.rm = TRUE),
      .groups = 'drop'
    )
  
  
  # Step 2: Plot with error bars
  p1<-
  ggplot(data = subset(metrics_df, species %in% sel_sp)) +
    #geom_density_2d(aes(x = COG_depth, y = COG_lat, color = mean_temp, group = mean_temp),
    #                contour_var = "ndensity", bins = 100) +
    geom_point(data = summary_points, aes(x = mean_depth, y = mean_lat, color = mean_temp),shape=1) +
    geom_point(data = summary_points, aes(x = mean_depth, y = mean_lat, color = mean_temp),alpha=0.2) +
    geom_errorbar(data = summary_points, aes(x = mean_depth,
                                             ymin = mean_lat - sd_lat, ymax = mean_lat + sd_lat, color = mean_temp))+
  
  geom_errorbarh(data = summary_points, aes(y = mean_lat,
                                            xmin = mean_depth - sd_depth, xmax = mean_depth + sd_depth, color = mean_temp))+  theme_bw() +
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
  
  
  
agg_png(paste0('.figures/slope/cog_sim_v3.png'), width = 6.5, height = 6.5, units = "in", res = 300)
print(p1)
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


# # effective area occupied ####
# 
# 
# # Create a data frame for the rectangles
# rect_data <- data.frame(
#   xmin = c(-Inf, 2013.5, 2005.5),
#   xmax = c(2005.5, Inf, 2013.5),
#   ymin = rep(-Inf, 3),
#   ymax = rep(Inf, 3),
#   fill = c("warm", "warm", "cold")  # Labels for the legend
# )
# 
# #get common
# metrics_df$common<-df_sp$common[match(metrics_df$species, df_sp$sci)]
# metrics_df$common<-factor(metrics_df$common,levels=c("Alaska pollock","Greenland turbot","Pacific cod","Snow crab" ))
# 
# p <- 
#   ggplot(data = metrics_df) + 
#   # Use geom_rect to add rectangles with fill aesthetics
#   geom_rect(data = rect_data, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill), alpha = 0.5) +
#   #geom_line(aes(x = Year, y = total_bio / mean_dens / 1000, group = sim), color = 'black', alpha = 0.3) +  # Convert to thousands
#   #geom_point(aes(x = Year, y = total_bio / mean_dens / 1000, group = sim), color = 'black', alpha = 0.3) +  # Convert to thousands
#   geom_boxplot(aes(x = Year, y = total_bio / mean_dens / 1000, group = Year), color = 'black', alpha = 0.3) +  # Convert to thousands
#     
#   labs(x = '', y = 'effective area occupied (thousands km²)') +  # Adjust y-axis label
#   theme_bw() + 
#   scale_x_continuous(breaks = bold_years,minor_breaks = c(2002:2016), expand = c(0, 0.5)) + 
#   scale_y_continuous(labels = comma) +  # Add commas to y-axis numbers
#   theme(aspect.ratio = 1, 
#         strip.background = element_blank(), 
#         strip.text = element_text(size = 12), 
#         axis.text.x = element_text(angle = 45, hjust = 0.7, vjust = 0.8),
#         text = element_text(size = 12)) +   
#   facet_wrap(~common, nrow = 2, scales = 'free') + 
#   scale_fill_manual(values = c("warm" = "#cc1d1f", "cold" = "#1675ac"), 
#                     name = "SBT regime") + 
#   theme(legend.position = 'right')  # Position the legend on the right
# 
# 
# #save env plot
# agg_png(paste0('.figures/slope/effective_area_sim.png'), width = 7.5, height = 7, units = "in", res = 300)
# print(p)
# dev.off()

# interdecile depth ####

library(ggridges)

# Define a custom color scale function
custom_colors <- colorRampPalette(c("#1675ac", "white", "#cc1d1f"))

metrics_df$depth_range <- metrics_df$depthq90 - metrics_df$depthq10

ggplot(metrics_df, aes(x = depth_range, y = factor(Year), fill = mean_temp)) +
  geom_density_ridges(
    jittered_points = TRUE,rel_min_height = 0.00001,bandwidth=c(0.4),#c(0.4,2.54,1,0.2),
    position = position_points_jitter(height = 0, width = 0.5),panel_scaling = TRUE,scale=1.8,
    point_shape = '|', point_size = 1, point_alpha = 0.4#, alpha = 0.7
  ) +
  #scale_x_discrete()+
  scale_y_discrete(limits = rev)+
  scale_fill_gradientn(
    colors = custom_colors(20), name = 'SBT (°C)',
    guide = guide_colorbar(frame.colour = "black", ticks.colour = "black")
  ) +
  scale_point_color_gradient(low = '#1675ac', high = "#cc1d1f") +
  scale_color_gradientn(
    colors = custom_colors(20), name = 'SBT (°C)',
    guide = guide_colorbar(frame.colour = "black", ticks.colour = "black")
  ) +
  facet_wrap(~common, scales = 'free_x') +
  theme_minimal() +
  theme(
    strip.background = element_blank(),
    text = element_text(size = 12),
    strip.text = element_text(size = 12)
  ) +
  scale_x_continuous(expand = c(0.5,0.5))+
  labs(x = 'depth range (Q90 - Q10, m)', y = '') #+
  #coord_flip()


# Assuming 'common' is the column representing species (like Greenland turbot)
# Adjust bandwidth per facet

metrics_df1<-subset(metrics_df,common=='Greenland turbot')



ggplot() +
  geom_point(data=metrics_df, aes(x = depthq90, y = factor(Year), color = mean_temp))+
  geom_point(data=metrics_df, aes(x = depthq10, y = factor(Year), color = mean_temp))+
  geom_density_ridges(data=metrics_df, aes(x = depth_range, y = factor(Year), fill = mean_temp),
                      jittered_points = TRUE,
                      #geom = "density_ridges_gradient",
                      rel_min_height = 0.000000001,
                      #bandwidth = 1,#c(0.4,40,0.4,0.4),
                      #n =100,
                      position = position_points_jitter(height = 0, width = 0.5),
                      panel_scaling = TRUE,
                      scale = 1.8,
                      point_shape = '|',
                      point_size = 1,
                      point_alpha = 0.4)+
  scale_y_discrete(limits = rev) +
  scale_x_continuous(expand=c(0.1,01))+
  scale_fill_gradientn(
    colors = custom_colors(20), name = 'SBT (°C)',
    guide = guide_colorbar(frame.colour = "black", ticks.colour = "black")
  ) +
  scale_point_color_gradient(low = '#1675ac', high = "#cc1d1f") +
  scale_color_gradientn(
    colors = custom_colors(20), name = 'SBT (°C)',
    guide = guide_colorbar(frame.colour = "black", ticks.colour = "black")
  ) +
  facet_wrap(~common, scales = 'free_x') +
  theme_minimal() +
  theme(
    strip.background = element_blank(),
    text = element_text(size = 12),
    strip.text = element_text(size = 12)
  ) +
  labs(x = 'Depth range (Q90 - Q10, m)', y = '')

# Plot with variable bandwidth for each facet
ggplot(metrics_df, aes(x = depth_range, y = factor(Year), fill = mean_temp)) +
  geom_density_ridges(jittered_points = TRUE,
                      #geom = "density_ridges_gradient",
                      rel_min_height = 0.000000001,
                      #bandwidth = 1,#c(0.4,40,0.4,0.4),
                      #n =100,
                      position = position_points_jitter(height = 0, width = 0.5),
                      panel_scaling = TRUE,
                      scale = 1.8,
                      point_shape = '|',
                      point_size = 1,
                      point_alpha = 0.4)+
  scale_y_discrete(limits = rev) +
  scale_x_continuous(expand=c(0.1,01))+
  scale_fill_gradientn(
    colors = custom_colors(20), name = 'SBT (°C)',
    guide = guide_colorbar(frame.colour = "black", ticks.colour = "black")
  ) +
  scale_point_color_gradient(low = '#1675ac', high = "#cc1d1f") +
  scale_color_gradientn(
    colors = custom_colors(20), name = 'SBT (°C)',
    guide = guide_colorbar(frame.colour = "black", ticks.colour = "black")
  ) +
  facet_wrap(~common, scales = 'free_x') +
  theme_minimal() +
  theme(
    strip.background = element_blank(),
    text = element_text(size = 12),
    strip.text = element_text(size = 12)
  ) +
  labs(x = 'Depth range (Q90 - Q10, m)', y = '')



# Define custom bandwidth values for each 'common' group (example values)
bandwidth_values <- data.frame(
  common = unique(metrics_df$common),
  bw = c(0.4, 0.4, 2,0.4)  # Replace with your desired bandwidth values for each 'common' group
)

# Merge the bandwidth values with the metrics_df based on 'common'
metrics_df <- metrics_df %>%
  left_join(bandwidth_values, by = "common")  # Adding bandwidth values to metrics_df

# Calculate density manually per panel with the respective bandwidth for each 'common' group
metrics_df <- metrics_df %>%
  group_by(common, Year) %>%
  mutate(density = list(density(depth_range, bw = first(bw))))  # Use 'first(bw)' to extract the scalar value

# Extract the x and y components of the density and store them in separate columns
metrics_df <- metrics_df %>%
  mutate(x_vals = map(density, ~ .x$x),  # Extract the x values
        y_vals = map(density, ~ .x$y))  # Extract the y values

# Unnest the x and y values
density_df <- metrics_df %>%
  unnest(cols = c(x_vals, y_vals))  # Unnest the x and y values
# Step 1: Find the global maximum y-value across all common groups
global_max_y <- max(density_df$y_vals, na.rm = TRUE)

# Step 2: Rescale y_vals for each common group
density_df <- density_df %>%
  group_by(common) %>%
  mutate(rescaled_y_vals = y_vals / max(y_vals, na.rm = TRUE) * global_max_y)

# Step 3: Plot with the rescaled y-values
  library(dplyr)

# Sample 10000 points
sampled_df <- density_df %>%
  sample_n(5000)

depth_labels <- sampled_df %>%
  group_by(Year, common, mean_temp) %>%
  summarize(
    mean_q10 = mean(depthq10, na.rm = TRUE),
    mean_q90 = mean(depthq90, na.rm = TRUE),
    depth_range_mid = max(depth_range, na.rm = TRUE)
  ) %>%
  mutate(
    Year = factor(Year),
    y_pos = Year,
    label = if_else(
      round(mean_q10, 0) >= 100,
      paste0("Q90: ", round(mean_q90, 0), "\nQ10: ", round(mean_q10, 0)),
      paste0("Q90: ", round(mean_q90, 0), "\n Q10: ", round(mean_q10, 0))  # leading space on Q10
    )
  )

p2<-
ggplot(sampled_df, aes(x = x_vals, y = factor(Year), fill = mean_temp)) + 
  # Plot the observation points on top of the ridgelines
  geom_point(
    aes(x = depth_range, y = factor(Year)), 
    position = position_jitter(height = 0, width = 0.4),
    color = 'black',
    shape = '|', 
    size = 2, 
    alpha = 0.2
  ) + 
  geom_ridgeline(
    aes(height = rescaled_y_vals),  # Use the rescaled density values
    color = 'black',
    scale = 1.8,
    linewidth = 0.5
  ) + 
  geom_text(
    data = depth_labels,
    aes(x = depth_range_mid, y = y_pos, label = label),
    vjust = -0.1,hjust=-0.2,
    size = 2.5,
    lineheight = 0.8)+
scale_y_discrete(limits = rev, expand = expansion(mult = c(0.05, .35))) + 
  scale_x_continuous(expand=c(0.2,0.1)) + 
  scale_fill_gradientn(
    colors = custom_colors(20), name = 'SBT (°C)', 
    guide = guide_colorbar(frame.colour = "black", ticks.colour = "black")
  ) + 
  scale_point_color_gradient(low = '#1675ac', high = "#cc1d1f") + 
  scale_color_gradientn(
    colors = custom_colors(20), name = 'SBT (°C)', 
    guide = guide_colorbar(frame.colour = "black", ticks.colour = "black")
  ) + 
  facet_wrap(~common, scales = 'free_x') + 
  theme_bw() + 
  theme(
    strip.background = element_blank(), 
    text = element_text(size = 12), 
    strip.text = element_text(size = 12),
    panel.grid.major.x = element_line(linetype = 'dashed'),
    panel.grid.minor.x = element_line(linetype = 'dashed')
  ) + 
  labs(x = 'depth range (Q90 - Q10, m)', y = '')

  #save env plot
agg_png(paste0('.figures/slope/interdecile_depth_sim4.png'), width = 8, height = 5, units = "in", res = 300)
print(p2)
dev.off()

ggplot(density_df, aes(x = x_vals, y = factor(Year), fill = mean_temp)) + 
  # Plot the ridgelines with rescaled y-values
  # Plot the observation points on top of the ridgelines
  
  geom_ridgeline(
    aes(height = rescaled_y_vals),  # Use the rescaled density values
    color = 'black',
    scale = 1.8
  ) + 
  geom_point(
    aes(x = depth_range, y = factor(Year)), #, color = mean_temp # Points for the observations
    position = position_jitter(height = 0, width = 1),
    color='black',
    shape = '—', 
    size = 1, 
    alpha = 1
  ) + 
  scale_y_discrete() + 
  scale_x_continuous(expand = c(0.3, 0.05),
                     trans = 'reverse')+  # Reverse the x-axis) + 
  scale_fill_gradientn(colors = custom_colors(20), name = 'SBT (°C)', 
                       guide = guide_colorbar(frame.colour = "black", ticks.colour = "black")) + 
  scale_point_color_gradient(low = '#1675ac', high = "#cc1d1f") + 
  scale_color_gradientn(colors = custom_colors(20), name = 'SBT (°C)', 
                        guide = guide_colorbar(frame.colour = "black", ticks.colour = "black")) + 
  facet_wrap(~common, scales = 'free_y') + 
  theme_minimal() + 
  theme(strip.background = element_blank(), 
        text = element_text(size = 12), 
        strip.text = element_text(size = 12),
        panel.grid.major.y = element_line(linetype = 'dashed'),
        panel.grid.minor.y = element_line(linetype = 'dashed')) + 
  labs(x = 'depth range (Q90 - Q10, m)', y = '')+
  coord_flip()



#p1<-
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
agg_png(paste0('.figures/slope/interdecile_depth_sim1.png'), width = 7, height = 5, units = "in", res = 300)
print(p)
dev.off()




p2<-
  ggplot(data = metrics_df) +  
  # Reorder the 'common' variable by 'mean_temp' for sorting the violins
  geom_violin(aes(x = depthq90 - depthq10, y = mean_temp, fill = mean_temp, group = mean_temp),  
              color = "black", size = 0.4, adjust = 2, scale = "width", position = position_dodge(width = 0.5), 
              trim = FALSE) +  # Adjusted scale for height
  
  scale_x_continuous() +  # Adjust x-axis scale
  
  facet_wrap(~common, scales = 'free') +  # Facet by 'common' variable
  
  scale_fill_gradientn(colors = custom_colors(20), name = 'SBT (°C)',  
                       guide = guide_colorbar(frame.colour = "black", ticks.colour = "black")) +  
  
  theme_bw() +  
  theme(
    #axis.text.y = element_blank(),  # Hide y-axis text
    #axis.ticks.y = element_blank(),  # Remove y-axis ticks
    #axis.title.y = element_blank(),  # Remove y-axis title
    strip.background = element_blank(),  # Clean facet strip background
    text = element_text(size = 12),  # General font size
    strip.text = element_text(size = 12)  # Size for facet labels
  ) +  
  
  labs(x = 'Depth at 90th percentile - Depth at 10th percentile (m)',y='SBT (°C)' )

#save env plot
agg_png(paste0('.figures/slope/interdecile_depth_sim3.png'), width = 7, height = 5, units = "in", res = 300)
print(p2)
dev.off()





# interdecile depth pred #### ----

library(ggridges)

# Define a custom color scale function
custom_colors <- colorRampPalette(c("#1675ac", "white", "#cc1d1f"))

metrics_df$depth_range <- metrics_df$depthq90 - metrics_df$depthq10

# Ensure Year is treated as factor for discrete y-axis
metrics_df$Year <- factor(metrics_df$Year)
# Define custom bandwidth values for each 'common' group (example values)
bandwidth_values <- data.frame(
  common = unique(metrics_df$common),
  bw = c(0.4, 0.4, 2,0.4)  # Replace with your desired bandwidth values for each 'common' group
)

# Merge the bandwidth values with the metrics_df based on 'common'
metrics_df <- metrics_df %>%
  left_join(bandwidth_values, by = "common")  # Adding bandwidth values to metrics_df

# Calculate density manually per panel with the respective bandwidth for each 'common' group
metrics_df <- metrics_df %>%
  group_by(common, Year) %>%
  mutate(density = list(density(depth_range, bw = first(bw))))  # Use 'first(bw)' to extract the scalar value

# Extract the x and y components of the density and store them in separate columns
metrics_df <- metrics_df %>%
  mutate(x_vals = map(density, ~ .x$x),  # Extract the x values
         y_vals = map(density, ~ .x$y))  # Extract the y values

# Unnest the x and y values
density_df <- metrics_df %>%
  unnest(cols = c(x_vals, y_vals))  # Unnest the x and y values
# Step 1: Find the global maximum y-value across all common groups
global_max_y <- max(density_df$y_vals, na.rm = TRUE)

# Step 2: Rescale y_vals for each common group
density_df <- density_df %>%
  group_by(common) %>%
  mutate(rescaled_y_vals = y_vals / max(y_vals, na.rm = TRUE) * global_max_y)

# Step 3: Plot with the rescaled y-values
library(dplyr)

# Sample 10000 points
sampled_df <- density_df #%>%
  #sample_n(5000)

depth_labels <- sampled_df %>%
  group_by(Year, common, mean_temp) %>%
  summarize(
    mean_q10 = mean(depthq10, na.rm = TRUE),
    mean_q90 = mean(depthq90, na.rm = TRUE),
    depth_range_mid = max(depth_range, na.rm = TRUE)
  ) %>%
  mutate(
    Year = factor(Year),
    y_pos = Year,
    label = if_else(
      round(mean_q10, 0) >= 100,
      paste0("Q90: ", round(mean_q90, 0), "\nQ10: ", round(mean_q10, 0)),
      paste0("Q90: ", round(mean_q90, 0), "\n Q10: ", round(mean_q10, 0))  # leading space on Q10
    )
  )

ggplot(metrics_df, aes(x = depth_range, y = Year, color = mean_temp)) +
  geom_point(size = 3, alpha = 0.8) +
  scale_color_gradientn(
    colors = custom_colors(20),  # your existing palette
    name = "SBT (°C)",
    guide = guide_colorbar(frame.colour = "black", ticks.colour = "black")
  ) +
  facet_wrap(~common, scales = "free_y") +
  labs(
    x = "Depth range (Q90 - Q10, m)",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12),
    text = element_text(size = 12),
    legend.position = "bottom"
  )


ggplot(metrics_df, aes(x = depth_range, y = Year, color = mean_temp)) +
  geom_point(size = 3, alpha = 0.8) +
  scale_color_gradientn(
    colors = custom_colors(20),  # your existing palette
    name = "SBT (°C)",
    guide = guide_colorbar(frame.colour = "black", ticks.colour = "black")
  ) +
  facet_wrap(~common, scales = "free_y") +
  labs(
    x = "Depth range (Q90 - Q10, m)",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12),
    text = element_text(size = 12),
    legend.position = "bottom"
  )+
  facet_wrap(~common, scales = "free_x") 



ggplot(metrics_df, aes(y = Year)) +
  geom_segment(
    aes(x = depthq10, xend = depthq90, yend = Year, color = mean_temp),
    size = 2
  ) +
  geom_point(
    aes(x = depthq90 - depthq10, y = Year, size=depthq90 - depthq10),
              # circle with border
    #size = 1.5,           # point size
    stroke = 1,         # thickness of the border
    color = "black",      # border color
    fill = "black",       # fill color (can be changed if needed)
    alpha = 0.5
  ) +
  scale_y_discrete(limits = rev) + 
  scale_color_gradientn(
    colors = custom_colors(20),
    name = "SBT (°C)",
    guide = guide_colorbar(frame.colour = "black", ticks.colour = "black")
  ) +
  facet_wrap(~common, scales = "free_x") +
  labs(
    x = "depth (m)",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12),
    text = element_text(size = 12),
    legend.position = "bottom"
  )




metrics_df <- metrics_df %>%
  group_by(common) %>%
  mutate(
    local_range = depthq90 - depthq10,
    scaled_range = scales::rescale(local_range, to = c(1, 6))  # Adjust size range here
  ) %>%
  ungroup()


ggplot(metrics_df, aes(y = Year)) +
  geom_segment(
    aes(x = depthq10, xend = depthq90, yend = Year, color = mean_temp),
    size = 2
  ) +
  geom_point(
    aes(x = local_range, y = Year, size = scaled_range),
    stroke = 1,
    color = "black",
    fill = "black",
    alpha = 0.5
  ) +
  scale_y_discrete(limits = rev) + 
  scale_color_gradientn(
    colors = custom_colors(20),
    name = "SBT (°C)",
    guide = guide_colorbar(frame.colour = "black", ticks.colour = "black")
  ) +
  scale_size_identity() +  # use scaled size directly, no legend
  facet_wrap(~common, scales = "free_x") +
  labs(
    x = "depth (m)",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12),
    text = element_text(size = 12),
    legend.position = "bottom"
  )

metrics_df <- metrics_df %>%
  mutate(depth_range = depthq90 - depthq10)

# Prepare wide format for COG_depth
depth_wide <- metrics_df %>%
  dplyr::select(common, year_type, depth_range) %>%
  pivot_wider(names_from = year_type, values_from = depth_range) %>%
  drop_na(cold, warm)

# Wilcoxon test for depth_range
wilcox_depth <- depth_wide %>%
  group_by(common) %>%
  summarise(
    W_test = list(wilcox.test(warm[[1]], cold[[1]], paired = FALSE, exact = FALSE)),
    .groups = "drop"
  ) %>%
  mutate(
    p_depth = map_dbl(W_test, ~ .x$p.value),
    p_depth_label = case_when(
      is.na(p_depth)     ~ "depth p = n/a",
      p_depth < 0.001    ~ "depth p < 0.001",
      TRUE               ~ paste0("depth p = ", formatC(p_depth, format = "f", digits = 3))
    )
  ) %>%
  dplyr::select(common, p_depth_label)

# COG_depth label for bottom-right (facet)
depth_labels <- wilcox_depth %>%
  mutate(label_depth = gsub("depth p = ", "W test\np = ", p_depth_label)) %>%
  dplyr::select(common, label_depth)

# Join labels into main plotting df
plot_df <- metrics_df %>%
  left_join(depth_labels, by = "common")

plot_df$legend_label <- "depth range (Q10–Q90)"
plot_df$point_label <- "depth niche width (Q90 - Q10)"
# Keep labels as strings in your data frame
plot_df$legend_label <- "depth range (Q10–Q90)"
plot_df$point_label <- "depth niche width (Q90 - Q10)"

# Define expression labels for the legend
legend_label_expr <- expression("depth range"~(Q[10]*","*Q[90]))
point_label_expr <- expression("depth niche width"~(Q[90] - Q[10]))

p3<-
ggplot(plot_df, aes(y = Year)) +
  geom_segment(
    aes(x = depthq10, xend = depthq90, yend = Year, color = mean_temp, linetype = legend_label),
    size = 1
  ) +
  geom_point(
    aes(x = local_range, y = Year, size = scaled_range, fill = mean_temp, shape = point_label),
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
    labels = c(legend_label_expr)
  ) +
  scale_shape_manual(
    name = NULL,
    values = c("depth niche width (Q90 - Q10)" = 21),
    labels = c(point_label_expr)
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

 # ) +
 #  # Add top-left label for Lat p-value
 #  geom_text(
 #    data = plot_df %>% distinct(common, label_depth),
 #    aes(x = -Inf, y = Inf, label = label_depth),
 #    hjust = -0.1, vjust = 1.2,
 #    inherit.aes = FALSE,
 #    size = 3.5,lineheight = 0.8
 #  )

#save env plot
agg_png(paste0('.figures/slope/interdecile_depth_pred2.png'), width = 7.5, height = 6.5, units = "in", res = 300)
print(p3)
dev.off()



metrics_df$legend_label <- "depth range (Q10–Q90)"
metrics_df$point_label <- "depth range width (Q90 - Q10)"

p2 <- ggplot(metrics_df, aes(y = Year)) +
  # Line segments from Q10 to Q90 depth
  geom_segment(
    aes(x = depthq10, xend = depthq90, yend = Year, color = mean_temp, linetype = legend_label),
    size = 2
  ) +
  # Point showing Q90 - Q10 (width)
  geom_point(
    aes(x = depthq90 - depthq10, shape = point_label),
    size = 2.5,
    color = "black",
    alpha = 0.6,
    stroke = 0.8,
    fill = "black"
  ) +
  scale_y_discrete(limits = rev) +
  
  # Color gradient for mean SBT (°C)
  scale_color_gradientn(
    colors = custom_colors(20),
    name = "mean SBT (°C)"
  ) +
  
  # Manual linetype scale for legend
  scale_linetype_manual(
    name = NULL,
    values = c("depth range (Q10–Q90)" = "solid")
  ) +
  
  # Manual shape scale for legend
  scale_shape_manual(
    name = NULL,
    values = c("depth range width (Q90 - Q10)" = 21)
  ) +
  
  # Guides with custom layout
  guides(
    color = guide_colorbar(
      order = 1,
      title.position = "top",
      direction = "horizontal",
      title.hjust = 0.5
    ),
    linetype = guide_legend(
      order = 2,
      override.aes = list(
        size = 6,
        color = "black"
      )
    ),
    shape = guide_legend(order = 3)
  ) +
  
  facet_wrap(~common, scales = "free_x") +
  labs(
    x = "depth (m)",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12),
    text = element_text(size = 12),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.box.just = "center",
    legend.spacing.x = unit(0.4, "cm"),
    legend.title = element_text(hjust = 0.5)
  )


#save env plot
agg_png(paste0('.figures/slope/interdecile_depth_pred.png'), width = 8, height = 7, units = "in", res = 300)
print(p2)
dev.off()


#p2<-
  ggplot(metrics_df, aes(x = depth_range, y = factor(Year), fill = mean_temp)) + 
  # Plot the observation points on top of the ridgelines
  geom_point(
    aes(x = depth_range, y = factor(Year)), 
    position = position_jitter(height = 0, width = 0.4),
    color = 'black',
    shape = '|', 
    size = 1, 
    alpha = 0.2
  ) + 
  geom_ridgeline(
    aes(height = rescaled_y_vals),  # Use the rescaled density values
    color = 'black',
    scale = 1.8,
    linewidth = 0.5
  ) + 
  # geom_text(
  #   data = depth_labels,
  #   aes(x = depth_range_mid, y = y_pos, label = label),
  #   vjust = -0.1,hjust=-0.2,
  #   size = 2.5,
  #   lineheight = 0.8)+
  scale_y_discrete(limits = rev, expand = expansion(mult = c(0.05, .35))) + 
  scale_x_continuous(expand=c(0.2,0.1)) + 
  scale_fill_gradientn(
    colors = custom_colors(20), name = 'SBT (°C)', 
    guide = guide_colorbar(frame.colour = "black", ticks.colour = "black")
  ) + 
  scale_point_color_gradient(low = '#1675ac', high = "#cc1d1f") + 
  scale_color_gradientn(
    colors = custom_colors(20), name = 'SBT (°C)', 
    guide = guide_colorbar(frame.colour = "black", ticks.colour = "black")
  ) + 
  facet_wrap(~common, scales = 'free_x') + 
  theme_bw() + 
  theme(
    strip.background = element_blank(), 
    text = element_text(size = 12), 
    strip.text = element_text(size = 12),
    panel.grid.major.x = element_line(linetype = 'dashed'),
    panel.grid.minor.x = element_line(linetype = 'dashed')
  ) + 
  labs(x = 'depth range (Q90 - Q10, m)', y = '')


# Assuming 'common' is the column representing species (like Greenland turbot)
# Adjust bandwidth per facet

metrics_df1<-subset(metrics_df,common=='Greenland turbot')



ggplot() +
  geom_point(data=metrics_df, aes(x = depthq90, y = factor(Year), color = mean_temp))+
  geom_point(data=metrics_df, aes(x = depthq10, y = factor(Year), color = mean_temp))+
  geom_density_ridges(data=metrics_df, aes(x = depth_range, y = factor(Year), fill = mean_temp),
                      jittered_points = TRUE,
                      #geom = "density_ridges_gradient",
                      rel_min_height = 0.000000001,
                      #bandwidth = 1,#c(0.4,40,0.4,0.4),
                      #n =100,
                      position = position_points_jitter(height = 0, width = 0.5),
                      panel_scaling = TRUE,
                      scale = 1.8,
                      point_shape = '|',
                      point_size = 1,
                      point_alpha = 0.4)+
  scale_y_discrete(limits = rev) +
  scale_x_continuous(expand=c(0.1,01))+
  scale_fill_gradientn(
    colors = custom_colors(20), name = 'SBT (°C)',
    guide = guide_colorbar(frame.colour = "black", ticks.colour = "black")
  ) +
  scale_point_color_gradient(low = '#1675ac', high = "#cc1d1f") +
  scale_color_gradientn(
    colors = custom_colors(20), name = 'SBT (°C)',
    guide = guide_colorbar(frame.colour = "black", ticks.colour = "black")
  ) +
  facet_wrap(~common, scales = 'free_x') +
  theme_minimal() +
  theme(
    strip.background = element_blank(),
    text = element_text(size = 12),
    strip.text = element_text(size = 12)
  ) +
  labs(x = 'Depth range (Q90 - Q10, m)', y = '')

# Plot with variable bandwidth for each facet
ggplot(metrics_df, aes(x = depth_range, y = factor(Year), fill = mean_temp)) +
  geom_density_ridges(jittered_points = TRUE,
                      #geom = "density_ridges_gradient",
                      rel_min_height = 0.000000001,
                      #bandwidth = 1,#c(0.4,40,0.4,0.4),
                      #n =100,
                      position = position_points_jitter(height = 0, width = 0.5),
                      panel_scaling = TRUE,
                      scale = 1.8,
                      point_shape = '|',
                      point_size = 1,
                      point_alpha = 0.4)+
  scale_y_discrete(limits = rev) +
  scale_x_continuous(expand=c(0.1,01))+
  scale_fill_gradientn(
    colors = custom_colors(20), name = 'SBT (°C)',
    guide = guide_colorbar(frame.colour = "black", ticks.colour = "black")
  ) +
  scale_point_color_gradient(low = '#1675ac', high = "#cc1d1f") +
  scale_color_gradientn(
    colors = custom_colors(20), name = 'SBT (°C)',
    guide = guide_colorbar(frame.colour = "black", ticks.colour = "black")
  ) +
  facet_wrap(~common, scales = 'free_x') +
  theme_minimal() +
  theme(
    strip.background = element_blank(),
    text = element_text(size = 12),
    strip.text = element_text(size = 12)
  ) +
  labs(x = 'Depth range (Q90 - Q10, m)', y = '')


library(tidyr)
library(purrr)

# Define custom bandwidth values for each 'common' group (example values)
bandwidth_values <- data.frame(
  common = unique(metrics_df$common),
  bw = c(0.4, 0.4, 2,0.4)  # Replace with your desired bandwidth values for each 'common' group
)

# Merge the bandwidth values with the metrics_df based on 'common'
metrics_df <- metrics_df %>%
  left_join(bandwidth_values, by = "common")  # Adding bandwidth values to metrics_df

# Calculate density manually per panel with the respective bandwidth for each 'common' group
metrics_df <- metrics_df %>%
  group_by(common, Year) %>%
  mutate(density = list(density(depth_range, bw = first(bw))))  # Use 'first(bw)' to extract the scalar value

# Extract the x and y components of the density and store them in separate columns
metrics_df <- metrics_df %>%
  mutate(x_vals = map(density, ~ .x$x),  # Extract the x values
         y_vals = map(density, ~ .x$y))  # Extract the y values

# Unnest the x and y values
density_df <- metrics_df %>%
  unnest(cols = c(x_vals, y_vals))  # Unnest the x and y values
# Step 1: Find the global maximum y-value across all common groups
global_max_y <- max(density_df$y_vals, na.rm = TRUE)

# Step 2: Rescale y_vals for each common group
density_df <- density_df %>%
  group_by(common) %>%
  mutate(rescaled_y_vals = y_vals / max(y_vals, na.rm = TRUE) * global_max_y)

# Step 3: Plot with the rescaled y-values

# Sample 10000 points
sampled_df <- density_df %>%
  sample_n(5000)

depth_labels <- sampled_df %>%
  group_by(Year, common, mean_temp) %>%
  summarize(
    mean_q10 = mean(depthq10, na.rm = TRUE),
    mean_q90 = mean(depthq90, na.rm = TRUE),
    depth_range_mid = max(depth_range, na.rm = TRUE)
  ) %>%
  mutate(
    Year = factor(Year),
    y_pos = Year,
    label = if_else(
      round(mean_q10, 0) >= 100,
      paste0("Q90: ", round(mean_q90, 0), "\nQ10: ", round(mean_q10, 0)),
      paste0("Q90: ", round(mean_q90, 0), "\n Q10: ", round(mean_q10, 0))  # leading space on Q10
    )
  )

p2<-
  ggplot(sampled_df, aes(x = x_vals, y = factor(Year), fill = mean_temp)) + 
  # Plot the observation points on top of the ridgelines
  geom_point(
    aes(x = depth_range, y = factor(Year)), 
    position = position_jitter(height = 0, width = 0.4),
    color = 'black',
    shape = '|', 
    size = 2, 
    alpha = 0.2
  ) + 
  geom_ridgeline(
    aes(height = rescaled_y_vals),  # Use the rescaled density values
    color = 'black',
    scale = 1.8,
    linewidth = 0.5
  ) + 
  geom_text(
    data = depth_labels,
    aes(x = depth_range_mid, y = y_pos, label = label),
    vjust = -0.1,hjust=-0.2,
    size = 2.5,
    lineheight = 0.8)+
  scale_y_discrete(limits = rev, expand = expansion(mult = c(0.05, .35))) + 
  scale_x_continuous(expand=c(0.2,0.1)) + 
  scale_fill_gradientn(
    colors = custom_colors(20), name = 'SBT (°C)', 
    guide = guide_colorbar(frame.colour = "black", ticks.colour = "black")
  ) + 
  scale_point_color_gradient(low = '#1675ac', high = "#cc1d1f") + 
  scale_color_gradientn(
    colors = custom_colors(20), name = 'SBT (°C)', 
    guide = guide_colorbar(frame.colour = "black", ticks.colour = "black")
  ) + 
  facet_wrap(~common, scales = 'free_x') + 
  theme_bw() + 
  theme(
    strip.background = element_blank(), 
    text = element_text(size = 12), 
    strip.text = element_text(size = 12),
    panel.grid.major.x = element_line(linetype = 'dashed'),
    panel.grid.minor.x = element_line(linetype = 'dashed')
  ) + 
  labs(x = 'depth range (Q90 - Q10, m)', y = '')

#save env plot
agg_png(paste0('.figures/slope/interdecile_depth_sim4.png'), width = 8, height = 5, units = "in", res = 300)
print(p2)
dev.off()

ggplot(density_df, aes(x = x_vals, y = factor(Year), fill = mean_temp)) + 
  # Plot the ridgelines with rescaled y-values
  # Plot the observation points on top of the ridgelines
  
  geom_ridgeline(
    aes(height = rescaled_y_vals),  # Use the rescaled density values
    color = 'black',
    scale = 1.8
  ) + 
  geom_point(
    aes(x = depth_range, y = factor(Year)), #, color = mean_temp # Points for the observations
    position = position_jitter(height = 0, width = 1),
    color='black',
    shape = '—', 
    size = 1, 
    alpha = 1
  ) + 
  scale_y_discrete() + 
  scale_x_continuous(expand = c(0.3, 0.05),
                     trans = 'reverse')+  # Reverse the x-axis) + 
  scale_fill_gradientn(colors = custom_colors(20), name = 'SBT (°C)', 
                       guide = guide_colorbar(frame.colour = "black", ticks.colour = "black")) + 
  scale_point_color_gradient(low = '#1675ac', high = "#cc1d1f") + 
  scale_color_gradientn(colors = custom_colors(20), name = 'SBT (°C)', 
                        guide = guide_colorbar(frame.colour = "black", ticks.colour = "black")) + 
  facet_wrap(~common, scales = 'free_y') + 
  theme_minimal() + 
  theme(strip.background = element_blank(), 
        text = element_text(size = 12), 
        strip.text = element_text(size = 12),
        panel.grid.major.y = element_line(linetype = 'dashed'),
        panel.grid.minor.y = element_line(linetype = 'dashed')) + 
  labs(x = 'depth range (Q90 - Q10, m)', y = '')+
  coord_flip()



#p1<-
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
agg_png(paste0('.figures/slope/interdecile_depth_sim1.png'), width = 7, height = 5, units = "in", res = 300)
print(p)
dev.off()




p2<-
  ggplot(data = metrics_df) +  
  # Reorder the 'common' variable by 'mean_temp' for sorting the violins
  geom_violin(aes(x = depthq90 - depthq10, y = mean_temp, fill = mean_temp, group = mean_temp),  
              color = "black", size = 0.4, adjust = 2, scale = "width", position = position_dodge(width = 0.5), 
              trim = FALSE) +  # Adjusted scale for height
  
  scale_x_continuous() +  # Adjust x-axis scale
  
  facet_wrap(~common, scales = 'free') +  # Facet by 'common' variable
  
  scale_fill_gradientn(colors = custom_colors(20), name = 'SBT (°C)',  
                       guide = guide_colorbar(frame.colour = "black", ticks.colour = "black")) +  
  
  theme_bw() +  
  theme(
    #axis.text.y = element_blank(),  # Hide y-axis text
    #axis.ticks.y = element_blank(),  # Remove y-axis ticks
    #axis.title.y = element_blank(),  # Remove y-axis title
    strip.background = element_blank(),  # Clean facet strip background
    text = element_text(size = 12),  # General font size
    strip.text = element_text(size = 12)  # Size for facet labels
  ) +  
  
  labs(x = 'Depth at 90th percentile - Depth at 10th percentile (m)',y='SBT (°C)' )

#save env plot
agg_png(paste0('.figures/slope/interdecile_depth_sim3.png'), width = 7, height = 5, units = "in", res = 300)
print(p2)
dev.off()

#background stratification figure ####

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
  # geom_text(
  #   data = rects,
  #   aes(x = label_x, y = label_y, label = label),
  #   size = 6,
  #   fontface = "bold",
  #   color = "white"
  # ) +
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


agg_png(paste0('.figures/slope/flexible_stratification.png'), width = 7, height = 7, units = "in", res = 300)
cowplot::plot_grid(p2,p1,ncol = 1)
dev.off()
