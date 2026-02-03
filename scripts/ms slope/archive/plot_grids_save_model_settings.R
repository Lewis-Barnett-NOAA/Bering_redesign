# plot extrapolation grids -----
setwd('/Users/daniel/Work/UW-NOAA/Adapting Monitoring to a Changing Seascape/')

eastern_bering_sea_grid <- FishStatsUtils::eastern_bering_sea_grid
grid <- data.frame(eastern_bering_sea_grid)
#grid1<-data.frame(grid[78:95,])
grid1<-subset(data.frame(grid),Area_in_survey_km2>13.6 & Area_in_survey_km2<13.8)

# Sort unique longitudes and latitudes
lon_res <- mean(diff(sort(unique(grid1[,'Lon']))))
lat_res <- mean(diff(sort(unique(grid1[,'Lat']))))

mean_lat <- mean(grid1[,'Lat'])
lon_km <- lon_res * 111 * cos(mean_lat * pi / 180)
lat_km <- lat_res * 111

# Suppose you have the extrapolation_list from VAST:

gridebs <- data.frame(eastern_bering_sea_grid)
#gridebs<-subset(data.frame(gridebs),Area_in_survey_km2==13.71962)
northern_bering_sea_grid <- FishStatsUtils::northern_bering_sea_grid
gridnbs <- data.frame(northern_bering_sea_grid)

bering_sea_slope_grid <- FishStatsUtils::bering_sea_slope_grid
gridslope <- data.frame(bering_sea_slope_grid)

library(ggplot2)

p<-
ggplot() +
  geom_point(data = gridnbs, aes(x = Lon, y = Lat), 
             color = "grey30", alpha = 0.5, size = 0.001) +
  
  geom_point(data = gridebs, aes(x = Lon, y = Lat), 
             color = "grey30", alpha = 0.5, size = 0.001) +
  
  geom_point(data = gridslope, aes(x = Lon, y = Lat), 
             color = "grey30", alpha = 0.5, size = 0.001) +
  
  labs(x = "Longitude", y = "Latitude") +
  #coord_fixed(1.3) +
  theme_minimal()+
  theme(text = element_text(size = 15))

#save plot
ragg::agg_png(paste0('figures/slope/grids1.png'), width = 12, height = 12, units = "in", res = 300)
#ragg::agg_png(paste0('./figures/ms_hist_indices_cv_box_EBSNBS_suppl.png'), width = 13, height = 8, units = "in", res = 300)
p
dev.off()


# gather VAST model settings for OMs

# slope models ----
# Set working directory
setwd('/Volumes/MAC drive/Redesign survey LB/OM SBS/')

# List all subdirectories (non-recursive)
ld <- list.dirs(full.names = TRUE, recursive = FALSE)

# Initialize a data frame to store model settings
model_info <- data.frame(
  model = character(),
  rho_config = I(list()),
  obs_model = I(list()),
  n_x = integer(),
  omega = I(list()),
  stringsAsFactors = FALSE
)

# Loop through each subdirectory
for (i in ld) {
  
  print(paste("Processing:", i))  # <- This line prints the current folder
  
  fit_file <- file.path(i, "fit_st.RData")
  
  if (file.exists(fit_file)) {
    
    load(fit_file)  # loads the 'fit' object
    
    omega_row <- fit$settings$FieldConfig["Omega", ]
    
    model_info <- rbind(
      model_info,
      data.frame(
        model = basename(i),
        rho_config = I(list(fit$settings$RhoConfig)),
        obs_model = I(list(fit$settings$ObsModel)),
        n_x = fit$settings$n_x,
        omega = I(list(omega_row))
      )
    )
    
  } else {
    warning(paste("Missing fit_st.RData in:", i))
  }
}

# shelf models ----
# Set working directory
setwd('/Volumes/MAC drive/Redesign survey LB/OM EBS-NBS//')

# List all subdirectories (non-recursive)
ld <- list.dirs(full.names = TRUE, recursive = FALSE)

# Initialize a data frame to store model settings
model_info <- data.frame(
  model = character(),
  rho_config = I(list()),
  obs_model = I(list()),
  n_x = integer(),
  omega = I(list()),
  stringsAsFactors = FALSE
)

# Loop through each subdirectory
for (i in ld) {
  
  print(paste("Processing:", i))  # <- This line prints the current folder
  
  fit_file <- file.path(i, "fit.RData")
  
  if (file.exists(fit_file)) {
    
    load(fit_file)  # loads the 'fit' object
    
    omega_row <- fit$settings$FieldConfig["Omega", ]
    
    model_info <- rbind(
      model_info,
      data.frame(
        model = basename(i),
        rho_config = I(list(fit$settings$RhoConfig)),
        obs_model = I(list(fit$settings$ObsModel)),
        n_x = fit$settings$n_x,
        omega = I(list(omega_row))
      )
    )
    
  } else {
    warning(paste("Missing fit.RData in:", i))
  }
}

