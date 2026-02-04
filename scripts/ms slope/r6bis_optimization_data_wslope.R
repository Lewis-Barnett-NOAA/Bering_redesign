####################################################################
####################################################################
##
##    Get true index from the OM, prepare data for optimization
##    (using devtools::install_github(repo = "zoyafuso-NOAA/SamplingStrata"))
##    Daniel Vilas (daniel.vilas@noaa.gov/dvilasg@uw.edu)
##
####################################################################
####################################################################

# clear all objects
rm(list = ls(all.names = TRUE))
# free up memory and report the memory usage
gc()

# libraries from cran to call or install/load
pack_cran <- c("splines", "SamplingStrata", "wesanderson", "dplyr", "sp",
               "rgeos", "scales", "rnaturalearth", "grid", "ggplot2")

# install pacman to use p_load function
if (!("pacman" %in% installed.packages())) {
  install.packages("pacman")
}

# install VAST
if (!("VAST" %in% installed.packages())) {
  devtools::install_github("james-thorson/VAST@main",
                           INSTALL_opts = "--no-staged-install")
}
library(VAST)

# install akgfmaps
if (!("akgfmaps" %in% installed.packages())) {
  devtools::install_github("afsc-gap-products/akgfmaps")
}
library(akgfmaps)

# load/install packages
pacman::p_load(pack_cran, character.only = TRUE)

# version VAST (cpp)
version <- "VAST_v14_0_1"

# selected species
spp <- c("Limanda aspera", "Gadus chalcogrammus", "Gadus macrocephalus",
         "Atheresthes stomias", "Reinhardtius hippoglossoides",
         "Lepidopsetta polyxystra", "Hippoglossoides elassodon",
         "Pleuronectes quadrituberculatus", "Hippoglossoides robustus",
         "Boreogadus saida", "Eleginus gracilis", "Anoplopoma fimbria",
         "Chionoecetes opilio", "Paralithodes platypus",
         "Paralithodes camtschaticus", "Chionoecetes bairdi", "Sebastes alutus",
         "Sebastes melanostictus", "Atheresthes evermanni", "Sebastes borealis",
         "Sebastolobus alascanus", "Glyptocephalus zachirus",
         "Bathyraja aleutica")

# classify cold and warm years
cyrs <- c(2006:2013)
wyrs <- c(2002:2005, 2014:2016)
n_yrs <- length(c(cyrs, wyrs))
yrs <- sort(c(cyrs, wyrs))

#####################
# check the slope model that converged
#####################

df_conv <- read.csv("output/tables/slope_conv.csv")

spp_conv_ebsnbs <- c(df_conv[which(df_conv$EBS_NBS == "convergence"), "spp"])
spp_conv_slope <- c(df_conv[which(df_conv$slope_st == "convergence"), "spp"])

###################################
# LOAD GRID EBS
###################################

# load slope grid
bering_sea_slope_grid <- FishStatsUtils::bering_sea_slope_grid
names(bering_sea_slope_grid)[4] <- "Stratum"
bering_sea_slope_grid$Stratum <- 999

# load EBS+NBS grid
northern_bering_sea_grid <- FishStatsUtils::northern_bering_sea_grid
eastern_bering_sea_grid <- FishStatsUtils::eastern_bering_sea_grid
grid <- as.data.frame(rbind(
  data.frame(northern_bering_sea_grid, region = "NBS"),
  data.frame(eastern_bering_sea_grid, region = "EBS"),
  data.frame(bering_sea_slope_grid, region = "SLP")
))
grid$cell <- as.numeric(1:nrow(grid))
gridslope <- grid[which(grid$region == "SLP"), ]

# load grid all years
load(file = "data/data_processed/grid_EBS_NBS.RData")

# years slope
yrs_slope <- c(2002, 2004, 2008, 2010, 2012, 2016)

# build array for temporal array to store results
temp_dens_vals <- array(
  NA,
  dim = c(nrow(gridslope), length(yrs), length(spp)),
  dimnames = list(1:nrow(gridslope), yrs, spp)
)

# array to store indices
true_index <- array(
  NA,
  dim = list(length(yrs), 1, length(spp)),
  dimnames = list(yrs, "slope", spp)
)

###################################
# LOOP OVER SPECIES
###################################

for (sp in spp) {
  cat(paste("#######################", sp, "#######################\n"))
  
  dir.create(paste0("./output/species/", sp), showWarnings = FALSE)
  dir.create(paste0("./output/species/", sp, "/optimization data/"),
             showWarnings = FALSE)
  
  if (sp %in% spp_conv_slope) {
    # load model
    load(paste0("./output/slope/vast/", sp, "/fit_st.RData"))
    
    # get predicted densities for sp
    temp_dens_vals[, , sp] <- unlist(fit$Report$D_gct[, 1, as.character(yrs)])
    D_gt <- unlist(fit$Report$D_gct[, 1, as.character(yrs)])
    
    # get true index
    true_index[, , sp] <- fit$Report$Index_ctl[1, paste0(yrs), 1]
    
    # get map info
    mdl <- make_map_info(
      Region = fit$settings$Region,
      spatial_list = fit$spatial_list,
      Extrapolation_List = fit$extrapolation_list
    )
    mdl$PlotDF$x2i <- mdl$PlotDF$x2i + 53464
    
  } else {
    D_gt <- data.frame(matrix(0, nrow = nrow(gridslope), ncol = length(yrs)))
    names(D_gt) <- as.character(yrs)
    true_index[, , sp] <- 0
    
    load(paste0("./output/slope/vast/", spp_conv_slope[1], "/fit_st.RData"))
    mdl <- make_map_info(
      Region = fit$settings$Region,
      spatial_list = fit$spatial_list,
      Extrapolation_List = fit$extrapolation_list
    )
    mdl$PlotDF$x2i <- mdl$PlotDF$x2i + 53464
  }
  
  # dataframe of cells with predictions
  D_gt <- data.frame("cell" = c(min(gridslope$cell):max(gridslope$cell)), D_gt)
  colnames(D_gt) <- c("cell", yrs)
  
  # reshape and merge
  D_gt1 <- reshape2::melt(D_gt, id = c("cell"))
  D_gt2 <- merge(D_gt1, gridslope, by = c("cell"))
  D <- merge(D_gt2, mdl$PlotDF,
             by.x = c("cell", "Lat", "Lon"),
             by.y = c("x2i", "Lat", "Lon"))
  
  colnames(D) <- c("cell", "Lat", "Lon", "Year", "Density", "Area",
                   "Stratum", "region", "Include")
  
  D$Biomass <- D$Density * D$Area
  if (sp %in% spp_conv_slope) {
    D$Biomass <- drop_units(D$Biomass)
  }
  
  # grid data merge
  grid.ebs_year1 <- grid.ebs_year[which(grid.ebs_year$Year %in% yrs &
                                          grid.ebs_year$region == "EBSslope"), ]
  grid.ebs_year1 <- grid.ebs_year1[, c("Lat", "Lon", "Area_in_survey_km2",
                                       "DepthGEBCO", "Temp", "Year")]
  names(grid.ebs_year1)[4] <- "Depth"
  
  D1 <- merge(D, grid.ebs_year1, by = c("Lat", "Lon", "Year"))
  D1$Biomass_sq <- (D1$Biomass)^2
  D1$Density_sq <- (D1$Density)^2
  
  if (sp %in% c("Atheresthes stomias", "Atheresthes evermanni")) {
    D1 <- subset(D1, Year %in% c(1991:2022))
  }
  
  D1$regime <- ifelse(D1$Year %in% cyrs, "cold", "warm")
  
  # Aggregate DYNAMIC
  D2dyn <- aggregate(cbind(Temp, Density) ~ Lat + Lon + cell + Depth + regime,
                     data = D1, FUN = mean, na.rm = TRUE)
  D3dyn <- aggregate(cbind(Temp, Density) ~ Lat + Lon + cell + Depth + regime,
                     data = D1, FUN = var, na.rm = TRUE)
  D4dyn <- aggregate(cbind(Density) ~ Lat + Lon + cell + Depth + regime,
                     data = D1, FUN = sum, na.rm = TRUE)
  D41dyn <- aggregate(cbind(Density_sq) ~ Lat + Lon + cell + Depth + regime,
                      data = D1, FUN = sum, na.rm = TRUE)
  
  colnames(D2dyn)[6:7] <- paste0("mean", colnames(D2dyn)[6:7])
  colnames(D3dyn)[6:7] <- paste0("var", colnames(D3dyn)[6:7])
  colnames(D4dyn)[6] <- paste0("sum", colnames(D4dyn)[6])
  colnames(D41dyn)[6] <- paste0("sum", colnames(D41dyn)[6])
  
  D6dyn <- merge(merge(merge(D2dyn, D3dyn,
                             by = c("Lat", "Lon", "cell", "Depth", "regime")),
                       D41dyn, by = c("Lat", "Lon", "cell", "Depth", "regime")),
                 D4dyn, by = c("Lat", "Lon", "cell", "Depth", "regime"))
  
  # Aggregate STATIC
  D2 <- aggregate(cbind(Temp, Density) ~ Lat + Lon + cell + Depth,
                  data = D1, FUN = mean, na.rm = TRUE)
  D3 <- aggregate(cbind(Temp, Density) ~ Lat + Lon + cell + Depth,
                  data = D1, FUN = var, na.rm = TRUE)
  D4 <- aggregate(cbind(Density) ~ Lat + Lon + cell + Depth,
                  data = D1, FUN = sum, na.rm = TRUE)
  D41 <- aggregate(cbind(Density_sq) ~ Lat + Lon + cell + Depth,
                   data = D1, FUN = sum, na.rm = TRUE)
  
  colnames(D2)[5:6] <- paste0("mean", colnames(D2)[5:6])
  colnames(D3)[5:6] <- paste0("var", colnames(D3)[5:6])
  colnames(D4)[5] <- paste0("sum", colnames(D4)[5])
  colnames(D41)[5] <- paste0("sum", colnames(D41)[5])
  
  D6 <- merge(merge(merge(D2, D3, by = c("Lat", "Lon", "cell", "Depth")),
                    D41, by = c("Lat", "Lon", "cell", "Depth")),
              D4, by = c("Lat", "Lon", "cell", "Depth"))
  
  D6dyn$LonE <- D6dyn$Lon + 360
  D6$LonE <- D6$Lon + 360
  
  CPUE_index <- list("CPUE" = D1, "true_index" = true_index[, , sp])
  input_optim <- list("dynamic" = D6dyn, "static" = D6)
  
  save(input_optim,
       file = paste0("./output/species/", sp,
                     "/optimization data/optimization_static_data_slope.RData"))
  save(CPUE_index,
       file = paste0("./output/species/", sp,
                     "/optimization data/OM_CPUE_index_slope.RData"))
}

# Final Combine
for (sp in spp) {
  load(paste0("./output/species/", sp,
              "/optimization data/optimization_static_data_slope.RData"))
  st <- input_optim[["static"]]
  dyn <- input_optim[["dynamic"]]
  
  if (sp == spp[1]) {
    dfst <- st[, c("Lat", "Lon", "cell", "Depth", "meanTemp", "varTemp",
                   "meanTempF", "LonE")]
    dfdyn <- dyn[, c("Lat", "Lon", "cell", "Depth", "meanTemp", "varTemp",
                     "meanTempF", "LonE", "regime")]
  }
  
  densst <- st[, c("sumDensity", "sumDensity_sq")]
  densdyn <- dyn[, c("sumDensity", "sumDensity_sq")]
  colnames(densst) <- c(paste0(sp, "_sumDensity"), paste0(sp, "_sumDensity_sq"))
  colnames(densdyn) <- c(paste0(sp, "_sumDensity"), paste0(sp, "_sumDensity_sq"))
  
  dfst <- cbind(dfst, densst)
  dfdyn <- cbind(dfdyn, densdyn)
}

save(dfst, file = "output/slope/multisp_optimization_static_data_slope_st.RData")
save(dfdyn, file = "output/slope/multisp_optimization_static_data_slope_dyn.RData")