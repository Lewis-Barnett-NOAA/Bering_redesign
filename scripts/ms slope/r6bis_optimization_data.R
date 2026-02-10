####################################################################
####################################################################
##
##    Get true index from the OM, prepare EBS+NBS data for optimization
##    (using devtools::install_github(repo = "zoyafuso-NOAA/SamplingStrata"))
##    Daniel Vilas (daniel.vilas@noaa.gov / dvilasg@uw.edu)
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

# install VAST if it is not
if (!("VAST" %in% installed.packages())) {
  devtools::install_github("james-thorson/VAST@main",
                           INSTALL_opts = "--no-staged-install")
}
library(VAST)

# install akgfmaps to extract shapefile of Alaska
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
         "Paralithodes camtschaticus", "Chionoecetes bairdi",
         "Sebastes alutus", "Sebastes melanostictus", "Atheresthes evermanni",
         "Sebastes borealis", "Sebastolobus alascanus",
         "Glyptocephalus zachirus", "Bathyraja aleutica")

###############################
# converged spp
###############################

df_conv <- read.csv("output/tables/slope_conv.csv")
spp_conv_ebsnbs <- c(df_conv[which(df_conv$EBS_NBS == "convergence"), "spp"])

###################################
# LOAD GRID EBS
###################################

load("data/data_processed/grid_EBS_NBS.RData")
names(grid.ebs_year)[7] <- "Depth"

# load grid of NBS and EBS
northern_bering_sea_grid <- FishStatsUtils::northern_bering_sea_grid
eastern_bering_sea_grid <- FishStatsUtils::eastern_bering_sea_grid
grid <- as.data.frame(rbind(
  data.frame(northern_bering_sea_grid, region = "NBS"),
  data.frame(eastern_bering_sea_grid, region = "EBS")
))
grid$cell <- 1:nrow(grid)

# annually grid data
grid.ebs_year1 <- grid.ebs_year[which(grid.ebs_year$region != "EBSslope"), ]
ncells <- nrow(grid.ebs_year1[which(grid.ebs_year1$Year == 1982), ])

# classify cold and warm years
cyrs <- c(2006:2013)
wyrs <- c(2002:2005, 2014:2016)
yrs <- sort(c(cyrs, wyrs))

# build array for temporal array to store results
temp_dens_vals <- array(NA,
                        dim = c(ncells, length(yrs), length(spp)),
                        dimnames = list(1:ncells, yrs, spp))

# array to store indices
true_index <- array(NA,
                    dim = list(length(yrs), 3, length(spp)),
                    dimnames = list(yrs, c("EBS_NBS", "NBS", "EBS"), spp))

###################################
# LOOP OVER SPECIES
###################################

for (sp in spp) {
  cat(paste(" #############   Species", sp, match(sp, spp),
            "out of", length(spp), " #############\n"))
  
  dir.create(paste0("./output/species/", sp), showWarnings = FALSE)
  dir.create(paste0("./output/species/", sp, "/optimization_data/"),
             showWarnings = FALSE)
  
  if (sp %in% spp_conv_ebsnbs) {
    # load fit file
    ff <- list.files(paste0("./output/vast/", sp, "/"), "fit", recursive = TRUE)
    load(paste0("./output/vast/", sp, "/", ff))
    
    # get predicted densities for sp
    temp_dens_vals[, as.character(yrs), sp] <-
      unlist(fit$Report$D_gct[, 1, as.character(yrs)])
    
    # get true index for NBS_EBS, NBS and EBS
    true_index[paste0(yrs), , sp] <- fit$Report$Index_ctl[1, paste0(yrs), 1:3]
    
    # dataframe of cells with predictions
    D_gt <- data.frame("cell" = c(1:fit$spatial_list$n_g),
                       unlist(fit$Report$D_gct[, 1, as.character(yrs)]))
    colnames(D_gt) <- c("cell", yrs)
    
    # reshape and merge
    D_gt1 <- reshape2::melt(D_gt, id = c("cell"))
    D_gt2 <- merge(D_gt1, grid, by = c("cell"))
    
    mdl <- make_map_info(Region = fit$settings$Region,
                         spatial_list = fit$spatial_list,
                         Extrapolation_List = fit$extrapolation_list)
    
    D <- merge(D_gt2, mdl$PlotDF,
               by.x = c("cell", "Lat", "Lon"),
               by.y = c("x2i", "Lat", "Lon"))
    
    colnames(D) <- c("cell", "Lat", "Lon", "Year", "Density", "Area",
                     "Stratum", "region", "Include")
    
    D$Biomass <- drop_units(D$Density * D$Area)
    
    grid.ebs_year2 <- grid.ebs_year1[which(grid.ebs_year1$Year %in% yrs), ]
    D1 <- merge(D, grid.ebs_year2, by = c("Lat", "Lon", "Year"))
    D1$Density_sq <- (D1$Density)^2
    
    if (sp %in% c("Atheresthes stomias", "Atheresthes evermanni")) {
      D1 <- subset(D1, Year %in% c(1991:2022))
    }
    
    D1$regime <- ifelse(D1$Year %in% cyrs, "cold", "warm")
    
    # DYNAMIC Aggregation
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
    
    D5dyn <- merge(D2dyn, D3dyn, by = c("Lat", "Lon", "cell", "Depth", "regime"))
    D51dyn <- merge(D5dyn, D41dyn,
                    by = c("Lat", "Lon", "cell", "Depth", "regime"))
    D6dyn <- merge(D51dyn, D4dyn,
                   by = c("Lat", "Lon", "cell", "Depth", "regime"))
    
    # STATIC Aggregation
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
    
    D5 <- merge(D2, D3, by = c("Lat", "Lon", "cell", "Depth"))
    D51 <- merge(D5, D41, by = c("Lat", "Lon", "cell", "Depth"))
    D6 <- merge(D51, D4, by = c("Lat", "Lon", "cell", "Depth"))
    
    D6dyn$include <- ifelse(D6dyn$Depth > 0, TRUE, FALSE)
    D6$include <- ifelse(D6$Depth > 0, TRUE, FALSE)
    D6dyn$meanTempF <- (9 / 5) * D6dyn$meanTemp + 32
    D6$meanTempF <- (9 / 5) * D6$meanTemp + 32
    D6dyn$LonE <- D6dyn$Lon + 360
    D6$LonE <- D6$Lon + 360
    
    tdf <- temp_dens_vals[, , sp]
    CPUE_index <- list("CPUE" = D1, "true_index" = true_index[, , sp])
    
  } else {
    D1 <- grid.ebs_year1
    D1$Density <- 0
    D1$Density_sq <- 0
    tdf <- temp_dens_vals[, , sp] <- 0
    true_index[, , sp] <- 0
    CPUE_index <- list("CPUE" = D1, "true_index" = true_index[, , sp])
  }
  
  input_optim <- list("dynamic" = D6dyn, "static" = D6)
  
  save(input_optim,
       file = paste0("./output/species/", sp,
                     "/optimization_data/optimization_static_data_ebsnbs.RData"))
  save(CPUE_index,
       file = paste0("./output/species/", sp,
                     "/optimization_data/OM_CPUE_index_ebsnbs.RData"))
  save(tdf,
       file = paste0("./output/species/", sp,
                     "/optimization_data/fit_temporal_data_ebsnbs.RData"))
}

# Combine species into multi-species optimization data
for (sp in spp) {
  if (sp == spp[1]) {
    load(paste0("./output/species/", sp,
                "/optimization_data/optimization_static_data_ebsnbs.RData"))
    st <- input_optim[["static"]]
    dyn <- input_optim[["dynamic"]]
    dfst <- st[, c("Lat", "Lon", "cell", "Depth", "meanTemp", "varTemp",
                   "include", "meanTempF", "LonE")]
    dfdyn <- dyn[, c("Lat", "Lon", "cell", "Depth", "meanTemp", "varTemp",
                     "include", "meanTempF", "LonE", "regime")]
  }
  
  load(paste0("./output/species/", sp,
              "/optimization_data/optimization_static_data_ebsnbs.RData"))
  
  if (sp %in% setdiff(spp, spp_conv_ebsnbs)) {
    dens <- data.frame(rep(0, ncells), rep(0, ncells))
  } else {
    st <- input_optim[["static"]]
    dyn <- input_optim[["dynamic"]]
    densst <- data.frame(st$sumDensity, st$sumDensity_sq)
    densdyn <- data.frame(dyn$sumDensity, dyn$sumDensity_sq)
  }
  
  names(densst) <- c(paste0(sp, "_sumDensity"), paste0(sp, "_sumDensity_sq"))
  names(densdyn) <- c(paste0(sp, "_sumDensity"), paste0(sp, "_sumDensity_sq"))
  dfst <- cbind(dfst, densst)
  dfdyn <- cbind(dfdyn, densdyn)
}

save(dfst,
     file = "output/slope/multisp_optimization_static_data_ebsnbs_st.RData")
save(dfdyn,
     file = "output/slope/multisp_optimization_static_data_ebsnbs_dyn.RData")