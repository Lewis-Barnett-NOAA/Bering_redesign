########################
##
## slope operating model (shelf-slope combined model did not work)
##
########################

# clear all objects
rm(list = ls(all.names = TRUE))
# free up memrory and report the memory usage
gc()

# set seed
set.seed(6)

# libraries from cran to call or install/load
pack_cran <- c("splines", "ggplot2", "dplyr", "doParallel")

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

# load/install packages
pacman::p_load(pack_cran, character.only = TRUE)

out_dir <- here::here()

# version VAST (cpp)
version <- "VAST_v14_0_1"

# number of knots
knots <- "300"

# list of sp
# splist <- list.dirs("data/data_processed/",
#                     full.names = FALSE,
#                     recursive = FALSE)

# folder region - only slope
fol_region <- "output/slope/vast"
if (!dir.exists(paths = fol_region)) 
  dir.create(path = fol_region, recursive = TRUE)


## load grid
load('data/data_processed/grid_bs.RData')
grid_bs_year <- grid_bs_year |> 
  subset(subset = region == "EBSslope" & Year %in% 2002:2016)

species_list <- read.csv(file = "data/species_list.csv")
species_list <- subset(x = species_list,
                       subset = SPECIES_CODE %in% 
                         unique(x = sort(x = species_list$GROUP_CODE)) 
                       & SLOPE == TRUE)

spp_vect <- c("Atheresthes evermanni", "Atheresthes stomias",
              "Gadus chalcogrammus", "Gadus macrocephalus",
              "Hippoglossoides elassodon", "Reinhardtius hippoglossoides",
              "Bathyraja aleutica")

# add grid to get prediction for simulate data on each cell of the grid
# bering_sea_slope_grid <- FishStatsUtils::bering_sea_slope_grid
# names(bering_sea_slope_grid)[4] <- "Stratum"
# bering_sea_slope_grid$Stratum <- 99

# load grid per year for all EBS
# load(file = "data/data_processed/grid_EBS_NBS.RData")
# grid_ebs <- subset(x = grid.ebs_year,
#                    subset = region == "EBSslope" & Year %in% 2002:2016)
# 
# splist <- list()
# splist <- vector("list", length(spp))
# names(splist) <- spp
# for (sp in spp) {
#   # read data
#   if (sp %in% spp_vect) {
#     df1 <- readRDS(file = paste0("data/data_processed/species/", sp,
#                                  "/data_geostat_slope_adj.rds"))
#   } else {
#     df1 <- readRDS(file = paste0("data/data_processed/species/", sp,
#                                  "/data_geostat.rds"))
#     df1$ADJ_KG_HA <- NA_real_
#   }
#   
#   # slope only
#   df1 <- subset(
#     x = df1,
#     subset = survey_name == "Eastern Bering Sea Slope Bottom Trawl Survey"
#   )
#   df1$survey_name <- "slope"
#   
#   yrs <- unique(x = df1$year)
#   df1 <- subset(x = df1, subset = year %in% yrs)
#   
#   # unified CPUE
#   if (sp %in% spp_vect) {
#     df1$cpue_kgha <- df1$ADJ_KG_HA
#   } 
#   
#   
#   # standard columns
#   df1 <- df1[, c("lat_start", "lon_start", "year", "scientific_name",
#                  "cpue_kgha", "effort", "depth_m", "survey_name")]
#   
#   colnames(df1) <- c("Lat", "Lon", "Year", "Species", "Cpue_kgha",
#                      "Effort", "Depth", "Region")
#   
#   # weight
#   df1$Weight_kg <- df1$Cpue_kgha * df1$Effort
#   df1$CPUEkgkm <- df1$Cpue_kgha
#   
#   # grid for predictions
#   grids <- data.frame(
#     Lat = grid_ebs$Lat,
#     Lon = grid_ebs$Lon,
#     Year = grid_ebs$Year,
#     Species = sp,
#     Weight_kg = mean(df1$Weight_kg, na.rm = TRUE),
#     Effort = grid_ebs$Area_in_survey_km2,
#     Depth = grid_ebs$DepthGEBCO,
#     Region = grid_ebs$region,
#     CPUEkgkm = mean(df1$Cpue_kgha, na.rm = TRUE),
#     stringsAsFactors = TRUE
#   )
#   
#   # bind observations and grid
#   df1 <- rbind(
#     df1[, c("Lat", "Lon", "Year", "Species", "Weight_kg", "Effort",
#             "Depth", "Region", "CPUEkgkm")],
#     grids
#   )
#   
#   splist[[sp]] <- df1
# }

# bind all species
# df2 <- dplyr::bind_rows(splist)
# BSS_data_geostat <- df2

# add missing environmental covariate
# BSS_data_geostat$SBT_insitu <- NA_real_

# remove missing weights
# BSS_data_geostat <- 
# BSS_data_geostat[complete.cases(BSS_data_geostat$Weight_kg), ]

# convert area from ha to km2
# BSS_data_geostat$Effort <- BSS_data_geostat$Effort / 100

# final layout
# BSS_data_geostat <- BSS_data_geostat[, c("Lat", "Lon", "Year", "Species",
# "Weight_kg", "Effort", "Depth",
# "SBT_insitu", "Region")]

# colnames(BSS_data_geostat) <- c("Lat", "Lon", "Year", "Species", "Weight_kg",
#                                 "Swept_area", "Depth", "SBT_insitu", "Region")

# BSS_data_geostat$Region <- "BSS"

# save rds all species BSS data_geostat
# saveRDS(object = BSS_data_geostat,
#         file = paste("data/data_processed/data_geostat_BSS.rds"))

# ebs_layers <- akgfmaps::get_base_layers(select.region = "ebs",
#                                         set.crs = "EPSG:3338")
# ebs_layers$survey.strata <- sf::st_transform(x = ebs_layers$survey.strata,
#                                              crs = "EPSG:4326")

# selected species - remove spp without observations in the slope
# spp_slope <- c("Gadus chalcogrammus", "Gadus macrocephalus",
#                "Atheresthes stomias", "Reinhardtius hippoglossoides",
#                "Hippoglossoides elassodon", "Anoplopoma fimbria",
#                "Chionoecetes opilio", "Chionoecetes bairdi",
#                "Sebastes alutus", "Sebastes melanostictus",
#                "Atheresthes evermanni", "Sebastes borealis",
#                "Sebastolobus alascanus", "Glyptocephalus zachirus",
#                "Bathyraja aleutica")

# number of simulations
n_sim_hist <- 100

# fit with depth or CPE
# df_conv <- data.frame(spp = c(spp))
# df_conv$slope_st <- NA

# load grid per year for all EBS
load(file = "data/data_processed/grid_bs.RData")
grid_bss <- subset(x = grid_bs_year, 
                   subset = region == "EBSslope" & 
                     Year %in% 2002:2016)

# loop over species
for (sp in species_list$SCIENTIFIC_NAME) {
  # cat(paste0("############### ", sp, " #########################\n"))
  
  # filter by sp
  data_geostat <-
    readRDS(file = paste0("data/data_processed/species/", sp, 
                          "/data_geostat.rds")) |>
    subset(subset = survey  == "BSS",
           select = c(lat_start, lon_start, year, scientific_name, 
                      weight_kg, area_swept_km2, depth_m,
                      ScaleLogDepth, survey, cpue_kgkm2 ))
  names(x = data_geostat) <- c("Lat", "Lon", "Year", "Species", "Weight_kg",
                               "Effort", "depth_m", "depth_scaled", 
                               "Region", "CPUEkgkm")
  
  # grid with info to get prediction
  data_geostat1 <- rbind(
    data_geostat[, c("Lat", "Lon", "Year", "Species", "Weight_kg", "Effort",
                     "depth_scaled", "Region", "CPUEkgkm")],
    data.frame(Lat = grid_bss$Lat,
               Lon = grid_bss$Lon,
               Year = grid_bss$Year,
               Species = sp,
               Weight_kg = mean(data_geostat$Weight_kg),
               Effort = grid_bss$Area_in_survey_km2,
               Depth = (log(grid_bss$depth_m) - mean(log(data_geostat$depth_m))) /
                 sd(log(data_geostat$depth_m)) ,
               Region = grid_bss$region,
               CPUEkgkm = mean(data_geostat$CPUEkgkm),
               stringsAsFactors = TRUE)
  )
  
  (scaled_data * attr(scaled_data, "scaled:scale")) + 
    attr(scaled_data, "scaled:center")
  
  # scale depth
  data_geostat1$ScaleLogDepth <- scale(x = log(x = data_geostat1$Depth))
  
  # create folder and save data
  dir.create(paste(fol_region, sp, sep = "/"), showWarnings = FALSE)
  save(data_geostat1,
       file = paste(fol_region, sp,
                    "data_geostat_temp_adj.RData", sep = "/"))
  
  # percentage of zeros
  percent_zeros <- data_geostat %>%
    group_by(Year) %>%
    summarize(percentage_zeros = mean(Weight_kg == 0) * 100)
  print(percent_zeros)
  
  region <- "bering_sea_slope"
  aniso <- ifelse(test = sp %in% c("Anoplopoma fimbria", "Atheresthes stomias"),
                  yes = FALSE, 
                  no = TRUE)
  
  if (sp %in% c("Reinhardtius hippoglossoides", "Bathyraja aleutica",
                "Hippoglossoides elassodon", "Sebastes alutus",
                "Sebastes melanostictus", "Sebastolobus alascanus")) {
    fieldconfig <- matrix(c(0, "IID", 0, "Identity", 0, "IID", 0, "Identity"),
                          ncol = 2, nrow = 4,
                          dimnames = list(c("Omega", "Epsilon", "Beta",
                                            "Epsilon_year"),
                                          c("Component_1", "Component_2")))
  } else {
    fieldconfig <- matrix(c("IID", "IID", 0, "Identity",
                            "IID", "IID", 0, "Identity"),
                          ncol = 2, nrow = 4,
                          dimnames = list(c("Omega", "Epsilon", "Beta",
                                            "Epsilon_year"),
                                          c("Component_1", "Component_2")))
  }
  
  # VAST model settings
  settings <- make_settings(n_x = knots,
                            Region = region,
                            purpose = "index2",
                            bias.correct = FALSE,
                            knot_method = "grid",
                            use_anisotropy = aniso,
                            FieldConfig = fieldconfig,
                            RhoConfig = c("Beta1" = 2, "Beta2" = 2,
                                          "Epsilon1" = 4, "Epsilon2" = 4),
                            Version = version,
                            ObsModel = c(2, 1),
                            max_cells = Inf,
                            Options = c("Calculate_Range" = FALSE,
                                        "Calculate_effective_area" = FALSE))
  
  # covariate data
  if (cov == "depth") {
    covariate_data <- data_geostat1[complete.cases(data_geostat1[, "Depth"]), ]
    covariate_data$Year <- NA
    formula <- " ~ bs(ScaleLogDepth, degree=2, intercept=FALSE)"
    X1config_cp <- array(c(1, 1), dim = c(1, 1))
    X2config_cp <- X1config_cp
  }
  
  pred_TF <- rep(1, nrow(x = data_geostat1))
  pred_TF[1:nrow(x = data_geostat)] <- 0
  steps <- ifelse(test = sp %in% c("Hippoglossoides elassodon",
                                   "Sebastes melanostictus",
                                   "Sebastolobus alascanus"), 
                  yes = 5, 
                  no = 1)
  
  # fit
  fit <- tryCatch({
    fit_model(settings = settings,
              Lat_i = data_geostat1$Lat,
              Lon_i = data_geostat1$Lon,
              t_i = data_geostat1$Year,
              b_i = data_geostat1$Weight_kg,
              c_iz = as.numeric(factor(data_geostat1$Species)) - 1,
              a_i = data_geostat1$Effort,
              input_grid = bering_sea_slope_grid,
              getJointPrecision = TRUE,
              test_fit = FALSE,
              covariate_data = covariate_data,
              X1_formula = formula,
              X2_formula = formula,
              newtonsteps = steps,
              PredTF_i = pred_TF,
              working_dir = paste(fol_region, sp, "/", sep = "/"))
  }, error = function(cond) {
    message("Did not converge.")
    return(NULL)
  })
  
  df_conv[which(df_conv$spp == sp), "slope_st"] <-
    fit$parameter_estimates$Convergence_check
  
  # simulate historical data
  sim_dens <- array(NA,
                    dim = c(nrow(x = bering_sea_slope_grid),
                            length(x = unique(data_geostat1$Year)), 
                            n_sim_hist),
                    dimnames = list(1:nrow(x = bering_sea_slope_grid),
                                    sort(x = unique(data_geostat1$Year)),
                                    1:n_sim_hist))
  
  if (inherits(fit, "fit_model")) {
    save(list = "fit",
         file = paste(out_dir, fol_region, sp, "fit_st.RData", sep = "/"))
    
    for (isim in 1:n_sim_hist) {
      cat(paste(" ############# Simulation", isim, "of", n_sim_hist, "\n"))
      Sim1 <- FishStatsUtils::simulate_data(fit = fit, type = 1,
                                            random_seed = isim)
      sim_bio <- matrix(data = Sim1$b_i[pred_TF == 1],
                        nrow = nrow(bering_sea_slope_grid),
                        ncol = length(unique(data_geostat1$Year)))
      sim_dens[, , isim] <- sim_bio / bering_sea_slope_grid$Area_in_survey_km2
    }
  }
  
  if (!dir.exists(paths = paste0("output/species/", sp, 
                                 "/simulated_historical_data/")))
    dir.create(path = paste0("output/species/", sp, 
                             "/simulated_historical_data/"),
               recursive = TRUE)
  save(sim_dens,
       file = paste0("output/species/", sp,
                     "/simulated_historical_data/sim_dens_slope.RData"))
}

# RESHAPE simulated data
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)
n_sim <- 100

sim_dens1 <- array(data = NA,
                   dim = c(nrow(x = bering_sea_slope_grid), 
                           length(x = spp),
                           length(x = 2002:2016), 
                           n_sim),
                   dimnames = list(1:nrow(x = bering_sea_slope_grid), 
                                   spp,
                                   sort(2002:2016), 
                                   1:n_sim))

foreach(sp = spp_slope) %do% {
  load(paste0("output/species/", sp,
              "/simulated_historical_data/sim_dens_slope.RData"))
  foreach(y = 2002:2016) %:%
    foreach(sim = 1:n_sim) %do% {
      sim_dens1[, sp, as.character(y), as.character(sim)] <-
        sim_dens[, as.character(y), as.character(sim)]
    }
}

stopCluster(cl)
save(sim_dens1, file = "output/slope/species/ms_sim_dens_slope.RData")

# join data_geostat for both regions
BSS_data_geostat <- readRDS(
  file = file.path(out_dir, "data processed", "data_geostat_BSS.rds")
)
NBSEBS_data_geostat <- readRDS(
  file = file.path(out_dir, "data processed", "data_geostat_NBSEBS.rds")
)

stopifnot(identical(names(x = BSS_data_geostat), 
                    names(x = NBSEBS_data_geostat)))

data_geostat_all <- dplyr::bind_rows(BSS_data_geostat, NBSEBS_data_geostat)
saveRDS(obj = data_geostat_all,
        file.path(out_dir, "data processed", "data_geostat_BSS_NBSEBS.rds"))
