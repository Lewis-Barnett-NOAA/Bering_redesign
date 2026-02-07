####################################################################
####################################################################
##    Shelf and Slope Operating Spatiotemporal Distribution Models
##    Daniel Vilas (danielvilasgonzalez@gmail.com)
##    Lewis Barnett, Stan Kotwicki, Zack Oyafuso, Megsie Siple, Leah Zacher, 
##    Lukas DeFilippo, Andre Punt
####################################################################
####################################################################

# clear all objects
rm(list = ls())

# set seed
# set.seed(6)

# libraries from cran to call or install/load
pack_cran <- c("splines", "ggplot2", "dplyr", "doParallel", "VAST")

# load/install packages
pacman::p_load(pack_cran, character.only = TRUE)

out_dir <- here::here()

# version VAST (cpp)
version <- "VAST_v14_0_1"

# folder region - only slope
fol_region <- "output/slope/vast"
if (!dir.exists(paths = fol_region)) 
  dir.create(path = fol_region, recursive = TRUE)

## 
cpue_bs_allspp <- readRDS(file = "data/data_processed/cpue_bs_allspp.RDS")

# load grid per year for all Bering Sea
grid_bs <- readRDS(file = "data/data_processed/grid_bs.RDS")
grid_bs_year <- readRDS(file = "data/data_processed/grid_bs_year.RDS")

species_list <- read.csv(file = "data/species_list.csv")
species_list <- subset(x = species_list,
                       subset = SPECIES_CODE %in% 
                         unique(x = sort(x = species_list$GROUP_CODE)))

# number of simulations
n_sim <- 100

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Fit VAST models
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# for (ispp in nrow(x = species_list)-1 ) {
for (ispp in 1:nrow(x = species_list)) {
  species_name <- species_list$SCIENTIFIC_NAME[ispp]
  for (iregion in c("bs_slope", "bs_shelf")[1]) {
    
    ## Skip Bering slope model run if it's not included in the slope analysis 
    if (iregion == "bs_slope" & !species_list$SLOPE[ispp]) next 
    
    ## Filter data_geostat to region
    cpue_data <- subset(
      x = cpue_bs_allspp, 
      subset = scientific_name == species_name 
      & survey %in% list("bs_slope" = "BSS",
                         "bs_shelf" = c("EBS", "NBS"))[[iregion]],
      select = c(survey, Lat, Lon, year, scientific_name,
                 weight_kg, area_swept_km2, depth_m, Temp, cpue_kgkm2 )
    ) |>
      setNames(nm = c("Region", "Lat", "Lon", "Year", "Species", "Weight_kg",
                      "Area_km2", "Depth", "Temp", "CPUEkgkm"))
    
    ## Filter interpolation grid to region. The Bering slope region grid only
    ## runs from 2002-2016. 
    interpolation_grid <- 
      subset(x = grid_bs, 
             subset = Region %in% list("bs_slope" = "BSS",
                                       "bs_shelf" = c("EBS", "NBS"))[[iregion]]
      ) 
    
    ## For VAST to provide prediction estimates for all grid cells and years
    ## we rbind the observed catch and effort dataset (data_geostat) with 
    ## the interpolation_grid
    interpolation_grid_year <- grid_bs_year |>
      transform(Species = species_name,
                Weight_kg = mean(cpue_data$Weight_kg), 
                CPUEkgkm = mean(cpue_data$CPUEkgkm),
                Depth = depth_m) |>
      subset(subset = Region %in% list("bs_slope" = "BSS",
                                       "bs_shelf" = c("EBS", "NBS"))[[iregion]],
             select = names(cpue_data))
    
    data_geostat <- rbind(
      cpue_data,
      interpolation_grid_year
    )
    
    ## Covariate Data: combination of the covariate data from the observed
    ## station locations and the interpolation grid
    covariate_data <- data_geostat |> 
      subset(select = c("Lon", "Lat", "Year", "Temp", "Depth"))
    
    ## Scale depth by the mean and sd of the observed depths 
    mean_logdepth <- mean(x = log(x = cpue_data$Depth))
    sd_logdepth <- sd(x = log(x = cpue_data$Depth))
    covariate_data$Depth <- (log(x = covariate_data$Depth) - mean_logdepth) / 
      sd_logdepth
    
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##  Species/Region specific VAST settings to help with convergence issues
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    ## Number of spatial knots for estimation
    knots <- list("bs_shelf" = 500, "bs_slope" = 300)[[iregion]]
    
    ## Anisotropy
    aniso <- ifelse(test = species_name %in% c("Anoplopoma fimbria", 
                                               "Atheresthes stomias") 
                    & iregion == "bs_slope",
                    yes = FALSE, 
                    no = TRUE)
    
    ## Newton Steps
    steps <- ifelse(test = species_name %in% c("Hippoglossoides elassodon",
                                               "Sebastolobus alascanus")  
                    & iregion == "bs_slope",
                    yes = 5, 
                    no = 1)
    
    ## Spatial and Spatiotemporal Field Configurations
    if (species_name %in% 
        c("Reinhardtius hippoglossoides", "Bathyraja aleutica",
          "Hippoglossoides elassodon", "Sebastes alutus",
          "Sebastolobus alascanus") 
        & iregion == "bs_slope") {
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
    
    
    ## Rho configurations
    rhoconfig <- c("Beta1" = 2, "Beta2" = 2,
                   "Epsilon1" = 4, "Epsilon2" = 4)
    
    ## Observation model
    ObsModel <- c(2, 1)
    
    ## Error Distributions, number of knots, and 
    # different specifications that aided convergence in prior runs(?)
    if (species_name %in% 
        c("Sebastes melanostictus","Sebastes alutus",
          "Bathyraja aleutica","Sebastolobus alascanus") &
        iregion == "bs_shelf"
    ) {
      knots <- 300  
      rhoconfig["Epsilon1"] <- 2
      ObsModel <- c(1, 1)
    } 
    
    ## Set up VAST model settings
    settings <- 
      make_settings(n_x = knots,
                    Region = "User",
                    purpose = "index2",
                    bias.correct = FALSE,
                    knot_method = "grid",
                    use_anisotropy = aniso,
                    FieldConfig = fieldconfig,
                    RhoConfig = rhoconfig,
                    Version = version,
                    ObsModel = ObsModel,
                    max_cells = Inf,
                    Options = c("Calculate_Range" = FALSE,
                                "Calculate_effective_area" = FALSE)
      )
    
    ## Setup how the Covariate is modeled
    formula <- 
      list("bs_shelf" = "~ bs(Temp, degree = 3, intercept = FALSE)",
           "bs_slope" = "~ bs(Depth, degree = 2, intercept = FALSE)")[[iregion]]
    X1config_cp <- array(c(1, 1), dim = c(1, 1))
    X2config_cp <- X1config_cp
    
    ## Specify which records in the input dataframe are used for estimation
    ## (pred_TF == 0) and which are used just for prediction (pred_TF == 1)
    pred_TF <- rep(1, nrow(x = data_geostat))
    pred_TF[1:nrow(x = cpue_data)] <- 0
    
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##  Fit model
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    fit <- VAST::fit_model(settings = settings,
                           Lat_i = data_geostat$Lat,
                           Lon_i = data_geostat$Lon,
                           t_i = data_geostat$Year,
                           b_i = data_geostat$Weight_kg,
                           a_i = data_geostat$Area_km2,
                           input_grid = interpolation_grid,
                           getJointPrecision = TRUE,
                           test_fit = FALSE,
                           covariate_data = covariate_data,
                           X1_formula = formula,
                           X2_formula = formula,
                           newtonsteps = steps,
                           PredTF_i = pred_TF,
                           working_dir = paste0("output/", iregion, "/vast/",
                                                species_name, "/"))
    
    ## Save Fit
    saveRDS(object = fit, file = paste0("output/", iregion, "/vast/",
                                        species_name, "/fit.RDS"))
    
    
    
  }
}


# Sim1 <- VAST::simulate_data(fit = fit,
#                             type = 1)
# 
# 
# 
# # simulate historical data
# n_interpolation_cells <- unique(x = table(grid_bss$Year))
# temp_sim_data <- array(NA,
#                        dim = c(n_interpolation_cells,
#                                length(x = unique(data_geostat1$Year)), 
#                                n_sim),
#                        dimnames = list(NULL,
#                                        sort(x = unique(data_geostat1$Year)),
#                                        NULL)  )
# 
# if (inherits(fit, "fit_model")) {
#   save(list = "fit",
#        file = paste(out_dir, fol_region, sp, "fit.RData", sep = "/"))
#   
#   ## Create Simulated Densities
#   for (isim in 1:n_sim) {
#     cat(paste(" ############# Simulation", isim, "of", n_sim, "\n"))
#     Sim1 <- FishStatsUtils::simulate_data(fit = fit, 
#                                           type = 1,
#                                           random_seed = isim)
#     
#     
#     temp_sim_data[, , isim] <- 
#       matrix(data = Sim1$b_i[pred_TF == 1] / grid_bss$Area_in_survey_km2,
#              nrow = n_interpolation_cells,
#              ncol = length(x = unique(x = data_geostat1$Year)))  
#     
#   }
# }
# 
# if (!dir.exists(paths = paste0("output/species/", sp, 
#                                "/simulated_historical_data/")))
#   dir.create(path = paste0("output/species/", sp, 
#                            "/simulated_historical_data/"),
#              recursive = TRUE)
# save(temp_sim_data,
#      file = paste0("output/species/", sp,
#                    "/simulated_historical_data/sim_dens_slope.RData"))
# }
# 
# ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ##  Collate simulated data into one large array
# ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# cl <- makeCluster(detectCores() - 1)
# registerDoParallel(cl)
# 
# sim_dens <- array(data = NA,
#                   dim = c(n_interpolation_cells, 
#                           length(x = species_list$SCIENTIFIC_NAME),
#                           length(x = 2002:2016), 
#                           n_sim),
#                   dimnames = list(NULL, 
#                                   species_list$SCIENTIFIC_NAME,
#                                   2002:2016, 
#                                   NULL))
# 
# foreach(sp = species_list$SCIENTIFIC_NAME) %do% {
#   load(paste0("output/species/", sp,
#               "/simulated_historical_data/sim_dens_slope.RData"))
#   foreach(y = 2002:2016) %:%
#     foreach(sim = 1:n_sim) %do% {
#       sim_dens1[, sp, as.character(y), as.character(sim)] <-
#         sim_dens[, as.character(y), as.character(sim)]
#     }
# }
# 
# stopCluster(cl)
# save(sim_dens1, file = "output/slope/species/ms_sim_dens_slope.RData")


for (ispp in 1:nrow(x = species_list)) {
  species_name <- species_list$SCIENTIFIC_NAME[ispp]
  for (iregion in c("bs_slope", "bs_shelf")[1]) {
    
    ## Skip Bering slope model run if it's not included in the slope analysis 
    if (iregion == "bs_slope" & !species_list$SLOPE[ispp]) next 

    
    cat(iregion, species_name , "\n")  
  }
}
