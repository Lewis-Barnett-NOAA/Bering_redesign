####################################################################
####################################################################
##    Shelf and Slope Operating Spatiotemporal Distribution Models
##    Daniel Vilas (danielvilasgonzalez@gmail.com)
##    Lewis Barnett, Stan Kotwicki, Zack Oyafuso, Megsie Siple, Leah Zacher, 
##    Lukas DeFilippo, Andre Punt
####################################################################
####################################################################
rm(list = ls())

## Import libraries
library(VAST)
library(splines)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Import data
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Catch and Effort Data
cpue_bs_allspp <- readRDS(file = "data/data_processed/cpue_bs_allspp.RDS")

## Interpolation Grid
grid_bs <- readRDS(file = "data/data_processed/grid_bs.RDS")

## Prediction Grid: interpolation grid for each year
grid_bs_year <- readRDS(file = "data/data_processed/grid_bs_year.RDS")

## Species List
species_list <- read.csv(file = "data/species_list.csv") |>
  subset(subset = SPECIES_CODE %in% 
           unique(x = sort(x = GROUP_CODE)))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Fit VAST models
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

for (ispp in 1:nrow(x = species_list)) {
  species_name <- species_list$SCIENTIFIC_NAME[ispp]
  for (iregion in c("bs_slope", "bs_shelf")) {
    
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
                                       "bs_shelf" = c("EBS", "NBS"))[[iregion]] 
             & Year %in% seq(from = list("bs_slope" = 2002,
                                         "bs_shelf" = 1982)[[iregion]],
                             to = list("bs_slope" = 2016,
                                       "bs_shelf" = 2022)[[iregion]],
                             by = 1) ,
             select = names(cpue_data))
    
    ## Specify which records in the input dataframe are used for estimation
    ## (pred_TF == 0) and which are used just for prediction (pred_TF == 1)
    data_geostat_w_grid <- rbind(
      cpue_data,
      interpolation_grid_year
    )
    data_geostat_w_grid$pred_TF <- rep(1, nrow(x = data_geostat_w_grid))
    data_geostat_w_grid$pred_TF[1:nrow(x = cpue_data)] <- 0
    
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##  Covariate Data: combination of the covariate data from the observed
    ##  station locations and the interpolation grid
    ##  Scale input data and covariate depts by the mean and sd of obs. depths 
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    covariate_data <- data_geostat_w_grid |> 
      subset(select = c("Lon", "Lat", "Year", "Temp", "Depth"))
    
    mean_logdepth <- mean(x = log(x = cpue_data$Depth))
    sd_logdepth <- sd(x = log(x = cpue_data$Depth))
    
    data_geostat_w_grid$Depth <- 
      (log(x = data_geostat_w_grid$Depth) - mean_logdepth) / 
      sd_logdepth
    covariate_data$Depth <-
      (log(x = covariate_data$Depth) - mean_logdepth) / 
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
    beta_field = 0# "IID" for estimating year intercepts
    beta_rho = 2 ## for random walk
    
    if (species_name %in% 
        c("Reinhardtius hippoglossoides", "Bathyraja aleutica",
          "Hippoglossoides elassodon", "Sebastes alutus",
          "Sebastolobus alascanus") 
        & iregion == "bs_slope") {
      fieldconfig <- matrix(c(0, "IID", beta_field, "Identity", 
                              0, "IID", beta_field, "Identity"),
                            ncol = 2, nrow = 4,
                            dimnames = list(c("Omega", "Epsilon", "Beta",
                                              "Epsilon_year"),
                                            c("Component_1", "Component_2")))
    } else {
      fieldconfig <- matrix(c("IID", "IID", beta_field, "Identity",
                              "IID", "IID", beta_field, "Identity"),
                            ncol = 2, nrow = 4,
                            dimnames = list(c("Omega", "Epsilon", "Beta",
                                              "Epsilon_year"),
                                            c("Component_1", "Component_2")))
    }
    
    ## Rho configurations
    rhoconfig <- c("Beta1" = beta_rho, "Beta2" = beta_rho,
                   "Epsilon1" = 4, "Epsilon2" = 4)
    
    # Need different link for species with 100% encounters if have betas as fixed effects
    mins <- data_geostat_w_grid |> dplyr::group_by(Year) |> dplyr::summarize(min = min(Weight_kg))
    if(sum(mins$min) != 0 & beta_rho == 0 & beta_field != 0){
      link = 4
    } else {
      link = 1
    }
    
    ## Observation model
    ObsModel <- c(2, link)
    
    ## Error Distributions, number of knots, and 
    # different specifications that aided convergence in prior runs(?)
    if (species_name %in% 
        c("Sebastes melanostictus","Sebastes alutus",
          "Bathyraja aleutica","Sebastolobus alascanus") &
        iregion == "bs_shelf"
    ) {
      knots <- 300  
      rhoconfig["Epsilon1"] <- 2
      ObsModel <- c(1, link)
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
                    Version = "VAST_v14_0_1",
                    ObsModel = ObsModel,
                    max_cells = Inf,
                    mesh_package = "fmesher",
                    Options = c("Calculate_Range" = FALSE,
                                "Calculate_effective_area" = FALSE)
      )
    
    ## Setup how the Covariate is modeled
    formula <- 
      list("bs_shelf" = "~ bs(Temp, degree = 3, intercept = FALSE)",
           "bs_slope" = "~ bs(Depth, degree = 2, intercept = FALSE)")[[iregion]]
    X1config_cp <- array(c(1, 1), dim = c(1, 1))
    X2config_cp <- X1config_cp
    
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##  Initial fitted model with just the observed stations, 
    ##  i.e., records where pred_TF == 0
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    initial_fit <-
      VAST::fit_model(
        settings = settings,
        Lat_i = data_geostat_w_grid$Lat[data_geostat_w_grid$pred_TF == 0],
        Lon_i = data_geostat_w_grid$Lon[data_geostat_w_grid$pred_TF == 0],
        t_i = data_geostat_w_grid$Year[data_geostat_w_grid$pred_TF == 0],
        b_i = data_geostat_w_grid$Weight_kg[data_geostat_w_grid$pred_TF == 0],
        a_i = data_geostat_w_grid$Area_km2[data_geostat_w_grid$pred_TF == 0],
        input_grid = interpolation_grid,
        getJointPrecision = TRUE,
        test_fit = FALSE,
        covariate_data = covariate_data,
        X1_formula = formula,
        X2_formula = formula,
        newtonsteps = steps,
        working_dir = paste0("output/", iregion, "/vast/", species_name, "/")
      )
    
    ## Save Fit
    saveRDS(object = initial_fit,
            file = paste0("output/", iregion, "/vast/",
                          species_name, "/initial_fit.RDS"))
    
    # load(paste0("output/", iregion, "/vast/", 
    #             species_name, "/parameter_estimates.RData"))
    
    ## Refit the model with the prediction grid in the input data. The 
    ## PredTF_i argument tells the model not to include the prediction grids 
    ## in the model estimation, just in the predictions. Use the initial 
    ## estimated parameters as a starting point to speed up estimation in this
    ## second round. 
    fit_w_preds <- 
      VAST::fit_model(settings = settings,
                      Lat_i = data_geostat_w_grid$Lat,
                      Lon_i = data_geostat_w_grid$Lon,
                      t_i = data_geostat_w_grid$Year,
                      b_i = data_geostat_w_grid$Weight_kg,
                      a_i = data_geostat_w_grid$Area_km2,
                      input_grid = interpolation_grid,
                      run_model =  FALSE,
                      covariate_data = covariate_data,
                      X1_formula = formula,
                      X2_formula = formula,
                      newtonsteps = steps,
                      PredTF_i = data_geostat_w_grid$pred_TF,
                      startpar = initial_fit$parameter_estimates$par,
                      working_dir = paste0("output/", iregion, "/vast/",
                                           species_name, "/"))
    
    ## Save Fit
    saveRDS(object = fit_w_preds, 
            file = paste0("output/", iregion, "/vast/",
                          species_name, "/fit_w_preds.RDS"))
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


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Collate convergence of each model
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_conv <- data.frame(species_name = sort(species_list$SCIENTIFIC_NAME),
                      bs_shelf = NA, 
                      bs_slope = NA)

for (iregion in c("bs_slope", "bs_shelf")) { ## Loop over regions -- start
  for (ispp in 1:nrow(x = df_conv)) { ## Loop over species -- start
    
    fit_filename <- paste0("output/", iregion, "/vast/",  
                           df_conv$species_name[ispp], 
                           "/parameter_estimates.RData")
    
    if (file.exists(fit_filename)) {
      load(fit_filename)
      max_grad <- parameter_estimates$max_gradient
      
      df_conv[ispp, iregion] <-
        ifelse(test = is.null(x = max_grad),
               yes = "did not converge",
               no = ifelse(test = max_grad < 0.01,
                           yes = "converged", 
                           no = "did not converge"))
    } else (df_conv[ispp, iregion] <- "model not fitted")
    
  } ## Loop over species -- end
} ## Loop over regions -- end

saveRDS(object = df_conv, file = "output/df_conv.RDS")
write.csv(x = df_conv, file = "output/df_conv.csv", row.names = F)
