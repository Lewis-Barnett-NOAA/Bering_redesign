####################################################################
####################################################################
##
##    Sloper Operating Spatiotemporal Distribution Models
##    Daniel Vilas (danielvilasgonzalez@gmail.com)
##    Lewis Barnett, Stan Kotwicki, Zack Oyafuso, Megsie Siple, Leah Zacher, 
##    Lukas DeFilippo, Andre Punt
##
####################################################################
####################################################################

# clear all objects
rm(list = ls())

# set seed
set.seed(6)

# libraries from cran to call or install/load
pack_cran <- c("splines", "ggplot2", "dplyr", "doParallel", "VAST")

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

# folder region - only slope
fol_region <- "output/slope/vast"
if (!dir.exists(paths = fol_region)) 
  dir.create(path = fol_region, recursive = TRUE)

## Load grid
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

# number of simulations
n_sim <- 100

# fit with depth or CPE
df_conv <- data.frame(spp = species_list$SCIENTIFIC_NAME,
                      max_grad = NA,
                      convergence_message = NA)

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
                      weight_kg, area_swept_km2, depth_m, survey, cpue_kgkm2 ))
  names(x = data_geostat) <- c("Lat", "Lon", "Year", "Species", "Weight_kg",
                               "Effort", "Depth", "Region", "CPUEkgkm")
  
  ## Scale depth
  
  # grid with info to get prediction
  data_geostat1 <- rbind(
    data_geostat[, c("Lat", "Lon", "Year", "Species", "Weight_kg", "Effort",
                     "Depth", "Region", "CPUEkgkm")],
    data.frame(Lat = grid_bss$Lat,
               Lon = grid_bss$Lon,
               Year = grid_bss$Year,
               Species = sp,
               Weight_kg = mean(data_geostat$Weight_kg),
               Effort = grid_bss$Area_in_survey_km2,
               Depth = grid_bss$depth_m,
               Region = grid_bss$region,
               CPUEkgkm = mean(data_geostat$CPUEkgkm),
               stringsAsFactors = TRUE)
  )
  
  ## Scale depth by the mean and sd of the observed depths 
  mean_logdepth <- mean(x = log(x = data_geostat$Depth))
  sd_logdepth <- sd(x = log(x = data_geostat$Depth))
  
  data_geostat1$Depth <-
    (log(data_geostat1$Depth) - mean_logdepth) / sd_logdepth
  
  ## Some extra settings to help with convergence issues
  ## Anisotropy
  aniso <- ifelse(test = sp %in% c("Anoplopoma fimbria", "Atheresthes stomias"),
                  yes = FALSE, 
                  no = TRUE)
  
  ## FIeld Configurations
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
  
  ## Newton Steps for convergence issues
  steps <- ifelse(test = sp %in% c("Hippoglossoides elassodon",
                                   "Sebastes melanostictus",
                                   "Sebastolobus alascanus"), 
                  yes = 5, 
                  no = 1)
  
  ## Set up VAST model settings
  settings <- make_settings(n_x = 300,
                            Region = "bering_sea_slope",
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
  
  ## Setup covariate data
  covariate_data <- data_geostat1[complete.cases(data_geostat1[, "Depth"]), ]
  covariate_data$Year <- NA
  formula <- " ~ bs(Depth, degree=2, intercept=FALSE)"
  X1config_cp <- array(c(1, 1), dim = c(1, 1))
  X2config_cp <- X1config_cp
  
  ## Specify which records in the input dataframe are used for estimation
  ## (pred_TF == 0) and which are used just for prediction (pred_TF == 1)
  pred_TF <- rep(1, nrow(x = data_geostat1))
  pred_TF[1:nrow(x = data_geostat)] <- 0
  
  # fit
  fit <- tryCatch({
    fit_model(settings = settings,
              Lat_i = data_geostat1$Lat,
              Lon_i = data_geostat1$Lon,
              t_i = data_geostat1$Year,
              b_i = data_geostat1$Weight_kg,
              c_iz = as.numeric(x = factor(x = data_geostat1$Species)) - 1,
              a_i = data_geostat1$Effort,
              input_grid = bering_sea_slope_grid,
              getJointPrecision = TRUE,
              test_fit = FALSE,
              covariate_data = covariate_data,
              X1_formula = formula,
              X2_formula = formula,
              newtonsteps = steps,
              PredTF_i = pred_TF,
              working_dir = paste(fol_region, "/", sp, "/"))
  }, error = function(cond) {
    message("Did not converge.")
    return(NULL)
  })
  
  ## Convergence messaging
  df_conv$convergence_message[which(df_conv$spp == sp)] <-
    fit$parameter_estimates$Convergence_check
  df_conv$max_grad[which(df_conv$spp == sp)] <-
    fit$parameter_estimates$max_gradient
  
  # simulate historical data
  n_interpolation_cells <- unique(x = table(grid_bss$Year))
  temp_sim_data <- array(NA,
                         dim = c(n_interpolation_cells,
                                 length(x = unique(data_geostat1$Year)), 
                                 n_sim),
                         dimnames = list(NULL,
                                         sort(x = unique(data_geostat1$Year)),
                                         NULL)  )
  
  if (inherits(fit, "fit_model")) {
    save(list = "fit",
         file = paste(out_dir, fol_region, sp, "fit.RData", sep = "/"))
    
    ## Create 
    for (isim in 1:n_sim) {
      cat(paste(" ############# Simulation", isim, "of", n_sim, "\n"))
      Sim1 <- FishStatsUtils::simulate_data(fit = fit, 
                                            type = 1,
                                            random_seed = isim)
      
      
      temp_sim_data[, , isim] <- 
        matrix(data = Sim1$b_i[pred_TF == 1] / grid_bss$Area_in_survey_km2,
               nrow = n_interpolation_cells,
               ncol = length(x = unique(x = data_geostat1$Year)))  
      
    }
  }
  
  if (!dir.exists(paths = paste0("output/species/", sp, 
                                 "/simulated_historical_data/")))
    dir.create(path = paste0("output/species/", sp, 
                             "/simulated_historical_data/"),
               recursive = TRUE)
  save(temp_sim_data,
       file = paste0("output/species/", sp,
                     "/simulated_historical_data/sim_dens_slope.RData"))
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Collate simulated data into one large array
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)

sim_dens <- array(data = NA,
                  dim = c(n_interpolation_cells, 
                          length(x = species_list$SCIENTIFIC_NAME),
                          length(x = 2002:2016), 
                          n_sim),
                  dimnames = list(NULL, 
                                  species_list$SCIENTIFIC_NAME,
                                  2002:2016, 
                                  NULL))

foreach(sp = species_list$SCIENTIFIC_NAME) %do% {
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
