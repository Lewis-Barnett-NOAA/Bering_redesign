##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Bering sea Operating Spatiotemporal Distribution Models
##  Daniel Vilas (danielvilasgonzalez@gmail.com)
##  Lewis Barnett, Stan Kotwicki, Zack Oyafuso, Megsie Siple, Leah Zacher, 
##  Lukas DeFilippo, Andre Punt
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# clear all objects
rm(list = ls())

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Import libraries
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(sdmTMB)
library(googledrive)
library(coldpool)
library(gargle)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Authorize Google Drive (need gmail account)
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Connect to google drive using your (probably NOAA) gmail account
gdrive_email <- rstudioapi::showPrompt(title = "Email",
                                       message = "Email for Google Drive",
                                       default = "")

googledrive::drive_auth(token = gargle::credentials_user_oauth2(
  scopes = "https://www.googleapis.com/auth/drive", 
  email = gdrive_email))

googledrive::drive_user()  # check user account

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Download Data from Google Drive
## Currently the links to the data are hard-coded, need to increase 
## reproducibility of this process
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Download species_list
googledrive::drive_download(
  file = googledrive::as_id("1aaTSU4rBS_VIGa-HXTWG5Tx7w1mOCwSX"), 
  path = "data/species_list.csv", 
  overwrite = TRUE
)
species_list <- read.csv(file = "data/species_list.csv") |>
  subset(subset = SPECIES_CODE %in% unique(x = sort(x = GROUP_CODE)))

## Download cpue df
googledrive::drive_download(
  file = googledrive::as_id("1cOaOwC2nTRHw2bUFLECN2DJE3gRoyuLs"), 
  path = "data/data_processed/cpue_bs_allspp.RDS", 
  overwrite = TRUE
)
cpue_bs_allspp <- readRDS(file = "data/data_processed/cpue_bs_allspp.RDS")

## Download interpolation grid
googledrive::drive_download(
  file = googledrive::as_id("1AqfgsXKWEiQq75whxUXR3B4Fkp402gsD"), 
  path = "data/data_processed/grid_bs_year.RDS", 
  overwrite = TRUE
)
grid_bs_year <- readRDS(file = "data/data_processed/grid_bs_year.RDS")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Fit Models - four types of covariate settings:
## "cp" = formula(x = cpue_kgkm2 ~ 0 + f_year),
## "cp_d" = formula(x = cpue_kgkm2 ~ 0 + f_year + s(scaled_depth, k = 3)),
## "cp_sbt" = formula(x = cpue_kgkm2 ~ 0 + f_year + s(roms_sbt_c, k = 3)),
## "cp_d_sbt" = formula(x = cpue_kgkm2 ~ 0 + f_year + s(scaled_depth, k = 3) + s(roms_sbt_c, k = 3))
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

for (species_name in species_list$SCIENTIFIC_NAME[c(4)]) { ## Loop over species -- start
  
  output_dir <- paste0("output/om/bs_all/", species_name, "/")
  if (!dir.exists(paths = output_dir))
    dir.create(path = output_dir, recursive = TRUE)
  
  # Create new directory in google drive
  # new_folder <- drive_mkdir(
  #   name = species_name,
  #   path = "https://drive.google.com/drive/folders/1nxcqG-hPjOyg6grfxL-bzio1ZKIBJ22T", 
  #   overwrite = TRUE
  # )
  
  ## Filter cpue to species and region, create a factor for year
  cpue_data <- subset(x = cpue_bs_allspp, 
                      subset = scientific_name == species_name) 
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##  sdmTMB settings
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  ## Spatial mesh
  mesh <- cpue_data |> sdmTMB::make_mesh(xy_cols = c("x_utm_km", "y_utm_km"), 
                                         n_knots = 300)
  
  ## Cold pool as a spatially varying coefficient
  cp_df <- coldpool::cold_pool_index
  cp_index <- cbind(cp_df,
                    env = scale(coldpool::cold_pool_index$AREA_LTE2_KM2)) |>
    transform(year = as.integer(x = YEAR)) |>
    subset(select = c(year, env))
  
  mean_logdepth <- mean(x = log(x = cpue_data$depth_m))
  sd_logdepth <- sd(x = log(x = cpue_data$depth_m))
  cpue_data$scaled_depth <- 
    (log(x = cpue_data$depth_m) - mean_logdepth) / sd_logdepth
  
  cpue_data <- merge(x = cpue_data, y = cp_index, by = "year")
  
  for (model_type in c("cp", "cp_d", "cp_sbt", "cp_d_sbt")[1]) { ## Loop over model types -- start
    
    ## Specify model formula for covariates (all have cold pool as a spatially 
    ## varying coefficient)
    model_formula <- list(
      "cp" = formula(x = cpue_kgkm2 ~ 0 + f_year),
      "cp_d" = formula(x = cpue_kgkm2 ~ 0 + f_year + s(scaled_depth, k = 3)),
      "cp_sbt" = formula(x = cpue_kgkm2 ~ 0 + f_year + s(roms_sbt_c, k = 3)),
      "cp_d_sbt" = formula(x = cpue_kgkm2 ~ 0 + f_year + s(scaled_depth, k = 3) + s(roms_sbt_c, k = 3))
    )[[model_type]]
    
    ## Compile sdmtmb settings
    sdm_settings <- list(
      data = cpue_data,
      spatial_varying = ~ env,
      formula = model_formula,
      mesh = mesh,
      family = delta_gamma(type = "poisson-link"),
      time = "year",
      spatial = "on",
      spatiotemporal = "ar1",
      anisotropy = TRUE,
      silent = FALSE
    )
    
    ##  Fit model, save
    start_time <- Sys.time()
    fit <- do.call(what = sdmTMB, args = sdm_settings)
    saveRDS(object = fit, 
            file = paste0(output_dir, "fit_", model_type, ".RDS"))
    duration <- Sys.time() - start_time
    cat("Finished with", species_name, model_type, ". Time elapse:", 
        duration, attributes(duration)$units, "\n")
    
    ## Predict over interpolation grid
    # p <- predict(object = fit,
    #              newdata = grid_bs_year |>
    #                transform(scaled_depth = (log(x = depth_m) - mean_logdepth) / 
    #                            sd_logdepth),
    #              return_tmb_object = TRUE)
    # saveRDS(object = p, 
    #         file = paste0(output_dir, "preds_", model_type, ".RDS"))
    
    ## Simulate densities for dharma residuals and future survey simulations
    sim_res <- simulate(fit, nsim = 500, type = "mle-mvn", model = NA)
    saveRDS(object = sim_res, 
            file = paste0(output_dir, "sim_res_", model_type, ".RDS"))
    
    ## Create dharma residual diagnostics
    dharma_resids <- sdmTMB::dharma_residuals(simulated_response = sim_res, 
                                              object = initial_fit, 
                                              return_DHARMa = TRUE)
    saveRDS(object = dharma_resids, 
            file = paste0(output_dir, "dharma_resids_", model_type, ".RDS"))
    
    jpeg(filename = paste0("output/om/bs_all/", species_name, 
                           "/dharma_resids_", model_type, ".jpeg"), 
         width = 6, height = 5, units = "in", res = 500) 
    plot(dharma_resids)
    dev.off()
    
    ## Loop over files and upload to Google Drive
    for (ifile in c(paste0(c("fit_", "sim_res_", "dharma_res_"), 
                           model_type, ".RDS"),
                    paste0("dharma_resids_", model_type, ".jpeg"))) {
      googledrive::drive_upload(
        media = paste0(output_dir, ifile),
        path = paste0("Bering_redesign/om/bs_all/", species_name, "/"),
        name = ifile
      )
    }
    
  }  ## Loop over model types -- end
} ## Loop over species -- end

