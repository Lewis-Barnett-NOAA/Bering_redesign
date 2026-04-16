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
##  Fit Models
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

for (idx in which(species_list$FOOTPRINT == "bs_slope")[]) { ## Loop over species -- start
  # for (idx in 1:nrow(x = species_list) ) { ## Loop over species -- start  
  ## Extract species and survey footprint to fit model
  species_name <- species_list$SCIENTIFIC_NAME[idx]
  footprint <- species_list$FOOTPRINT[idx]
  footprint_regions <- 
    list("bs_slope" = c("BSS"),
         "bs_shelf" = c("EBS", "NBS"), 
         "bs_all" = c("EBS", "NBS", "BSS"))[[footprint]]
  googlelink_dir <- 
    list("bs_slope" = "1M7g8DW55uTScLHwApjYOChNAInCid5zU",
         "bs_shelf" = "15b3B0ZLros2HW-ctc1BjAVHX1in4yy4J", 
         "bs_all" = "1nxcqG-hPjOyg6grfxL-bzio1ZKIBJ22T")[[footprint]]
  
  output_dir <- paste0("output/om/", footprint, "/", species_name, "/")
  if (!dir.exists(paths = output_dir))
    dir.create(path = output_dir, recursive = TRUE)
  
  # Create new directory in google drive
  new_folder <- drive_mkdir(
    name = species_name,
    path = paste0("https://drive.google.com/drive/folders/", googlelink_dir),
    overwrite = TRUE
  )
  
  googlelink_dir_species <- 
    drive_ls(as_id(googlelink_dir), type = "folder") |>
    subset(name == species_name)
  
  ## Filter cpue to species and region, create a factor for year
  cpue_data <- subset(x = cpue_bs_allspp, 
                      subset = scientific_name == species_name & 
                        survey %in% footprint_regions) 
  
  ## Spatial mesh
  mesh <- sdmTMB::make_mesh(data = cpue_data,
                            xy_cols = c("x_utm_km", "y_utm_km"), 
                            n_knots = c("bs_slope" = 200, 
                                        "bs_shelf" = 300, 
                                        "bs_all" = 300)[[footprint]])
  
  ## Cold pool as a spatially varying coefficient
  cp_df <- coldpool::cold_pool_index
  cp_index <- cbind(cp_df,
                    env = scale(coldpool::cold_pool_index$AREA_LTE2_KM2)) |>
    transform(year = as.integer(x = YEAR)) |>
    subset(select = c(year, env))
  cpue_data <- merge(x = cpue_data, y = cp_index, by = "year")
  
  ## Log depth and scale 
  mean_logdepth <- mean(x = log(x = cpue_data$depth_m))
  sd_logdepth <- sd(x = log(x = cpue_data$depth_m))
  cpue_data$scaled_depth <- 
    (log(x = cpue_data$depth_m) - mean_logdepth) / sd_logdepth
  
  ## Scale roms-sbt
  mean_roms_sbt <- mean(x = cpue_data$roms_sbt_c)
  sd_roms_sbt <- sd(x = cpue_data$roms_sbt_c)
  cpue_data$scaled_roms_sbt <- 
    (cpue_data$roms_sbt_c - mean_roms_sbt) / sd_roms_sbt
  
  ## Subset covariate scenarios
  covariate_scenarios <- list("bs_slope" = c("base", "d", "sbt", "d_sbt"), 
                              "bs_shelf" = c("base", "d_sbt"), 
                              "bs_all" = c("base", "d_sbt"))[[footprint]]
  cold_pool <- switch(footprint,
                      bs_all = formula("~ env"), 
                      bs_shelf = formula("~ env"), 
                      bs_slope = NULL)
  st_field <- species_list[idx, c("ST1", "ST2")] |> do.call(what = list)
  extra_time <- list("bs_slope" = NULL, 
                     "bs_shelf" = 2020L, 
                     "bs_all" = 2020L)[[footprint]]
  
  ## Loop over covariate scenarios
  for (model_type in covariate_scenarios) { ## Loop over model types -- start
    
    ## Specify model formula for covariates (all have cold pool as a spatially 
    ## varying coefficient)
    model_formula <- list(
      "base" = formula(x = cpue_kgkm2 ~ 0 + f_year),
      "d" = formula(x = cpue_kgkm2 ~ 0 +
                      f_year + stats::poly(scaled_depth, degree = 2)),
      "sbt" = formula(x = cpue_kgkm2 ~ 0 +
                        f_year +
                        stats::poly(scaled_roms_sbt, degree = 2)),
      "d_sbt" = formula(x = cpue_kgkm2 ~ 0 +
                          f_year + stats::poly(scaled_depth, degree = 2) +
                          stats::poly(scaled_roms_sbt, degree = 2))
    )[[model_type]]
    
    ## Compile sdmtmb settings
    sdm_settings <- list(
      data = cpue_data,
      spatial_varying = cold_pool,
      formula = model_formula,
      mesh = mesh,
      family = delta_gamma(type = "poisson-link"),
      time = "year",
      spatial = "on",
      spatiotemporal = st_field,
      extra_time = extra_time,
      anisotropy = TRUE,
      control = sdmTMBcontrol(profile = TRUE),
      silent = FALSE      
    )
    
    ##  Fit model, save
    fit <- do.call(what = sdmTMB, args = sdm_settings)
    saveRDS(object = fit, 
            file = paste0(output_dir, "fit_", 
                          ifelse(test = !is.null(x = cold_pool), 
                                 yes = "cp_", no = ""),
                          model_type, ".RDS"))
    
    ## Save initial output and diagnostic
    saveRDS(object = sdmTMB::sanity(fit), 
            file = paste0(output_dir, "sanity_", 
                          ifelse(test = !is.null(x = cold_pool), 
                                 yes = "cp_", 
                                 no = ""),
                          model_type, ".RDS"))
    
    sink(
      paste0(output_dir, "summary_", 
             ifelse(test = !is.null(x = cold_pool), yes = "cp_", no = ""),
             model_type, ".txt")
    )
    print(summary(fit))
    sink()
    
    ## Save cAIC
    saveRDS(object = sdmTMB::cAIC(object = fit),
            file = paste0(output_dir, "cAIC_", 
                          ifelse(test = !is.null(x = cold_pool), 
                                 yes = "cp_", no = ""),
                          model_type, ".RDS"))
    
    
    ## Simulate densities for dharma residuals and future survey simulations
    sim_res <- simulate(fit, nsim = 500, type = "mle-mvn", seed = 32, model = NA)
    saveRDS(object = sim_res, 
            file = paste0(output_dir, "sim_res_", 
                          ifelse(test = !is.null(x = cold_pool), 
                                 yes = "cp_", no = ""),
                          model_type, ".RDS"))
    
    ## Create dharma residual diagnostics
    dharma_resids <- sdmTMB::dharma_residuals(simulated_response = sim_res, 
                                              object = fit, 
                                              return_DHARMa = TRUE)
    saveRDS(object = dharma_resids, 
            file = paste0(output_dir, "dharma_res_", 
                          ifelse(test = !is.null(x = cold_pool), 
                                 yes = "cp_", no = ""),
                          model_type, ".RDS"))
    
    jpeg(filename = paste0(output_dir, "dharma_res_", 
                           ifelse(test = !is.null(x = cold_pool), 
                                  yes = "cp_", no = ""),
                           model_type, ".jpeg"),
         width = 6, height = 5, units = "in", res = 500) 
    plot(dharma_resids)
    dev.off()
    
    ## Loop over files and upload to Google Drive
    for (ifile in c(paste0(c("fit_", "sim_res_", "dharma_res_", "cAIC_"), 
                           ifelse(test = !is.null(x = cold_pool), 
                                  yes = "cp_", no = ""),
                           model_type, ".RDS"),
                    paste0("summary_",
                           ifelse(test = !is.null(x = cold_pool), 
                                  yes = "cp_", no = ""),
                           model_type, ".txt"),
                    paste0("sanity_",
                           ifelse(test = !is.null(x = cold_pool), 
                                  yes = "cp_", no = ""),
                           model_type, ".RDS"),
                    paste0("dharma_res_", 
                           ifelse(test = !is.null(x = cold_pool), 
                                  yes = "cp_", no = ""),
                           model_type, ".jpeg") ) ) {
      googledrive::drive_upload(
        media = paste0(output_dir, ifile),
        path = googlelink_dir_species$id,
        name = ifile
      )
    }
  }  ## Loop over model types -- end 
} ## Loop over species -- end

