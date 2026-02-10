################################################################################
################################################################################
##
## Simulate data from OM for historical years for shelf
## Reshape output to combine simulated densities for shelf and slope
## Daniel Vilas (danielvilasgonzalez@gmail.com)
## Lewis Barnett, Zack Oyafuso, Megsie Siple
##
################################################################################
################################################################################

# clear all objects
rm(list = ls(all.names = TRUE))

# free up memory and report usage
gc()

# libraries
pack_cran <- c("ggplot2", "units", "splines", "raster", "sp", "foreach",
               "doParallel")

if (!("pacman" %in% installed.packages())) {
  install.packages("pacman")
}

if (!("VAST" %in% installed.packages())) {
  devtools::install_github("james-thorson/VAST@main",
                           INSTALL_opts = "--no-staged-install")
}
library(VAST)

pacman::p_load(pack_cran, character.only = TRUE)

# setwd
# out_dir <- "/Users/daniel/Work/UW-NOAA/Adapting Monitoring to a Changing Seascape/"
# setwd(out_dir)

# species list
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

# read convergence
df_conv <- read.csv("output/tables/slope_conv.csv")
slp_conv <- df_conv[which(df_conv$slope_st == "convergence"), "spp"]
ebsnbs_conv <- df_conv[which(df_conv$EBS_NBS == "convergence"), "spp"]

dir.create(paste0("output/slope/species/"), recursive = TRUE)

###################################
# Grid Configuration
###################################

bering_sea_slope_grid <- FishStatsUtils::bering_sea_slope_grid
grid <- as.data.frame(rbind(data.frame(bering_sea_slope_grid, region = "SLP")))
grid$cell <- 1:nrow(grid)

load("data/data_processed/grid_EBS_NBS.RData")
yrs <- 1982:2022
grid_ebs <- grid.ebs_year[which(grid.ebs_year$region != "EBSslope" &
                                  grid.ebs_year$Year %in% yrs), ]

n_sim_hist <- 100
dens_index_hist_OM <- list()

sim_hist_dens_spp <- array(
  NA,
  dim = c(nrow(grid), length(unique(yrs)), n_sim_hist, length(spp)),
  dimnames = list(1:nrow(grid), unique(yrs), 1:n_sim_hist, spp)
)

######################
# Simulate Historical Data
######################

northern_bering_sea_grid <- FishStatsUtils::northern_bering_sea_grid
eastern_bering_sea_grid <- FishStatsUtils::eastern_bering_sea_grid
grid <- as.data.frame(rbind(
  data.frame(northern_bering_sea_grid, region = "NBS"),
  data.frame(eastern_bering_sea_grid, region = "EBS")
))
grid$cell <- 1:nrow(grid)

for (sp in spp) {
  
  cat(paste0("############### ", sp, " #########################\n"))
  mod1 <- "fit.RData"
  dir.create(paste0("output/species/", sp, "/"), recursive = TRUE)
  
  ff <- list.files(paste0("output/vast/", sp), mod1, recursive = TRUE)
  if (length(ff) == 0) next
  
  load(paste0("output/vast/", sp, "/", ff))
  fit <- reload_model(x = fit)
  
  index <- fit$Report$Index_ctl
  dens <- fit$Report$D_gct
  dens_index_hist_OM[[sp]] <- list("index" = index, "dens" = dens)
  
  data_geostat1 <- readRDS(paste0("output/vast/", sp, "/data_geostat_temp.rds"))
  data_geostat <- data_geostat1[which(data_geostat1$Region %in%
                                        c("Eastern Bering Sea Crab/Groundfish Bottom Trawl Survey",
                                          "Northern Bering Sea Crab/Groundfish Survey - EBS Shelf Extension")), ]
  
  pred_TF <- rep(1, nrow(data_geostat1))
  pred_TF[1:nrow(data_geostat)] <- 0
  
  sim_dens <- array(
    NA,
    dim = c(nrow(grid), length(unique(yrs)), n_sim_hist),
    dimnames = list(1:nrow(grid), unique(yrs), 1:n_sim_hist)
  )
  
  dir.create(paste0("output/species/", sp, "/simulated_historical_data/"),
             recursive = TRUE)
  
  for (isim in 1:n_sim_hist) {
    cat(paste(" ############# Species", sp, "Simulation", isim, "\n"))
    
    Sim1 <- FishStatsUtils::simulate_data(fit = fit, type = 1,
                                          random_seed = isim)
    
    sim_bio <- matrix(data = Sim1$b_i[pred_TF == 1],
                      nrow = nrow(grid),
                      ncol = length(yrs))
    
    sim_dens[, , isim] <- sim_bio / grid$Area_in_survey_km2
  }
  
  save(sim_dens,
       file = paste0("output/species/", sp,
                     "/simulated_historical_data/sim_dens.RData"))
  sim_hist_dens_spp[, , , sp] <- sim_dens
}

######################
# Parallel Reshape
######################

cl <- makeCluster(1)
registerDoParallel(cl)
n_sim <- 100

sim_dens1 <- array(
  NA,
  dim = c(nrow(grid), length(spp), length(unique(yrs)), n_sim),
  dimnames = list(1:nrow(grid), spp, unique(yrs), 1:n_sim)
)

foreach(sp = ebsnbs_conv) %do% {
  load(paste0("output/species/", sp, "/simulated_historical_data/sim_dens.RData"))
  foreach(y = yrs) %:%
    foreach(sim = 1:n_sim) %do% {
      sim_dens1[, sp, as.character(y), as.character(sim)] <-
        sim_dens[, as.character(y), as.character(sim)]
    }
}

stopCluster(cl)
save(sim_dens1, file = paste0("output/slope/species/ms_sim_dens.RData"))

################################################################################
# JOIN EBS+NBS and SLP ARRAY
################################################################################

load(file = paste0("output/slope/species/ms_sim_dens.RData"))
ebsnbs_simdens <- sim_dens1

load(file = paste0("output/slope/species/ms_sim_dens_slope.RData"))
slp_simdens <- sim_dens1

ebsnbs_simdens[is.na(ebsnbs_simdens)] <- 0
slp_simdens[is.na(slp_simdens)] <- 0

# Final Combined Array (56505 cells)
sim_dens1 <- array(
  NA,
  dim = c(56505, length(spp), length(1982:2022), n_sim),
  dimnames = list(1:56505, spp, 1982:2022, 1:n_sim)
)

for (sp in spp) {
  for (y in 1982:2022) {
    cat(paste(sp, y, "\n"))
    for (sim in 1:n_sim) {
      if (y %in% 2002:2016) {
        sim_dens1[, sp, as.character(y), as.character(sim)] <- c(
          ebsnbs_simdens[, sp, as.character(y), as.character(sim)],
          slp_simdens[, sp, as.character(y), as.character(sim)]
        )
      } else {
        sim_dens1[, sp, as.character(y), as.character(sim)] <- c(
          ebsnbs_simdens[, sp, as.character(y), as.character(sim)],
          rep(NA, 3041)
        )
      }
    }
  }
}

save(sim_dens1, file = paste0("output/slope/species/ms_sim_dens_all.RData"))
write.csv(sim_dens1, file = "data/data_processed/ms_sim_dens_all.csv",
          row.names = FALSE)