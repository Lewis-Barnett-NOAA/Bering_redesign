library(devtools)

if (!('pacman' %in% installed.packages())) {
  install.packages("pacman")}

#Using R v4.5.x

devtools::install_github("James-Thorson-NOAA/VAST@4.0.0")
devtools::install_github("afsc-gap-products/gapindex")

c("lubridate", "gapindex", 'ncdf4', 'terra', "FNN", "spines", "sp")


## Create Folder
if (!dir.exists(paths = "output/")) dir.create(path = 'output/')
if (!dir.exists(paths = "figures/")) dir.create(path = 'figures/')
if (!dir.exists(paths = "data/data_processed/")) 
  dir.create('data/data_processed/')
if (!dir.exists(paths = "data/data_processed/species/")) 
  dir.create('data/data_processed/species/')

## Check for raw data sources
file.exists("data/data_raw/gebco_2022_n70.0_s50.0_w-180.0_e-155.0.asc")
dir.exists("data/data_raw/bering_10k_roms/netcdf_forecast/")
dir.exists("data/data_raw/bering_10k_roms/netcdf_historical/")


