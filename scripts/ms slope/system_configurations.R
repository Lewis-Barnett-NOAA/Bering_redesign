library(devtools)

if (!('pacman' %in% installed.packages())) {
  install.packages("pacman")}

#Using R v4.5.x

devtools::install_github("James-Thorson-NOAA/VAST@4.0.0")
devtools::install_github("afsc-gap-products/gapindex")
devtools::install_github("afsc-gap-products/coldpool")

install.packages("sdmTMB", dependencies = TRUE)
install.packages("lubridate", dependencies = TRUE)
install.packages("ncdf4", dependencies = TRUE)
install.packages("terra", dependencies = TRUE)
install.packages("sf", dependencies = TRUE)

## Create Folder
if (!dir.exists(paths = "output/")) dir.create(path = 'output/')
if (!dir.exists(paths = "figures/")) dir.create(path = 'figures/')
if (!dir.exists(paths = "data/data_processed/")) 
  dir.create('data/data_processed/')
if (!dir.exists(paths = "data/data_processed/species/")) 
  dir.create('data/data_processed/species/')

## Check for raw data sources
dir.exists("data/data_raw/bering_10k_roms/netcdf_forecast/")
dir.exists("data/data_raw/bering_10k_roms/netcdf_historical/")


